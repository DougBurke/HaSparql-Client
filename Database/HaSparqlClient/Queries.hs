{-# LANGUAGE OverloadedStrings #-}

{- | This module provides functions to access remote endpoints.
     'runSelectQuery' and 'runAskQuery' may not work if you try to override the output format. See also about 'HGET' and 'HPOST'.
-}

module Database.HaSparqlClient.Queries 
       (
         runQuery
       , runSelectQuery
       , runAskQuery
       ) where

-- import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as B8

import Control.Exception as CE

import Network.URI
import Network.HTTP.Conduit
import qualified Network.HTTP.Types as NT
import Network.HTTP (urlEncodeVars)

import Text.XML.Light
import Text.XML.Light.Lexer (XmlSource)
import Data.Maybe
import Control.Monad (guard, liftM)
import Data.Char (toLower)

import Database.HaSparqlClient.Types
import Paths_hasparql_client (version)
import Data.Version (showVersion)

{- | Execute a service.

On success returns a string created from the service.
By default, the string is a representation in XML, other formats such as Turtle and N3 
could be returned by adding the output format from the list of optional parameters.

Returns an error message on failure. SPARUL and SPARQL can be performed.
-}
runQuery :: Service -> Method -> IO (Either String String)
runQuery = getSparqlRequest (right . L8.unpack) . constructURI 

-- | Find all possible values for a query of type @SELECT@, returning the bound variables.
--   If it fails returns an error message.
runSelectQuery :: Service ->  Method -> IO (Either String [[BindingValue]])
runSelectQuery =  getSparqlRequest (parse $ parseSparqlVariables >>= parseSparqlResults) . constructURI

-- @
--  select = do
--     res <- runSelectQuery defaultService HPOST
--     case res of
--       Left e -> print $ "Error:" ++ e
--       Right s -> print s
-- @

-- | Return Right True or Right False for a query of type @ASK@. 
--   If it fails returns an error message.
runAskQuery :: Service -> Method -> IO (Either String Bool)
runAskQuery serv m= do 
  b <- getSparqlRequest (parse parseSparqlBooleanResult) (constructURI serv) m
  case b of
    Right x -> case x of
      Just True -> return $ Right True
      Just False -> return $ Right False
      _ -> return $ Left "Boolean binding not found."
    Left x -> return $ Left x

-- @
--  ask = do
--     let s = Sparql "http://dbpedia.org/sparql/" query Nothing [] []
--     res <- runAskQuery s HGET
--       where
--         query = "PREFIX prop: <http://dbpedia.org/property/>" ++
--          " ASK { <http://dbpedia.org/resource/Amazon_River> prop:length ?amazon ." ++
--          " <http://dbpedia.org/resource/Nile> prop:length ?nile ." ++
--          "FILTER(?amazon > ?nile) .} "
--     case res of
--       Left e -> print $ "Error:" ++ e
--       Right s -> print s
-- @

-- In case of success makes the request and transforms the result depending on the callback function.

getSparqlRequest :: 
  (L8.ByteString -> Either String b) 
  -> Either String (URI, [ExtraParameters]) 
  -> Method 
  -> IO (Either String b)
getSparqlRequest f u m = 
  case u of
    Left err -> return $ Left err
    Right uri -> do
      resp <- getRespBody uri m
      case resp of
        Left err -> return $ Left (show err)
        Right rsp -> return $ f rsp
                                                                  

{-
getSparqlRequest :: 
  (String -> Either String b) 
  -> Either String (URI, [ExtraParameters]) 
  -> Method 
  -> IO (Either String b)
getSparqlRequest f u m = 
  case u of
    Left err -> return $ Left err
    Right uri -> do
      resp <- getRespBody uri m
      case resp of
        Left err -> return $ Left (show err)
        Right rsp -> case rspCode rsp of
          (2,_,_) -> return $ f $ rspBody rsp
          _       -> return $ Left $ rspBody rsp
                                                                  
-}

-- This function looks if the Endpoint is a valid URI, then returns the URI and other parameters are fixed and added.                                            
constructURI :: Service -> Either String (URI, [ExtraParameters])                                            
constructURI (Sparql epoint qry defg ng oth) = 
  case parseURI epoint of
    Nothing -> Left "Bad string for endpoint."
    Just uri -> Right (uri, [("query",qry)] ++ dgraph ++ othervars ng ++ filtparams oth)
    where
      othervars lst = [("named-graph-uri", x) | x<-lst]
      dgraph = case defg of
        Nothing -> []
        Just g -> [("default-graph-uri", g)]
      filtparams = filter bool
      bool (a,_)
        | lower a /= "named-graph-uri" && lower a /= "default-graph-uri" = True
        | otherwise = False
      lower = map toLower
         
quri :: Maybe String
quri = Just "http://www.w3.org/2005/sparql-results#"

-- Find the names for all sparql variables in the XML document.
parseSparqlVariables :: Element -> [String]    
parseSparqlVariables doc = 
  mapMaybe attr (findElements (QName "variable" quri Nothing) doc)
    where
      attr = findAttr (QName "name" Nothing Nothing)

-- Transform the XmlElement recivied from HTTP request with variable's name in lists.
parseSparqlResults :: [String] -> Element -> [[BindingValue]]
parseSparqlResults vars = map (parseSparqlBindings vars) . findElements (QName "result" quri Nothing)

     
parseSparqlBindings :: [String] -> Element -> [BindingValue]
parseSparqlBindings vars doc = map pVar vars
  where
   pVar v  = maybe Unbound (elementBinding . head . elChildren) $ filterElement (predV v) doc
   predV v e = isJust $ do 
     a <- findAttr (unqual "name") e
     guard $ a == v
                        
-- Parse the XML document and returns the Boolean value as Maybe Bool 
parseSparqlBooleanResult  :: Element -> Maybe Bool
parseSparqlBooleanResult doc =
  case findElement (QName "boolean" quri Nothing) doc of
    Just e -> case strContent e of
      "true" -> Just True
      "false" -> Just False
      _ -> Nothing
    _ -> Nothing

-- Transform an XML element in a BindingValue.                                                          
elementBinding :: Element -> BindingValue
elementBinding e = case qName (elName e) of
                "uri" -> Database.HaSparqlClient.Types.URI (strContent e)
                "literal" -> case findAttr (unqual "datatype") e of
                                    Just dt -> TypedLiteral (strContent e) dt
                                    Nothing -> case findAttr langAttr e of
                                        Just lang -> LangLiteral (strContent e) lang
                                        Nothing  -> Literal (strContent e)
                "bnode" -> BNode (strContent e)
                _ -> Unbound
  where
    langAttr = blank_name {qName = "lang", qPrefix = Just "xml"}

getRespBody ::
  (URI, [ExtraParameters])
  -> Method
  -> IO (Either IOError L8.ByteString)
getRespBody u m = CE.catch (Right `liftM` makeCall u m) (return . Left)

makeCall ::
  (URI, [ExtraParameters])
  -> Method
  -> IO L8.ByteString
makeCall (uri, params) m = do
  u <- parseUrl $ show uri
  let baseHdrs = [ NT.headerAccept accept
                 , ("Accept-Charset", "utf-8")
                 , ("UserAgent", B8.pack (showVersion version))]
                 ++ requestHeaders u
                 
      qs = B8.pack $ urlEncodeVars params
      
      u' = case m of
        HGET -> u { method = "GET"
                  , requestHeaders = baseHdrs
                  , queryString = qs
                  }
                
        HPOST -> u { method = "POST"
                   , requestHeaders = ("Content-Type", "application/x-www-form-urlencoded") : baseHdrs
                   , requestBody = RequestBodyBS qs
                   }
                 
  withManager $ fmap responseBody . httpLbs u'

-- Parse XML documents depending on the generic function in the argument.                 
parse :: (XmlSource s) => (Element -> a) -> s -> Either String a                 
parse f s = case parseXMLDoc s of
                Just doc -> Right (f doc)
                Nothing -> Left "Internal error parsing xml results."

right :: b -> Either a b
right = Right

-- Defaults MIME/Types for SPARQL queries. '*/*' for all other possibilities.
accept :: NT.Ascii
accept = "application/sparql-results+xml, application/rdf+xml, */*"
