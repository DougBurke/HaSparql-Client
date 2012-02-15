{-# LANGUAGE OverloadedStrings, TupleSections #-}

{- | This module provides functions to access remote endpoints.
     'runSelectQuery' and 'runAskQuery' may not work if you try to override the output format. See also about 'HGET' and 'HPOST'.
-}

{-
The SPARQL response XML format is described at

http://www.w3.org/TR/rdf-sparql-XMLres/

Note that there is no attempt at providing access to any
additional metadata about the search results provided within
the head element of the response.
-}

module Database.HaSparqlClient.Queries 
       (
         runQuery
       , runSelectQuery
       , runAskQuery
       ) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as B8

import Control.Exception as CE

import Network.URI
import Network.HTTP.Conduit
import qualified Network.HTTP.Types as NT
import Network.HTTP (urlEncodeVars)

import Text.XML
import Text.XML.Cursor

import Data.Maybe
import Data.Char (toLower)

import Control.Applicative ((<|>), (<$>))
import Control.Monad (liftM)

import qualified Database.HaSparqlClient.Types as HT
import Database.HaSparqlClient.Types hiding (URI)
import Paths_hasparql_client (version)
import Data.Version (showVersion)

{- | Execute a service.

On success returns a string created from the service.
By default, the string is a representation in XML, other formats such as Turtle and N3 
could be returned by adding the output format from the list of optional parameters.

Returns an error message on failure. SPARUL and SPARQL can be performed.
-}
runQuery :: ToSPARQL a => Service a -> Method -> IO (Either String String)
runQuery = getSparqlRequest (Right . L8.unpack) . constructURI 

-- | Find all possible values for a query of type @SELECT@, returning the bound variables.
--   If it fails returns an error message.
--
--   An example:
--
-- > select = do
-- >    res <- runSelectQuery defaultService HPOST
-- >    case res of
-- >      Left e -> print $ "Error:" ++ e
-- >      Right s -> print s

runSelectQuery :: ToSPARQL a => Service a ->  Method -> IO (Either String [[BindingValue]])
runSelectQuery =  getSparqlRequest parseResultsDocument . constructURI

-- | Return Right True or Right False for a query of type @ASK@. 
--   If it fails returns an error message.
--
--   An example:
--
-- > ask = do
-- >    let s = Sparql "http://dbpedia.org/sparql/" query Nothing [] []
-- >    res <- runAskQuery s HGET
-- >      where
-- >        query = "PREFIX prop: <http://dbpedia.org/property/>" ++
-- >         " ASK { <http://dbpedia.org/resource/Amazon_River> prop:length ?amazon ." ++
-- >         " <http://dbpedia.org/resource/Nile> prop:length ?nile ." ++
-- >         "FILTER(?amazon > ?nile) .} "
-- >    case res of
-- >      Left e -> print $ "Error:" ++ e
-- >      Right s -> print s

runAskQuery :: ToSPARQL a => Service a -> Method -> IO (Either String Bool)
runAskQuery serv = 
  getSparqlRequest parseBooleanDocument (constructURI serv)


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
                                                                  

-- This function looks if the Endpoint is a valid URI,
-- then returns the URI and other parameters are fixed and added.                                            
constructURI :: ToSPARQL a => Service a -> Either String (URI, [ExtraParameters])                                            
constructURI (Sparql epoint qry defg ng oth) = 
  case parseURI epoint of
    Nothing -> Left "Bad string for endpoint."
    Just uri -> Right (uri, ("query", toSPARQL qry) : dgraph ++ othervars ng ++ filtparams oth)
    where
      othervars lst = [("named-graph-uri", x) | x<-lst]
      dgraph = case defg of
        Nothing -> []
        Just g -> [("default-graph-uri", g)]
      filtparams = filter isBool
      isBool (a,_)
        | lower a /= "named-graph-uri" && lower a /= "default-graph-uri" = True
        | otherwise = False
      lower = map toLower
         
parseResultsDocument :: L8.ByteString -> Either String [[BindingValue]]
parseResultsDocument d = case parseLBS def d of
  Left se -> Left (show se)
  Right doc -> 
    let cursor = fromDocument doc
        names = getVarNames cursor
    in Right $ getVarResults names cursor

parseBooleanDocument :: L8.ByteString -> Either String Bool
parseBooleanDocument d = case parseLBS def d of
  Left se -> Left (show se)
  Right doc -> 
    let cursor = fromDocument doc
        txt = child cursor >>= element "{http://www.w3.org/2005/sparql-results#}boolean" >>= child >>= content
    in case txt of
      ["true"] -> Right True
      ["false"] -> Right False
      _ -> Left $ "Invalid boolean response:\n" ++ L8.unpack d

getVarNames :: Cursor -> [T.Text]
getVarNames c = 
  child c >>= element "{http://www.w3.org/2005/sparql-results#}head" >>=
  child >>= element "{http://www.w3.org/2005/sparql-results#}variable" >>=
  attribute "name"
  
-- The order of the elements within a result block are not necessarily in
-- the same order as in the head/variable section, hence the need to
-- potentially re-order the results (this also lets the code identify
-- unbound variables).
--
-- This processing feels a bit ugly; I am probably not using the API
-- properly. I plan to upgrade to the streaming API at a later date.
--
getVarResults :: [T.Text] -> Cursor -> [[BindingValue]]
getVarResults names c = 
  let res = child c >>= element "{http://www.w3.org/2005/sparql-results#}results" >>= 
            child >>= element "{http://www.w3.org/2005/sparql-results#}result" 
      bs = map parseResult res
  
  in map (getVarResult names) bs
  
getVarResult :: [T.Text] -> [(T.Text, BindingValue)] -> [BindingValue]     
getVarResult ns bs = map (fromMaybe Unbound . flip lookup bs) ns

parseResult :: Cursor -> [(T.Text, BindingValue)]
parseResult c = 
  child c >>= element "{http://www.w3.org/2005/sparql-results#}binding" >>= getBinding

getBinding :: Cursor -> [(T.Text, BindingValue)]
getBinding c = 
  let name = head $ attribute "name" c
      bv c' = getBNodeBinding c' 
              <|> getURIBinding c'
              <|> getLiteralBinding c'
  in (name,) <$> (child c >>= bv)

getBindingValue :: Name -> (T.Text -> BindingValue) -> Cursor -> [BindingValue]
getBindingValue n f c =
  let vals = element n c >>= child >>= content
  in case vals of
    [x] -> [f x]
    _ -> []
  
getBNodeBinding, getURIBinding, getLiteralBinding :: Cursor -> [BindingValue]
getBNodeBinding = getBindingValue "{http://www.w3.org/2005/sparql-results#}bnode" (BNode . T.unpack)
getURIBinding   = getBindingValue "{http://www.w3.org/2005/sparql-results#}uri"   (HT.URI . T.unpack)

getLiteralBinding c = element "{http://www.w3.org/2005/sparql-results#}literal" c >>= handleLiteral

handleLiteral :: Cursor -> [BindingValue]
handleLiteral c = 
  let dt = attribute "datatype" c
      ln = attribute "{http://www.w3.org/XML/1998/namespace}lang" c
      f = case dt of
        [dtype] -> flip TypedLiteral (T.unpack dtype)
        _ -> case ln of
          [lang] -> flip LangLiteral (T.unpack lang)
          _ -> Literal
          
  in case child c >>= content of 
    [x] -> [f x]
    _ -> []


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
                 , ("User-Agent", B8.pack (showVersion version))]
                 ++ requestHeaders u
                 
      qs = B8.pack $ urlEncodeVars params
      
      u' = case m of
        HGET -> u { method = "GET"
                  , requestHeaders = baseHdrs
                  , queryString = qs
                  }
                
        HPOST -> u { method = "POST"
                   , requestHeaders = NT.headerContentType "application/x-www-form-urlencoded" : baseHdrs
                   , requestBody = RequestBodyBS qs
                   }
                 
  withManager $ fmap responseBody . httpLbs u'

-- Defaults MIME/Types for SPARQL queries. '*/*' for all other possibilities.
accept :: NT.Ascii
accept = "application/sparql-results+xml, application/rdf+xml, */*"
