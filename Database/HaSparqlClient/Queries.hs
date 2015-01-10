{-# LANGUAGE OverloadedStrings, TupleSections #-}

{- | This module provides functions to access remote endpoints.

-}

-- TODO: need to find out what the encoding is and use it to convert to Text
--       Is this still true?

{-
The SPARQL response XML format is described at

http://www.w3.org/TR/rdf-sparql-XMLres/

Note that there is no attempt at providing access to any
additional metadata about the search results provided within
the head element of the response (the link tag).
-}

module Database.HaSparqlClient.Queries 
       (
         runQuery
       , runSelectQuery
       , runAskQuery
         -- * Options
       , QueryOptions
       , queryTimeout
       ) where

import Control.Exception as CE

import qualified Data.ByteString.Char8 as B8
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.XML.Types as XT

import qualified Network.HTTP.Types as NT

import qualified Text.XML as X
import qualified Text.XML.Stream.Parse as X

import Control.Applicative ((<|>))
import Control.Monad (liftM)
import Control.Monad.Trans.Resource (MonadThrow, ResourceT)

import Data.Char (toLower)
import Data.Conduit (ConduitM, Sink, ($$+-), (=$=))
import Data.Default (Default)
import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat)

import Network.URI
import Network.HTTP.Conduit
import Network.HTTP (urlEncodeVars)

import qualified Database.HaSparqlClient.Types as HT
import Database.HaSparqlClient.Types hiding (URI)
import Paths_hasparql_client (version)
import Data.Version (showVersion)

-- Specializing to IO here
type ProcessResponseBody a =
  Sink B8.ByteString (ResourceT IO) a
  -- ConduitM B8.ByteString Void (ResourceT IO)

-- | Options for the query. At present only contains the response timeout
--   setting.
data QueryOptions =
  QueryOptions
  { queryTimeout :: Maybe Int
    -- ^ Number of microseconds before the query times out. The default
    --   is 10 seconds. A value of @Nothing@ turns off the timeout.
  }

instance Show QueryOptions where
  show x = unlines
    [ "QueryOptions {"
    , "  queryTimeout  = " ++ show (queryTimeout x)
    , "}"
    ]

instance Default QueryOptions where
  def = QueryOptions
    { queryTimeout = Just (10 * 1000000)
    }
    
{- | Execute a service.

On success returns a string created from the service.

Returns an error message on failure. SPARUL and SPARQL can be performed.
-}
runQuery ::
  QueryOptions
  -> Service
  -> Method
  -> [MIMEType]
  -- ^ This set of mime types is added to the start of
  --   @[mtSPARQLResultsXML, mtRDFXML, mtAny]@, and then used
  --   in the @Accept@ field of the request.
  -> IO (Either String T.Text)
runQuery opt srv = getSparqlRequest opt
               ((T.decodeUtf8 . mconcat) `liftM` CL.consume)
               (constructURI srv)

-- | Find all possible values for a query of type @SELECT@, returning the bound variables.
--   If it fails returns an error message.
--
--   An example:
--
-- > select = do
-- >    let s = Sparql "http://dbpedia.org/sparql/" query [] [] []
-- >        query = "SELECT ?p ?o { <http://dbpedia.org/resource/Amazon_River> ?p ?o . }"
-- >    res <- runSelectQuery def s HPOST
-- >    case res of
-- >      Left e -> print $ "Error:" ++ e
-- >      Right (varnames,rows) -> print rows
runSelectQuery ::
  QueryOptions
  -> Service -- ^ There is no check that this request is a @SELECT@ query.
  -> Method
  -> IO (Either String ([T.Text], [[BindingValue]]))
  -- ^ On success, returns the variables names (@[Text]@)
  --   and the RDF terms bound to these variables (in the same
  --   order), for each row. The list of results will be empty if
  --   there was no match.
runSelectQuery opt srv m = getSparqlRequest opt parseResultsDocument (constructURI srv) m []

-- | Return @Right True@ or @Right False@ for a query of type @ASK@. 
--   If it fails returns an error message.
--
--   An example:
--
-- > ask = do
-- >    let s = Sparql "http://dbpedia.org/sparql/" query [] [] []
-- >        query = "PREFIX prop: <http://dbpedia.org/property/>" ++
-- >         " ASK { <http://dbpedia.org/resource/Amazon_River> prop:length ?amazon ." ++
-- >         " <http://dbpedia.org/resource/Nile> prop:length ?nile ." ++
-- >         "FILTER(?amazon > ?nile) .} "
-- >    res <- runAskQuery def s HGET
-- >    case res of
-- >      Left e -> print $ "Error:" ++ e
-- >      Right s -> print s

runAskQuery ::
  QueryOptions
  -> Service -- ^ There is no check that this request is an @ASK@ query.
  -> Method
  -> IO (Either String Bool)
runAskQuery opt serv m = getSparqlRequest opt
                     parseBooleanDocument
                     (constructURI serv) m []

-- In case of success makes the request and transforms the result depending on the callback function.

getSparqlRequest ::
  QueryOptions
  -> ProcessResponseBody a
  -> Either String (URI, [ExtraParameters]) 
  -> Method
  -> [MIMEType] 
  -> IO (Either String a)
getSparqlRequest opt stream u m mts = 
  case u of
    Left err -> return $ Left err
    Right urivals -> do
      resp <- processResponse opt stream urivals m mts
      case resp of
        Left err -> return $ Left (show err)
        Right r -> return $ Right r

-- QUS: Do we need to send in the mime type values into construct URI ???

-- This function looks if the Endpoint is a valid URI,
-- then returns the URI and other parameters are fixed and added.                                            
constructURI :: Service -> Either String (URI, [ExtraParameters])                                            
constructURI (Sparql epoint qry defgs ngs oth) = 
  case parseURI epoint of
    Nothing -> Left "Bad string for endpoint."
    Just uri -> Right (uri, ("query", qry) : dgraphs ++ ngraphs ++ filtparams oth)
    where
      ngraphs = zip (repeat "named-graph-uri") ngs
      dgraphs = zip (repeat "default-graph-uri") defgs
      filtparams = filter keep
      keep = (`notElem` ["named-graph-uri", "default-graph-uri"]) . map toLower . fst
         
-- Return the names of the variables and then the per-row value sets.
parseResultsDocument :: ProcessResponseBody ([T.Text], [[BindingValue]])
parseResultsDocument =
  X.parseBytes X.def =$= parseSparqlResults

-- Is the answer true of false?
parseBooleanDocument :: ProcessResponseBody Bool
parseBooleanDocument =
  X.parseBytes X.def =$= parseSparqlBoolean
  
nSparql, nHead, nVariable, nResults, nResult, nBinding, nLiteral, nBnode, nUri :: X.Name
nSparql = "{http://www.w3.org/2005/sparql-results#}sparql"
nHead = "{http://www.w3.org/2005/sparql-results#}head"
nVariable = "{http://www.w3.org/2005/sparql-results#}variable"
nResults = "{http://www.w3.org/2005/sparql-results#}results"
nResult = "{http://www.w3.org/2005/sparql-results#}result"
nBinding = "{http://www.w3.org/2005/sparql-results#}binding"
nLiteral = "{http://www.w3.org/2005/sparql-results#}literal"
nBnode = "{http://www.w3.org/2005/sparql-results#}bnode"
nUri = "{http://www.w3.org/2005/sparql-results#}uri"

nBoolean, nLink :: X.Name
nBoolean = "{http://www.w3.org/2005/sparql-results#}boolean"
nLink = "{http://www.w3.org/2005/sparql-results#}link"

nName, nDatatype :: X.Name
nName = "name"
nDatatype = "datatype"

nLang :: X.Name
nLang = "{http://www.w3.org/XML/1998/namespace}lang"

-- NOTE: I had used X.tagNoAttr for many tags, but
-- Virtuoso 7.0.0 added attributes to the results tag
-- (e.g. <results distinct="false" ordered="true">)
-- so I have decided to use this apporach instead,
-- which ignores any attributes rather than requires
-- there to be none.
--
tagNoAttr ::
  (MonadThrow m)
  => X.Name
  -> ConduitM XT.Event o m b
  -> ConduitM XT.Event o m (Maybe b)
tagNoAttr name parser = X.tagName name X.ignoreAttrs $ const parser

-- | Parse the SPARQL Boolean XML format (@ASK@).

parseSparqlBoolean :: (MonadThrow m) => ConduitM XT.Event o m Bool
parseSparqlBoolean =
  X.force "sparql results" $ X.tagName nSparql X.ignoreAttrs $ \_ -> do
    _ <- X.force "Unable to parse head tag" parseHead
    X.force "boolean" $ tagNoAttr nBoolean $ do
      cnt <- X.content
      return $ cnt == "true"

-- | Parse the SPARQL results XML format (@SELECT@).

parseSparqlResults :: (MonadThrow m) => ConduitM XT.Event o m ([T.Text], [[BindingValue]])
parseSparqlResults =
  X.force "sparql results" $ X.tagName nSparql X.ignoreAttrs $ \_ -> do
    varNames <- X.force "Unable to parse head tag" parseHead
    results <- X.force "Unable to parse results tag" (parseResults varNames)
    return (varNames, results)

-- | Get the variable name from a variable tag.
    
parseVariable :: (MonadThrow m) => ConduitM XT.Event o m (Maybe T.Text)
parseVariable = X.tagName nVariable (X.requireAttr nName) return

-- | Extract the list of variable names from the head tag.
--
--   Any @link@ information is discarded.
--
parseHead :: (MonadThrow m) => ConduitM XT.Event o m (Maybe [T.Text])
parseHead = tagNoAttr nHead $ do
  res <- X.many parseVariable
  _ <- X.many $ X.tagName nLink X.ignoreAttrs $ \_ -> return ()
  return res

-- | Parse the results section.

parseResults ::
  (MonadThrow m)
  => [T.Text]    -- ^ order of variables
  -> ConduitM XT.Event o m (Maybe [[BindingValue]])
parseResults varNames =
  tagNoAttr nResults $ X.many (parseResult varNames)
  
-- | Parse a set of bindings (a result tag).

parseResult ::
  (MonadThrow m)
  => [T.Text]    -- ^ order of variables
  -> ConduitM XT.Event o m (Maybe [BindingValue])
parseResult varNames =
  tagNoAttr nResult $ do
    fs <- X.many parseBinding
    return $ map (fromMaybe HT.Unbound . flip lookup fs) varNames 

-- | Parse a binding tag.

parseBinding :: (MonadThrow m) => ConduitM XT.Event o m (Maybe (T.Text, BindingValue))
parseBinding = X.tagName nBinding (X.requireAttr nName) $ \name -> do
  res <- X.force "Unable to parse contents of binding tag" $ X.choose [parseBNode, parseURIElem, parseLiteral]
  return (name, res)

parseBNode, parseURIElem, parseLiteral  :: (MonadThrow m) => ConduitM XT.Event o m (Maybe BindingValue)
parseBNode = tagNoAttr nBnode $ HT.BNode `liftM` X.content

parseURIElem = tagNoAttr nUri $ HT.URI `liftM` X.content

parseLiteral = X.tagName nLiteral getLiteralAttr (`liftM` X.content)

getLiteralAttr :: X.AttrParser (T.Text -> BindingValue)
getLiteralAttr =
  (flip TypedLiteral `liftM` X.requireAttr nDatatype)
  <|>
  (flip LangLiteral `liftM` X.requireAttr nLang)
  <|>
  (X.ignoreAttrs >> return HT.Literal)

{-
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
getBNodeBinding = getBindingValue "{http://www.w3.org/2005/sparql-results#}bnode" BNode
getURIBinding   = getBindingValue "{http://www.w3.org/2005/sparql-results#}uri"   HT.URI

getLiteralBinding c = element "{http://www.w3.org/2005/sparql-results#}literal" c >>= handleLiteral

handleLiteral :: Cursor -> [BindingValue]
handleLiteral c = 
  let dt = attribute "datatype" c
      ln = attribute "{http://www.w3.org/XML/1998/namespace}lang" c
      f = case dt of
        [dtype] -> flip TypedLiteral dtype
        _ -> case ln of
          [lang] -> flip LangLiteral lang
          _ -> Literal
          
  in case child c >>= content of 
    [x] -> [f x]
    _ -> []

-}

makeCall ::
  QueryOptions
  -> ProcessResponseBody a  -- ^ process the response body
  -> (URI, [ExtraParameters])
  -> Method
  -> [MIMEType]
  -> IO a
makeCall opt stream (uri, params) m mts = do
  -- strip out any basic authentication since parseUrl does not handle this
  let (uri', basicAuth) = case uriAuthority uri of
        Nothing -> (uri, "")
        Just ua -> case uriUserInfo ua of
          "" -> (uri, "")
          x -> (uri { uriAuthority = Just (ua { uriUserInfo = ""}) }, x)
        
      (uname, upass1) = break (==':') basicAuth
      upass = case upass1 of
        "" -> ""
        ':':xs -> dropAt xs
        ys -> error $ "Unexpected password value of " ++ ys ++ " in URI: " ++ show uri
        
      -- assume passwords are not long
      dropAt = reverse . d . reverse
          where
            d ('@':xs) = xs
            d xs = xs

  u <- parseUrl $ show uri'
  
  let acceptVals = -- should probably just use a single ByteString for default case
          B8.intercalate ", " $ mts ++ [mtSPARQLResultsXML, mtRDFXML, mtAny]
          {- OLD CODE: not sure why I chose this
            case mts of
              [] -> [mtSPARQLResultsXML, mtRDFXML, mtAny]
              _  -> mtSPARQLResultsXML : mts ++ [mtAny]
           -}

      baseHdrs = [ (NT.hAccept, acceptVals)
                 , ("Accept-Charset", "utf-8")
                 , (NT.hUserAgent, B8.pack (showVersion version))]
                 ++ requestHeaders u
                 
      qs = B8.pack $ urlEncodeVars params
      
      u' = case m of
        HGET -> u { method = "GET"
                  , requestHeaders = baseHdrs
                  , queryString = qs
                  }
                
        HPOST -> u { method = "POST"
                   , requestHeaders = (NT.hContentType, "application/x-www-form-urlencoded") : baseHdrs
                   , requestBody = RequestBodyBS qs
                   }

      u'' = u' { responseTimeout = queryTimeout opt }
      
      request = if null uname
            then u''
            else applyBasicAuth (B8.pack uname) (B8.pack upass) u''

  withManager $ \mgr -> do
    response <- http request mgr
    responseBody response $$+- stream

-- TODO: should this catch HTTPException, or is this part of IOError?
    
processResponse ::
  QueryOptions
  -> ProcessResponseBody a  -- ^ process the response body
  -> (URI, [ExtraParameters])
  -> Method
  -> [MIMEType]
  -> IO (Either IOError a)
processResponse opt stream u m mts =
  let ehdl :: IOError -> IO (Either IOError a)
      ehdl = return . Left
  in CE.catch
     (Right `liftM` makeCall opt stream u m mts)
     ehdl

