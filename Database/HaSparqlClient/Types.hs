{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Type definitions.

module Database.HaSparqlClient.Types (
  -- * SELECT binding value
  BindingValue(..) 
  
  -- * SPARQL service
  , Service(..)
    
    -- ** Required elements
  , Endpoint
  , Query 
    
    -- ** Optional elements
  , DefaultGraph
  , NamedGraph
  , ExtraParameters
  , Key
  , Value 
    
    -- * Request method
  , Method(..)
  , MIMEType
  , mtAny, mtSPARQLResultsXML
  , mtTurtle, mtNTriples, mtRDFXML, mtTriG, mtTriX
  , defaultService
    
  ) where

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T

-- | Representation for SELECT query result format.
data BindingValue = 
  URI T.Text -- ^ URI reference to remote resource.
  | Literal T.Text -- ^ Literal string without datatype or lang.
  | LangLiteral  T.Text T.Text -- ^ Literal with language.
  | TypedLiteral T.Text T.Text -- ^ Literal with datatype URI.
  | BNode T.Text  -- ^ Blank Node with label.
  | Unbound -- ^ Unbound result value.
  deriving (Eq, Show) 

-- | Local representation for a SPARQL service.
data Service = 
  Sparql
  { 
    endpoint :: Endpoint -- ^ The URI of the SPARQL end point.
  , query :: Query -- ^ The query.
  , defaultgraph :: [DefaultGraph] -- ^ Override the default graph in the SPARQL query with the merge of these graphs
  , namedgraph :: [NamedGraph] -- ^ Override named graphs from SPARQL queries.
  , optionalparameters :: [ExtraParameters]
    -- ^ Some SPARQL endpoints require extra key value pairs. E.g., in Virtuoso Server, 
    --   one would add @should-sponge=soft@ to the query forcing virtuoso to retrieve graphs
    --   that are not stored in its local database. Can be for example, used to try other 
    --   output formats in 'RunQuery' depending on the server.
  } deriving (Eq, Show)

type Endpoint = String
type Query = String
type DefaultGraph = String
type NamedGraph = String
type ExtraParameters = (Key, Value)

-- | key of the query part.
type Key = String -- ^ named-graph-uri and default-graph-uri keys not allowed here. They're removed.

-- | value of the query part.
type Value = String

-- | Set to HTTP GET or POST request, according to the SPARQL protocol, 
--   some endpoints do not yet support POST requests. 
-- 
--   Some SPARQL queries, perhaps machine generated, may be longer than can be
--   reliably conveyed by way of the HTTP GET. In those cases POST should be used.
data Method = HGET | HPOST deriving (Eq, Show)

-- | Represent the MIME type of a request.
type MIMEType = B8.ByteString

-- resurrecting the following from lost code; just guessing in places

mtAny, mtSPARQLResultsXML, mtTurtle, mtNTriples, mtRDFXML, mtTriG, mtTriX :: MIMEType
mtAny = "*/*"
mtSPARQLResultsXML = "application/sparql-results+xml"
mtTurtle = "text/turtle"
mtNTriples = "text/plain"
mtRDFXML = "application/rdf+xml"
mtTriG = "text/plain"
mtTriX = "application/xml"

-- | An example query of DBPedia, using the <http://dbpedia.org/sparql> endpoint, where
--   the query is
--
--   > select * where { ?s ?p ?o . } limit 10
--
defaultService :: Service
defaultService = Sparql "http://dbpedia.org/sparql" "select * where {?s ?p ?o} limit 10" [] [] []
