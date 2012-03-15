{-# LANGUAGE TypeSynonymInstances #-}

-- | Type definitions.

module Database.HaSparqlClient.Types (
  -- * SELECT binding value
  BindingValue(..) 
  
  -- * SPARQL service
  , Service(..)
  , ToSPARQL(..)
    
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
  , defaultService
    
  ) where

import qualified Data.Text as T

-- | Construct a SPARQL query string.
--
--   The intent is to support a type-safe version as provided
--   by the hsparql package as well as basic string values
--   for flexibility (e.g. to use features that may not be
--   supported by the Query EDSL or when you are provided a
--   query by an external agent). Unfortunately it has the
--   wrong kind to support the monadic Query from
--   the hsparql package, so it will probably be a very
--   short-lived experiment.
--
class ToSPARQL a where
  toSPARQL :: a -> Query

instance ToSPARQL String where
  toSPARQL = id

instance ToSPARQL T.Text where
  toSPARQL = T.unpack


-- | Representation for SELECT query result format.
data BindingValue = 
  URI String -- ^ URI reference to remote resource.
  | Literal T.Text -- ^ Literal string without datatype or lang.
  | TypedLiteral T.Text String -- ^ Literal with datatype URI.
  | LangLiteral  T.Text String -- ^ Literal with language.
  | BNode String  -- ^ Blank Node with label.
  | Unbound -- ^ Unbound result value.
  deriving (Eq, Show) 

-- | Local representation for a SPARQL service.
data Service a = 
  Sparql
  { 
    endpoint :: Endpoint -- ^ The URI of the SPARQL end point.
  , query :: a -- ^ The query, which should be an instance of 'ToSPARQL'.
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

-- | An example query of DBPedia, using the <http://dbpedia.org/sparql> endpoint, where
--   the query is
--
--   > select * where { ?s ?p ?o . } limit 10
--
defaultService :: Service String
defaultService = Sparql "http://dbpedia.org/sparql" "select * where {?s ?p ?o} limit 10" [] [] []
