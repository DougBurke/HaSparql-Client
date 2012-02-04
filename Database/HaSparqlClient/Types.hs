-- | Type definitions.
module Database.HaSparqlClient.Types (-- * SELECT binding value
    BindingValue(..), 
    -- * SPARQL service
    Service(..), 
    -- ** Required elements
    Endpoint, Query, 
    -- ** Optional elements
    DefaultGraph, NamedGraph, ExtraParameters, Key, Value, 
    -- * Request method
    Method(..),
    defaultService) where

-- | Representation for SELECT query result format.
data BindingValue = URI String -- ^ URI reference to remote resource.
                    | Literal String -- ^ Literal string without datatype or lang.
                    | TypedLiteral String String -- ^ Literal with datatype URI.
                    | LangLiteral  String String -- ^ Literal with language.
                    | BNode String  -- ^ Blank Node with label.
                    | Unbound -- ^ Unbound result value.
                    deriving (Eq, Show) 

-- | Local representation for a SPARQL service.
data Service = Sparql{ endpoint :: Endpoint, 
                query :: Query, 
                defaultgraph :: DefaultGraph, 
                namedgraph :: [NamedGraph], 
                optionalparameters :: [(ExtraParameters)]
                } deriving (Eq, Show)

-- | Represents a SPARQL endpoint.
type Endpoint = String

-- | SPARQL query 'String'.
type Query = String

-- | Add a default graph URI. Overrides the default graph from SPARQL queries.
type DefaultGraph = Maybe String

-- | Add a named graph URI. Overrides named graphs from SPARQL queries.
type NamedGraph = String

-- | Some SPARQL endpoints require extra key value pairs. E.g., in Virtuoso Server, one would add should-sponge=soft to the query forcing virtuoso to retrieve graphs that are not stored in its local database. Can be for example, used to try others output formats in 'RunQuery' depending on the server.
type ExtraParameters = (Key,Value)

-- | key of the query part.
type Key = String -- ^ named-graph-uri and default-graph-uri keys not allowed here. They're removed.

-- | value of the query part.
type Value = String

-- | Set to HTTP GET or POST request, according to the SPARQL protocol, some endpoints do not yet support POST requests. Some SPARQL queries, perhaps machine generated, may be longer than can be reliably conveyed by way of the HTTP GET. In those cases the POST may be used.
data Method = HGET | HPOST deriving (Eq, Show)

-- | Just a example.
defaultService :: Service
defaultService = Sparql "http://dbpedia.org/sparql" "select ?s ?p ?o where {?s ?p ?o} limit 1" Nothing [] []
