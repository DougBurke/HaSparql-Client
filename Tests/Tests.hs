module Tests where

import Database.HaSparqlClient

{- See some SPARQL endpoints: 
http://www.w3.org/wiki/SparqlEndpoints
All examples work with HGET and HPOST.
-}

simpleService :: Endpoint -> Query -> Service
simpleService epoint qry = Sparql epoint qry [] [] []

{- Expected: Right True or Right False
   Is the Amazon river longer than the Nile River?
   Example found at: http://www.cambridgesemantics.com/2008/09/sparql-by-example/#%2837%29
-}

ask :: IO ()
ask = do
        let s = simpleService "http://dbpedia.org/sparql/" qry
        res <- runAskQuery s HGET
        printf res
  where
   qry = "PREFIX prop: <http://dbpedia.org/property/>" ++
    "ASK { <http://dbpedia.org/resource/Amazon_River> prop:length ?amazon ." ++
     " <http://dbpedia.org/resource/Nile> prop:length ?nile ." ++
     "FILTER(?amazon > ?nile) .} "

select :: IO ()
select = do 
    let s = simpleService "http://api.talis.com/stores/periodicals/services/sparql" "select ?x where {?x a ?z} limit 10" 
    res <- runSelectQuery s HGET
    printf res
        
-- Find places near the Eiffel Tower in a radius of 20 Km.

select2 :: IO ()
select2 = do
        let s = simpleService "http://dbpedia.org/sparql" qry
        res <- runSelectQuery s HPOST
        printf res
  where
    qry = "SELECT DISTINCT ?resource ?label (bif:st_distance( ?point1,?point2 )) AS ?distance "
                    ++ "WHERE {<http://dbpedia.org/resource/Eiffel_Tower> geo:geometry ?point1."
                    ++ "?resource geo:geometry ?point2."
                    ++ "?resource rdfs:label ?label."
                    ++ "?resource a ?type."
                    ++ "FILTER ((?type = <http://www.opengis.net/gml/_Feature>) || (?type = <http://dbpedia.org/ontology/Place>))."
                    ++ "FILTER(bif:st_intersects(?point2,?point1,20))."
                    ++ "FILTER( lang( ?label ) = \"en\")."
                    ++ "FILTER(?resource != <http://dbpedia.org/resource/Eiffel_Tower>)."
                    ++ "} ORDER BY ASC(?distance) LIMIT 20"
{- 
    Findl all musical artists.
    Example found at: http://dbtune.org/jamendo/
-}

describe :: IO ()
describe = do
            let s = simpleService "http://dbtune.org/jamendo/sparql/" "Describe <http://purl.org/ontology/mo/MusicArtist>"
            res <- runQuery s HGET
            printf res

--Just print.

printf :: (Show a) => Either String a -> IO ()
printf x = case x of
            Left e -> print $ "Error:" ++ e
            Right s -> print s

