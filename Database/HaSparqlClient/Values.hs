-- | This module provides some convenience functions to get values from BindingValue. 
module Database.HaSparqlClient.Values(-- * Getting values
                                    languageValue, datatypeValue, uriValue, literalValue, bnodeValue, value, 
                                    -- * XML representation
                                    showsparql) where

import Database.HaSparqlClient.Types

-- | SPARQL XML representation.
class ShowQuery a where
  showsparql :: a -> String
    
-- | SPARQL XML representation for 'BindingValue'.
instance ShowQuery BindingValue where
  showsparql (URI uri) = "<uri>" ++ uri ++ "</uri>"
  showsparql (Literal str) = "<literal>"++ str ++ "</literal>"
  showsparql (TypedLiteral str tp) = "<literal datatype=\"" ++ tp ++ "\">" ++ str ++ "</literal>"
  showsparql (BNode str) = "<bnode>" ++ str ++ "</bnode>"
  showsparql (LangLiteral str lan) = "<literal xml:lang=\"" ++ lan ++ "\">" ++ str ++ "</literal>"
  showsparql _ = ""

-- | Get the language value for a 'BindingValue'. Return 'Nothing' if 'BindingValue' is not 'LangLiteral'.
languageValue :: BindingValue -> Maybe String
languageValue (LangLiteral _ lan) = Just lan
languageValue _ = Nothing 

-- | Get the datatype value for a 'BindingValue'. Return 'Nothing' if 'BindingValue' is not 'TypedLiteral'.
datatypeValue :: BindingValue -> Maybe String
datatypeValue (TypedLiteral _ tp) = Just tp
datatypeValue _ = Nothing

-- | Get the 'URI' value for a BindingValue. Return 'Nothing' if 'BindingValue' is not 'URI'.
uriValue :: BindingValue -> Maybe String
uriValue (URI str) = Just str
uriValue _ = Nothing

-- | Get the literal value for a 'BindingValue'. Return 'Nothing' if not is of the any literal type.
literalValue :: BindingValue -> Maybe String
literalValue (Literal str) = Just str
literalValue (TypedLiteral str _) = Just str
literalValue (LangLiteral str _) = Just str
literalValue _ = Nothing

-- | Get the 'BNode' value for a 'BindingValue'. Return 'Nothing' if 'BindingValue' is not 'BNode'.
bnodeValue :: BindingValue -> Maybe String
bnodeValue (BNode bno) = Just bno
bnodeValue _ = Nothing

-- | Get the value for a 'BindingValue'. Return 'Nothing' if BindingValue is 'Unbound'.
value :: BindingValue -> Maybe String
value res = case (uriValue res) of
				Nothing -> case (literalValue res) of
							Nothing -> case (bnodeValue res) of
									Nothing -> Nothing
									Just bno -> Just bno
							Just lit -> Just lit
				Just uri -> Just uri
