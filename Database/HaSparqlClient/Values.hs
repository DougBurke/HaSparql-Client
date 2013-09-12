-- | This module provides some convenience functions to get values from a `BindingValue`. 

module Database.HaSparqlClient.Values (
  -- * Getting values
  languageValue
  , datatypeValue
  , uriValue
  , literalValue
  , bnodeValue
  , value 
    
    -- * XML representation
  , ShowQuery(..)
  ) where

import qualified Data.Text as T
import Database.HaSparqlClient.Types

-- | SPARQL XML representation.
class ShowQuery a where
  showsparql :: a -> String
    
-- | SPARQL XML representation for 'BindingValue'.
instance ShowQuery BindingValue where
  showsparql (URI uri) = "<uri>" ++ T.unpack uri ++ "</uri>"
  showsparql (Literal txt) = "<literal>" ++ T.unpack txt ++ "</literal>"
  showsparql (TypedLiteral txt tp) = "<literal datatype=\"" ++ T.unpack tp ++ "\">" ++ T.unpack txt ++ "</literal>"
  showsparql (BNode txt) = "<bnode>" ++ T.unpack txt ++ "</bnode>"
  showsparql (LangLiteral txt lan) = "<literal xml:lang=\"" ++ T.unpack lan ++ "\">" ++ T.unpack txt ++ "</literal>"
  showsparql _ = ""

-- | Get the language value for a 'BindingValue'. Return 'Nothing' if 'BindingValue' is not 'LangLiteral'.
languageValue :: BindingValue -> Maybe T.Text
languageValue (LangLiteral _ lan) = Just lan
languageValue _ = Nothing 

-- | Get the datatype value for a 'BindingValue'. Return 'Nothing' if 'BindingValue' is not 'TypedLiteral'.
datatypeValue :: BindingValue -> Maybe T.Text
datatypeValue (TypedLiteral _ tp) = Just tp
datatypeValue _ = Nothing

-- | Get the 'URI' value for a BindingValue. Return 'Nothing' if 'BindingValue' is not 'URI'.
uriValue :: BindingValue -> Maybe T.Text
uriValue (URI str) = Just str
uriValue _ = Nothing

-- | Get the literal value for a 'BindingValue'. Return 'Nothing' if not is of the any literal type.
literalValue :: BindingValue -> Maybe T.Text
literalValue (Literal txt) = Just txt
literalValue (TypedLiteral txt _) = Just txt
literalValue (LangLiteral txt _) = Just txt
literalValue _ = Nothing

-- | Get the 'BNode' value for a 'BindingValue'. Return 'Nothing' if 'BindingValue' is not 'BNode'.
bnodeValue :: BindingValue -> Maybe T.Text
bnodeValue (BNode bno) = Just bno
bnodeValue _ = Nothing

-- | Get the value for a 'BindingValue'. Return 'Nothing' if BindingValue is 'Unbound'.
value :: BindingValue -> Maybe T.Text
value res = case uriValue res of
  Nothing -> case literalValue res of
    Nothing -> case bnodeValue res of
      Nothing -> Nothing
      Just bno -> Just bno
    Just lit -> Just lit
  Just uri -> Just uri
