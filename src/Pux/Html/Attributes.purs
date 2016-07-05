module Pux.Html.Attributes where

import Prelude
import Data.Tuple
import Data.Foldable (foldMap)
import Pux.Html (Attribute)

className :: forall a. String -> Attribute a
className = attr "class"

href :: forall a. String -> Attribute a
href = attr "href"

style :: forall a. Array (Tuple String String) -> Attribute a
style pairs = "style='" <> foldMap f pairs <> "'"
  where
    f :: Tuple String String -> String
    f (Tuple k v) =  k <> ":" <> v <> ";"

-- TODO Pux signature is: attr :: forall a v. String -> v -> Attribute a
attr :: forall a. String -> String -> Attribute a
attr attrKey v = attrKey <> "='" <> v <> "'"
