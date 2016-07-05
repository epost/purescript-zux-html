module Pux.Html.Attributes where

import Prelude
import Data.Tuple
import Data.Foldable (foldMap)
import Pux.Html (Attribute)

className :: forall a. String -> Attribute a
className c = "class='" <> c <> "'"

style :: forall a. Array (Tuple String String) -> Attribute a
style pairs = "style='" <> foldMap f pairs <> "'"
  where
    f :: Tuple String String -> String
    f (Tuple k v) =  k <> ":" <> v <> ";"
