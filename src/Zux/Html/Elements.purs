module Zux.Html.Elements (
  Html,
  unHtml,
  text, span, ul, li, table, tr, td, div
  ) where

import Prelude ((<>), class Semigroup)
import Data.Monoid
import Data.Foldable (intercalate, foldMap)
import Zux.Html.Attributes (Attribute)

--------------------------------------------------------------------------------

newtype Html a = Html String

unHtml :: forall a. Html a -> String
unHtml (Html s) = s

instance semigroupHtml :: Semigroup (Html a) where
  append (Html x) (Html y) = Html (x <> y)

instance monoidHtml :: Monoid (Html a) where
  mempty = Html ""

--------------------------------------------------------------------------------

text :: forall a. String -> Html a
text str = Html str

span :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
span = mkTag "span"

-- | Example application: `ul [] [li [] [text "hoi", text "poes"], li [] [text "dag", text "hond"]]`.
ul :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
ul = mkTag "ul"

-- | Example application: `li [] [text "hoi"]`.
li :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
li = mkTag "li"

table :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
table = mkTag "table"

tr :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
tr = mkTag "tr"

td :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
td = mkTag "td"

div :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
div = mkTag "div"

p :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
p = mkTag "p"

h1 :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
h1 = mkTag "h1"

mkTag :: forall a. String -> Array (Attribute a) -> Array (Html a) -> Html a
mkTag tagName attrs kids = Html (openTag <> content <> closeTag)
  where
    openTag  = "<" <> tagName <> " " <> attrs' <> ">"
    content  = foldMap unHtml kids
    closeTag = "</" <> tagName <> ">"
    attrs'   = intercalate " " attrs
