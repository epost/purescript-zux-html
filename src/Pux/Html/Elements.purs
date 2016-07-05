module Pux.Html.Elements (
  Html,
  unHtml,
  element, text, span, a, ul, li, table, tr, td, div, p, h1, h3
  ) where

import Prelude ((<>), class Semigroup)
import Data.Monoid
import Data.Foldable (intercalate, foldMap)
import Pux.Html.Attributes (Attribute)

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
span = element "span"

a :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
a = element "a"

-- | Example application: `ul [] [li [] [text "hoi", text "poes"], li [] [text "dag", text "hond"]]`.
ul :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
ul = element "ul"

-- | Example application: `li [] [text "hoi"]`.
li :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
li = element "li"

table :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
table = element "table"

tr :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
tr = element "tr"

td :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
td = element "td"

div :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
div = element "div"

p :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
p = element "p"

h1 :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
h1 = element "h1"

h3 :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
h3 = element "h3"

element :: forall a. String -> Array (Attribute a) -> Array (Html a) -> Html a
element tagName attrs kids = Html (openTag <> content <> closeTag)
  where
    openTag  = "<" <> tagName <> " " <> attrs' <> ">"
    content  = foldMap unHtml kids
    closeTag = "</" <> tagName <> ">"
    attrs'   = intercalate " " attrs
