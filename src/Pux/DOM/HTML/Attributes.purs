module Pux.DOM.HTML.Attributes where

import CSS.Render (render, renderedInline)
import CSS.Stylesheet (CSS)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Text.Smolder.HTML.Attributes (style) as A
import Text.Smolder.Markup (Attribute, attribute)

-- | An attribute prefixed with "data-".
data_ :: String -> String -> Attribute
data_ prop = attribute ("data-" <> prop)

-- | Control cursor focus declaratively. Whichever element has this attribute
-- | will be focused after rendering if it was not already focused.
focused :: Attribute
focused = attribute "focused" "focused"

-- | Key attribute for improved rendering performance.
-- | See: https://facebook.github.io/react/docs/lists-and-keys.html
key :: String -> Attribute
key = attribute "key"

-- | Render inline CSS and return a style attribute.
style :: CSS -> Attribute
style css = A.style (fromMaybe "" (renderedInline (render css)))
