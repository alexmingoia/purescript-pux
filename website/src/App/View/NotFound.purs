module App.View.NotFound where

import Data.Function (($))
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (div, h2)
import Text.Smolder.Markup (text)

view :: forall st ev. st -> HTML ev
view state = div $ h2 $ text "404 Not Found"
