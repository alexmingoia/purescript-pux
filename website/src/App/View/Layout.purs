module App.View.Layout where

import App.Events (Event, State)
import App.Routes (Route(..))
import App.Style (appStyle)
import App.View.GuidePage (view) as GuidePage
import App.View.NotFound (view) as NotFound
import Control.Bind (bind)
import Data.Function (($))
import Pux.DOM.HTML (HTML, style)
import Pux.DOM.HTML.Attributes (key)
import Text.Smolder.HTML (div)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup ((!))

view :: State -> HTML Event
view st = div do
  style appStyle ! key "app-style"

  case st.route of
    (NotFound url) -> NotFound.view st
    (Home url) -> div ! className "homepage" $ GuidePage.view st
    (Guide url) -> GuidePage.view st
