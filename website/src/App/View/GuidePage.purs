module App.View.GuidePage where

import App.Events (Event, State)
import App.Sidebar (view) as Sidebar
import Control.Bind (bind)
import Data.Function (($))
import Data.Maybe (Maybe(..))
import Pux.DOM.HTML (HTML)
import Text.Markdown.SlamDown.Smolder (toMarkup)
import Text.Smolder.HTML (div)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup (text, (!))

view :: State -> HTML Event
view st =
  div ! className "guidepage" $ do
    Sidebar.view st.url

    div ! className "main" $ do
      case st.guidepage.markdown of
        Nothing -> text "Loading..."
        Just md -> toMarkup md
