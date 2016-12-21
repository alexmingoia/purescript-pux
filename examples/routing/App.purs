module RoutingExample.App where

import Control.Applicative (pure)
import Control.Bind ((=<<), bind)
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM)
import DOM.Event.Event (preventDefault)
import DOM.HTML (window)
import DOM.HTML.History (DocumentTitle(..), URL(..), pushState)
import DOM.HTML.Types (HISTORY)
import DOM.HTML.Window (history)
import Data.Foreign (toForeign)
import Data.Function (($))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (show)
import Pux (EffModel, noEffects)
import Pux.DOM.Events (DOMEvent, onClick)
import Pux.DOM.HTML (HTML)
import RoutingExample.Routes (Route(Home, Users, User, NotFound), match)
import Text.Smolder.HTML (a, div, h1, li, nav, ul)
import Text.Smolder.HTML.Attributes (href)
import Text.Smolder.Markup ((!), (#!), text)

data Event = PageView Route | Navigate String DOMEvent

type State = { currentRoute :: Route }

init :: State
init = { currentRoute: Home }

foldp :: ∀ fx. Event -> State -> EffModel State Event (history :: HISTORY, dom :: DOM | fx)
foldp (Navigate url ev) st =
  { state: st
  , effects: [
      liftEff do
        preventDefault ev
        h <- history =<< window
        pushState (toForeign {}) (DocumentTitle "") (URL url) h
        pure $ Just $ PageView (match url)
    ]
  }
foldp (PageView route) st = noEffects $ st { currentRoute = route }

view :: State -> HTML Event
view state =
  div do
    navigation
    page state.currentRoute

page :: Route -> HTML Event
page Home      = h1 $ text "Home"
page (Users sortBy) = h1 $ text ("Users sorted by: " <> sortBy)
page (User id) = h1 $ text ("User: " <> show id)
page NotFound  = h1 $ text "Not Found"

navigation :: HTML Event
navigation =
  nav do
    ul do
      li $ a ! href "/" #! onClick (Navigate "/") $ text "Home"
      li $ a ! href "/users" #! onClick (Navigate "/users") $ text "Users"
      li $ a ! href "/users?sortBy=age" #! onClick (Navigate "/users?sortBy=age") $ text "Users sorted by age."
      li $ a ! href "/users/123" #! onClick (Navigate "/users/123") $ text "User 123"
      li $ a ! href "/foobar" #! onClick (Navigate "/foobar") $ text "Not found"
