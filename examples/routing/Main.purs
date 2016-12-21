module RoutingExample where

import Control.Bind ((=<<), bind)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (HISTORY)
import Data.Function ((<<<))
import Data.Unit (Unit)
import Pux (start, CoreEffects)
import Pux.DOM.History (sampleURL)
import Pux.Renderer.React (renderToDOM)
import RoutingExample.App (Event(PageView), init, foldp, view)
import RoutingExample.Routes (match)
import Signal ((~>))

main :: Eff (CoreEffects (history :: HISTORY, console :: CONSOLE, dom :: DOM)) Unit
main = do
  urlSignal <- sampleURL =<< window
  let routeSignal = urlSignal ~> (PageView <<< match)

  app <- start
    { initialState: init
    , view
    , foldp
    , inputs: [routeSignal]
    }

  renderToDOM "#app" app.markup app.input
