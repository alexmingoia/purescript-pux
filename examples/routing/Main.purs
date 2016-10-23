module RoutingExample where

import Control.Monad.Eff (Eff)
import DOM (DOM)
import Prelude ((<<<), bind, Unit)
import Pux (start, fromSimple, renderToDOM, CoreEffects)
import Pux.Router (sampleUrl)
import RoutingExample.Routes (match)
import RoutingExample.App (Action(PageView), init, update, view)
import Signal ((~>))

main :: Eff (CoreEffects (dom :: DOM)) Unit
main = do
  urlSignal <- sampleUrl
  let routeSignal = urlSignal ~> (PageView <<< match)

  app <- start
    { initialState: init
    , update: fromSimple update
    , view: view
    , inputs: [routeSignal]
    }

  renderToDOM "#app" app.html
