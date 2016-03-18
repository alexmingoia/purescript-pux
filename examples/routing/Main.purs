module RoutingExample where

import Prelude ((<<<), bind)
import Pux (start, fromSimple, renderToDOM)
import Pux.Router (sampleUrl)
import Signal ((~>))

import RoutingExample.Routes (match)
import RoutingExample.App (Action(PageView), init, update, view)

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
