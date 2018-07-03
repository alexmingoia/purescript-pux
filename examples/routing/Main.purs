module RoutingExample where

import Control.Bind ((=<<), bind)
import Effect (Effect)
import Web.HTML (window)
import Data.Function ((<<<))
import Data.Unit (Unit)
import Pux (start)
import Pux.DOM.History (sampleURL)
import Pux.Renderer.React (renderToDOM)
import RoutingExample.App (Event(PageView), init, foldp, view)
import RoutingExample.Routes (match)
import Signal ((~>))

main :: Effect Unit
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
