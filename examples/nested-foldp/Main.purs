module CounterPairExample where

import Control.Bind (bind)
import Effect (Effect)
import CounterPairExample.App (init, foldp, view)
import Data.Unit (Unit)
import Pux (start)
import Pux.Renderer.React (renderToDOM)

main :: Effect Unit
main = do
  app <- start
    { initialState: init 0
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input
