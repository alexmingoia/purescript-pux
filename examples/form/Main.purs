module FormsExample where

import Control.Bind (bind)
import Effect (Effect)
import Data.Unit (Unit)
import FormsExample.Form (foldp, view, init)
import Pux (start)
import Pux.Renderer.React (renderToDOM)

main :: Effect Unit
main = do
  app <- start
    { initialState: init
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input
