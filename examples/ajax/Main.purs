module AjaxExample where

import AjaxExample.Todos (init, foldp, view)
import Control.Bind (bind)
import Effect (Effect)
import Data.Unit (Unit)
import Pux (start)
import Pux.Renderer.React (renderToDOM)

main :: Effect Unit
main = do
  app <- start
    { initialState: init
    , view
    , foldp
    , inputs: [] }

  renderToDOM "#app" app.markup app.input
