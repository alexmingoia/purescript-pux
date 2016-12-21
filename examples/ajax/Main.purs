module AjaxExample where

import AjaxExample.Todos (init, foldp, view)
import Control.Bind (bind)
import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import Network.HTTP.Affjax (AJAX)
import Pux (start, CoreEffects)
import Pux.Renderer.React (renderToDOM)

main :: Eff (CoreEffects (ajax :: AJAX)) Unit
main = do
  app <- start
    { initialState: init
    , view
    , foldp
    , inputs: [] }

  renderToDOM "#app" app.markup app.input
