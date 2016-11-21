module AjaxExample where

import AjaxExample.Todos (init, update, view)
import Control.Monad.Eff (Eff)
import Prelude (bind, Unit)
import Network.HTTP.Affjax (AJAX)
import Pux (start, renderToDOM, CoreEffects)

main :: Eff (CoreEffects (ajax :: AJAX)) Unit
main = do
  app <- start
    { initialState: init
    , update: update
    , view: view
    , inputs: [] }

  renderToDOM "#app" app.html
