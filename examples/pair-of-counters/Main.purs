module CounterPairExample where

import CounterPairExample.App (init, update, view)
import Control.Monad.Eff (Eff)
import Prelude (bind, Unit)
import Pux (start, fromSimple, renderToDOM, CoreEffects)

main :: forall e. Eff (CoreEffects e) Unit
main = do
  app <- start
    { initialState: init 0
    , update: fromSimple update
    , view: view
    , inputs: []
    }

  renderToDOM "#app" app.html
