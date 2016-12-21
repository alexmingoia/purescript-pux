module CounterPairExample where

import Control.Bind (bind)
import Control.Monad.Eff (Eff)
import CounterPairExample.App (init, foldp, view)
import Data.Unit (Unit)
import Pux (start, CoreEffects)
import Pux.Renderer.React (renderToDOM)

main :: ∀ fx. Eff (CoreEffects fx) Unit
main = do
  app <- start
    { initialState: init 0
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input
