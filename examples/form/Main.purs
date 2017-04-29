module FormsExample where

import Control.Bind (bind)
import Control.Monad.Eff (Eff)
import Data.Unit (Unit)
import FormsExample.Form (foldp, view, init)
import Pux (CoreEffects, start)
import Pux.Renderer.React (renderToDOM)

main :: ∀ fx. Eff (CoreEffects fx) Unit
main = do
  app <- start
    { initialState: init
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input
