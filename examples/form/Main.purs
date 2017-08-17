module FormsExample where

import Control.Bind (bind)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Unit (Unit)
import DOM (DOM)
import FormsExample.Form (foldp, view, init)
import Pux (start)
import Pux.Renderer.React (renderToDOM)
import Signal.Channel (CHANNEL)

main :: ∀ fx. Eff (channel :: CHANNEL, dom :: DOM, exception :: EXCEPTION | fx) Unit
main = do
  app <- start
    { initialState: init
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input
