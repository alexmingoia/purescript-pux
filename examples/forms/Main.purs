module FormsExample where

import Control.Bind (bind)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Unit (Unit)
import FormsExample.Form (update, view, init)
import Pux (start, fromSimple, renderToDOM)
import Signal.Channel (CHANNEL)

main :: forall e. Eff ( channel :: CHANNEL, err :: EXCEPTION | e ) Unit
main = do
  app <- start
    { initialState: init
    , update: fromSimple update
    , view: view
    , inputs: []
    }

  renderToDOM "#app" app.html

