module FormsExample where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Unit (Unit)
import FormsExample.Form (update, view, init)
import Prelude (Unit, bind)
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

