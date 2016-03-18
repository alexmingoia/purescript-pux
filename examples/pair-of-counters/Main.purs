module CounterPairExample where

import CounterPairExample.App (init, update, view)
import Prelude (bind)
import Pux (start, fromSimple, renderToDOM)

main = do
  app <- start
    { initialState: init 0
    , update: fromSimple update
    , view: view
    , inputs: []
    }

  renderToDOM "#app" app.html
