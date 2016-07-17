module CounterPairExample where

import CounterPairExample.App (init, update, view)
import Prelude (bind, map)
import Pux (start, fromSimple, renderToDOM)

main = do
  app <- start
    { initialState: init 0
    , update: fromSimple update
    , view: map view
    , inputs: []
    }

  renderToDOM "#app" app.html
