module BasicExample where

import BasicExample.Counter (update, view)
import Prelude (bind, map)
import Pux (start, fromSimple, renderToDOM)

main = do
  app <- start
    { initialState: 0
    , update: fromSimple update
    , view: map view
    , inputs: []
    }

  renderToDOM "#app" app.html
