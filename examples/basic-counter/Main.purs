module BasicExample where

import BasicExample.Counter (update, view)
import Prelude (bind)
import Pux (start, fromSimple, renderToDOM)

main = do
  app <- start
    { initialState: 0
    , update: fromSimple update
    , view: view
    , inputs: []
    }

  renderToDOM "#app" app.html
