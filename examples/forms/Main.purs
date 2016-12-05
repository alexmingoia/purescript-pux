module FormsExample where

import FormsExample.Form (update, view, init)
import Prelude (bind)
import Pux (start, fromSimple, renderToDOM)

main = do
  app <- start
    { initialState: init
    , update: fromSimple update
    , view: view
    , inputs: []
    }

  renderToDOM "#app" app.html

