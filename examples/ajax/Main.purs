module AjaxExample where

import AjaxExample.Todos (init, update, view)
import Prelude (bind, map)
import Pux (start, renderToDOM)

main = do
  app <- start
    { initialState: init
    , update: update
    , view: map view
    , inputs: [] }

  renderToDOM "#app" app.html
