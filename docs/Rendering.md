# Rendering

To render a Pux application, create your application using `app` and combine it
with the appropriate render function.

## In the browser

`renderToDOM` calls `ReactDOM.render()` with the container element selected
using the given selector.

```purescript
import Pux (app)
import Pux.Render.DOM (renderToDOM)

main = renderToDOM "#app" =<< app
  { state: initialState
  , view: view
  , update: update
  -- | additional action signals to merge into input
  , inputs: []
  }
```

## On the server

`renderToHTML` calls `ReactDOMServer.renderToString()`.

```purescript
import Pux (app)
import Pux.Render.HTML (renderToHTML)

main = renderToHTML =<< app
  { state: initialState
  , view: view
  , update: update
  -- | additional action signals to merge into input
  , inputs: []
  }
```
