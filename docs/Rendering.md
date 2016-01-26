# Rendering

To render a Pux application, create your application using `app` and combine it
with the appropriate render function.

## In the browser

```purescript
import Control.Bind ((=<<))
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

```purescript
import Control.Bind ((=<<))
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
