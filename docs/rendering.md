# Rendering

## Rendering with React

First create an application by providing initial state and view and foldp
functions to Pux's [`start`](https://pursuit.purescript.org/packages/purescript-pux/8.0.0/docs/Pux#v:start):

```purescript
main :: ∀ fx. Eff (CoreEffects fx) Unit
main = do
  app <- start
    { initialState: State 0
    , view
    , foldp
    , inputs: []
    }
```

The resulting application is a record with reactive values
([Signals](https://pursuit.purescript.org/packages/purescript-signal)) for events,
state, and markup.

Render in the browser by passing the markup signal and event channel to
[`renderToDOM`](https://pursuit.purescript.org/packages/purescript-pux/8.0.0/docs/Pux.Renderer.React#v:renderToDOM)
from Pux.Renderer.React:

```purescript
  renderToDOM "#app" app.markup app.input
```

Render to a string using
[`renderToString`](https://pursuit.purescript.org/packages/purescript-pux/8.0.0/docs/Pux.Renderer.React#v:renderToString):

```purescript
  html <- renderToString app.markup
```

### Other renderers

Rendering with other virtual DOM libraries can be accomplished by writing a
parser for `HTML`, using the React renderer as a reference.

## Isomorphic and server-side rendering

The Pux [starter app](https://github.com/alexmingoia/pux-starter-app) provides a
complete isomorphic configuration including sharing state, routes and other code
between client and server. 

### Server-side rendering

[purescript-express](https://github.com/dancingrobot84/purescript-express)
and `renderToString` can be used to render the app on the
server.

```purescript
renderApp :: ∀ fx. Handler (channel :: CHANNEL | fx)
renderApp = do
  url <- getOriginalUrl

  app <- liftEff $ start
    { initialState: App.init url
    , view: App.view
    , foldp: App.foldp
    , inputs: [constant (PageView (match url))]
    }

  html <- renderToString app.markup

  send html
```

### Waiting for events and data to load before rendering

When loading data asynchronously on the server you may want to wait for the
data to load before rendering. Pux provides [`waitState`](https://pursuit.purescript.org/packages/purescript-pux/8.0.0/docs/Pux#v:waitState)
which blocks the monadic context until the provided test function returns true.

> #### Next: [Forms](/docs/forms)
> #### Previous: [Markup](/docs/markup)
