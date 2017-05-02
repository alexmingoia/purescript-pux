# Rendering

## Rendering with React

Create an application by providing `initialState`, `view`, and `foldp`
to Pux's [`start`](https://pursuit.purescript.org/packages/purescript-pux/9.0.0/docs/Pux#v:start):

```purescript
main :: ∀ fx. Eff (CoreEffects fx) Unit
main = do
  app <- start
    { initialState: 0
    , view
    , foldp
    , inputs: []
    }
```

The resulting application is a record with reactive values
([Signals](https://pursuit.purescript.org/packages/purescript-signal)) for events,
state, and markup:

```purescript
type App e ev st =
  { markup :: Signal (Markup e)
  , state  :: Signal st
  , events :: Signal (List ev)
  , input  :: Channel (List ev)
  }
```

Render in the browser by passing the markup and event signal to
[`renderToDOM`](https://pursuit.purescript.org/packages/purescript-pux/9.0.0/docs/Pux.Renderer.React#v:renderToDOM)
from Pux.Renderer.React:

```purescript
  renderToDOM "#app" app.markup app.input
```

Render to a string using
[`renderToString`](https://pursuit.purescript.org/packages/purescript-pux/9.0.0/docs/Pux.Renderer.React#v:renderToString):

```purescript
  html <- renderToString app.markup
```

## Other renderers

Rendering to other virtual DOM libraries can be accomplished by writing a
parser for purescript-smolder's `Markup (a -> b)`, using the React renderer as a
reference.

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
data to load before rendering. Pux provides [`waitState`](https://pursuit.purescript.org/packages/purescript-pux/9.0.0/docs/Pux#v:waitState)
which blocks the monadic context until the provided test function returns true.

> #### Next: [Components](/docs/components)
> #### Previous: [Markup](/docs/markup)
