## Module Pux

#### `start`

``` purescript
start :: forall state action eff. Config state action eff -> Eff (err :: EXCEPTION, channel :: CHANNEL | eff) (App state action)
```

Start an application. The resulting html signal is fed into `renderToDOM`.

```purescript
main = do
  app <- start
    { update: update
    , view: view
    , initialState: initialState
    , inputs: [] }

  renderToDOM "#app" app.html
```

#### `App`

``` purescript
type App state action = { html :: Signal (Html action), state :: Signal state }
```

An `App` consists of three signals:

* `html` – A signal of `Html` representing the current view of your
  app. This should be fed into `renderToDOM`.

* `state` – A signal representing the application's current state.

#### `Update`

``` purescript
type Update state action eff = action -> state -> EffModel state action eff
```

Synonym for an update function that returns state and an array of
asynchronous effects that return an action.

#### `EffModel`

``` purescript
type EffModel state action eff = { state :: state, effects :: Array (Aff (channel :: CHANNEL | eff) action) }
```

`EffModel` is a container for state and a collection of asynchronous
effects which return an action.

#### `fromSimple`

``` purescript
fromSimple :: forall s a eff. (a -> s -> s) -> Update s a eff
```

Create an `Update` function from a simple step function.

#### `noEffects`

``` purescript
noEffects :: forall state action eff. state -> EffModel state action eff
```

Create an `EffModel` with no effects from a given state.

#### `renderToDOM`

``` purescript
renderToDOM :: forall a eff. String -> Signal (Html a) -> Eff eff Unit
```

#### `renderToString`

``` purescript
renderToString :: forall a eff. Signal (Html a) -> Eff eff String
```

#### `toReact`

``` purescript
toReact :: forall a eff. Signal (Html a) -> Eff eff ReactClass
```

Return a React class from a Pux component's html signal.

#### `ReactClass`

``` purescript
data ReactClass :: *
```


