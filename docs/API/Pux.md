## Module Pux

#### `start`

``` purescript
start :: forall state action eff. Config state action eff -> Eff (CoreEffects eff) (App state action)
```

Start an application. The resulting html signal is fed into `renderToDOM`.

```purescript
main = do
  app <- start
    { update: update
    , view: map view
    , initialState: initialState
    , inputs: [] }

  renderToDOM "#app" app.html
```

#### `Config`

``` purescript
type Config state action eff = { update :: Update state action eff, view :: Signal state -> Signal (Html action), initialState :: state, inputs :: Array (Signal action) }
```

The configuration of an app consists of update and view functions along
with an initial state.

The `update` and `view` functions describe how to step the state and view
the state.

The `inputs` array is for any external signals you might need. These will
be merged into the app's input signal.

#### `CoreEffects`

``` purescript
type CoreEffects eff = (channel :: CHANNEL, err :: EXCEPTION | eff)
```

The set of effects every Pux app needs to allow through when using `start`.
Extend this type with your own app's effects, for example:

```purescript
type AppEffects = (console :: CONSOLE, dom :: DOM)

main :: State -> Eff (CoreEffects AppEffects) (App State Action)
main state = do
  -- ...
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

#### `onlyEffects`

``` purescript
onlyEffects :: forall state action eff. state -> Array (Aff (channel :: CHANNEL | eff) action) -> EffModel state action eff
```

#### `mapState`

``` purescript
mapState :: forall sa sb a e. (sa -> sb) -> EffModel sa a e -> EffModel sb a e
```

Map over the state of an `EffModel`.

#### `mapEffects`

``` purescript
mapEffects :: forall s a b e. (a -> b) -> EffModel s a e -> EffModel s b e
```

Map over the effectful actions of an `EffModel`.

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
toReact :: forall a props eff. Signal (Html a) -> Eff eff (ReactClass props)
```

Return a ReactClass from a Pux component's html signal.


