## Module Pux

#### `App`

``` purescript
type App e ev st = { markup :: Signal (Markup e), state :: Signal st, events :: Signal (List ev), input :: Channel (List ev) }
```

An `App` is a record consisting of:

* `markup` – A signal of `Markup e` representing the current view of the
  app. This is consumed by renderers.

* `state` – A signal representing the application's current state.

* `input` – A channel representing the application's event input.

#### `Config`

``` purescript
type Config e ev st = { initialState :: st, view :: st -> Markup e, foldp :: FoldP st ev, inputs :: Array (Signal ev) }
```

The configuration of an app consists of foldp and view functions along
with an initial state. The `foldp` and `view` functions describe how to
step the state and view | the state.

The `inputs` array is for any external inputs you might need. These will
be merged into the app's input signal.

#### `FoldP`

``` purescript
type FoldP st ev = ev -> st -> EffModel st ev
```

Return an `EffModel` from the current event and state.

#### `EffModel`

``` purescript
type EffModel st ev = { state :: st, effects :: Array (Aff (Maybe ev)) }
```

`EffModel` is a container for state and asynchronous effects which return
an event.

#### `noEffects`

``` purescript
noEffects :: forall st ev. st -> EffModel st ev
```

Create an `EffModel` with no effects from a given state.

#### `onlyEffects`

``` purescript
onlyEffects :: forall st ev. st -> Array (Aff (Maybe ev)) -> EffModel st ev
```

#### `mapState`

``` purescript
mapState :: forall a b ev. (a -> b) -> EffModel a ev -> EffModel b ev
```

Map over the state of an `EffModel`.

#### `mapEffects`

``` purescript
mapEffects :: forall a b st. (a -> b) -> EffModel st a -> EffModel st b
```

Map over the effects of an `EffModel`.

#### `start`

``` purescript
start :: forall e ev st. Config e ev st -> Effect (App e ev st)
```

Create an application, which exposes a markup signal that can be used by
renderers.

```purescript
main = do
  app <- start
   { initialState
   , view
   , foldp
   , inputs: [] }

  renderToDOM "#app" app.markup app.input
```

#### `waitEvent`

``` purescript
waitEvent :: forall e ev st. (ev -> Boolean) -> App e ev st -> Aff st
```

Wait for a specific event until returning the app state.

#### `waitState`

``` purescript
waitState :: forall e ev st. (st -> Boolean) -> App e ev st -> Aff st
```

Wait for a specific state before returning the app state.


