## Module Pux

#### `start`

``` purescript
start :: forall e ev st fx. Config e ev st fx -> Eff (CoreEffects fx) (App e ev st)
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

#### `Config`

``` purescript
type Config e ev st fx = { "initialState" :: st, "view" :: st -> Markup e, "foldp" :: FoldP st ev fx, "inputs" :: Array (Signal ev) }
```

The configuration of an app consists of foldp and view functions along
with an initial state. The `foldp` and `view` functions describe how to
step the state and view | the state.

The `inputs` array is for any external inputs you might need. These will
be merged into the app's input signal.

#### `CoreEffects`

``` purescript
type CoreEffects fx = ("channel" :: CHANNEL, "err" :: EXCEPTION | fx)
```

The set of effects every Pux app needs to allow through when using `start`.
Extend this type with your own app's effects, for example:

```purescript
type AppEffects = (console :: CONSOLE, dom :: DOM)

main :: State -> Eff (CoreEffects AppEffects) (App DOMEvent State Event)
main state = do
  -- ...
```

#### `App`

``` purescript
type App e ev st = { "markup" :: Signal (Markup e), "state" :: Signal st, "events" :: Signal (List ev), "input" :: Channel (List ev) }
```

An `App` is a record consisting of:

* `markup` – A signal of `Markup e` representing the current view of the
  app. This is consumed by renderers.

* `state` – A signal representing the application's current state.

* `input` – A channel representing the application's event input.

#### `FoldP`

``` purescript
type FoldP st ev fx = ev -> st -> EffModel st ev fx
```

Return an `EffModel` from the current event and state.

#### `EffModel`

``` purescript
type EffModel st ev fx = { "state" :: st, "effects" :: Array (Aff (CoreEffects fx) (Maybe ev)) }
```

`EffModel` is a container for state and asynchronous effects which return
an event.

#### `noEffects`

``` purescript
noEffects :: forall st ev fx. st -> EffModel st ev fx
```

Create an `EffModel` with no effects from a given state.

#### `onlyEffects`

``` purescript
onlyEffects :: forall st ev fx. st -> Array (Aff (CoreEffects fx) (Maybe ev)) -> EffModel st ev fx
```

#### `mapState`

``` purescript
mapState :: forall a b ev fx. (a -> b) -> EffModel a ev fx -> EffModel b ev fx
```

Map over the state of an `EffModel`.

#### `mapEffects`

``` purescript
mapEffects :: forall a b st fx. (a -> b) -> EffModel st a fx -> EffModel st b fx
```

Map over the effects of an `EffModel`.

#### `waitEvent`

``` purescript
waitEvent :: forall e ev st fx. (ev -> Boolean) -> App e ev st -> Aff fx st
```

Wait for a specific event until returning the app state.

#### `waitState`

``` purescript
waitState :: forall e ev st fx. (st -> Boolean) -> App e ev st -> Aff fx st
```

Wait for a specific state before returning the app state.


