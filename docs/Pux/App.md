## Module Pux.App

#### `app`

``` purescript
app :: forall eff state action. Config eff state action -> Eff (chan :: Chan, dom :: DOM | eff) ReactClass
```

Initialize a Pux application.

```purescript
main = renderToDOM "#app" =<< app
  { state: initialState
  , view: view
  , update: update
  -- | additional action signals to merge into input
  , inputs: []
  }
```

#### `Config`

``` purescript
type Config eff state action = { state :: state, view :: View state, update :: Update eff state action, inputs :: Array (Signal action) }
```

#### `Input`

``` purescript
type Input action = Channel (List action)
```

`Input` is a channel which receives actions from the `View`.

#### `Update`

``` purescript
type Update eff state action = action -> state -> Input action -> EffModel eff state
```

`Update` receives actions from `Input`, the current state, the input
channel (for asynchronous state changes), and returns a new state and
collection of effects to run.

```purescript
update :: forall eff. Update (console :: CONSOLE | eff) State Action
update action state input =
  case action of
    Increment ->
      { state: state { counter = state.counter + 1 }
      , effects: [ do log "increment" ] }
    Decrement ->
      { state: state { counter = state.counter - 1 }
      , effects: [ do log "decrement" ] }
```

#### `EffModel`

``` purescript
type EffModel eff state = { state :: state, effects :: Array (Eff (dom :: DOM, chan :: Chan | eff) Unit) }
```

`EffModel` is a container for state and an associated collection of effects
returned by the `Update` function.


