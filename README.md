# Pux

[![Latest release](http://img.shields.io/bower/v/purescript-pux.svg)](https://github.com/alexmingoia/purescript-pux/releases)
[![Build Status](https://travis-ci.org/alexmingoia/purescript-pux.svg?branch=master)](https://travis-ci.org/alexmingoia/purescript-pux)
[![Gitter Chat](https://img.shields.io/gitter/room/gitterHQ/gitter.svg)](https://gitter.im/alexmingoia/purescript-pux)

A PureScript FRP interface to React.

- Build React UIs as a fold of actions to state via [`purescript-signal`](https://github.com/bodil/purescript-signal/)
- Type-safe routing
- Server-side rendering
- React 0.14 support

---

- [Getting Started](#getting-started)
- [Documentation](https://github.com/alexmingoia/purescript-pux/tree/master/docs/Pux.md)
- Examples:
  - [Basic counter](https://github.com/alexmingoia/purescript-pux/tree/master/examples/basic/)
  - [AJAX](https://github.com/alexmingoia/purescript-pux/tree/master/examples/ajax/)
  - [Routing](https://github.com/alexmingoia/purescript-pux/tree/master/examples/routing/)

## Installation

`bower install purescript-pux` or clone the boilerplate repository, which
installs a skeleton Pux app along with webpack and gulp tasks for live
reloading:

```sh
git clone git://github.com/alexmingoia/purescript-pux-boilerplate.git example
cd example
npm install
npm start
```

## Example

```purescript
data Action = Increment | Decrement

type State = { counter :: Int }

initialState :: State
initialState = { counter: 0 }

update :: forall eff. Update (console :: CONSOLE | eff) State Action
update action state input =
  case action of
    Increment ->
      { state: state { counter = state.counter + 1 }
      , effects: [ do log "increment" ] }
    Decrement ->
      { state: state { counter = state.counter - 1 }
      , effects: [ do log "decrement" ] }

view :: View State
view state children = div $ do
  p $ text ("Counter: " ++ show state.counter)
  p $ do
    button ! onClick (send Increment) $ text "Increment"
    button ! onClick (send Decrement) $ text "Decrement"

main = renderToDOM (ElementId "app") =<< app
  { state: initialState
  , update: update
  , view: view
  , inputs: []
  }
```

## Getting Started

Pux applications consist of:

- A type for application state.
- A type for actions taken by the user and sent to `Input`.
- A `View` function, which produces HTML from the current state.
- An `Update` function, which produces a new state from the signal of actions.

First we need to input some modules:

```purescript
import Prelude hiding (div)

import Control.Bind ((=<<))
import Control.Monad.Eff.Console (CONSOLE(), log)
import Pux
import Pux.DOM.HTML.Elements (div, p, button, text)
import Pux.DOM.HTML.Attributes (onClick, send)
import Pux.Render.DOM
```

### Model

We'll need a type describing the application state. In this example,
we'll be building a simple counter which can be incremented or decremented.

```purescript
type State = { counter :: Int }

initialState :: State
initialState = { counter: 0 }
```

We also need a type for actions received by the `Input` channel. Actions are
taken by the user, and may originate from mouse clicks, URL changes, etc.

```purescript
data Action = Increment | Decrement
```

### View

Now that we have our state and action types defined, we can create a view of
our application state.

`View` is a rendering function that receives state, children, and returns
a `VDom`. `VDom` is a monadic DSL for constructing React virtual DOM using
`do` notation:

```purescript
view :: View State
view state children = div $ do
  p $ text ("Counter: " ++ show state.counter)
  p $ do
    button ! onClick (send Increment) $ text "Increment"
    button ! onClick (send Decrement) $ text "Decrement"
```

The `!` operator combines elements with attributes.

`send` creates a handler that sends actions to input. There are also
handlers for preventing the default event behavior or its propagation,
which can be combined using append:

```purescript
onClick (send Increment <> preventDefault)
```

Handlers take either an action, or a function that constructs an action
from an event object. Some events, such as keyboard or mouse events,
provide an object that contains event information such as key pressed,
mouse position, etc.

For example, `onKeyUp` provides the `KeyboardEvent` object:

```purscript
data Action = KeyUp KeyboardEvent

type State = { lastKeyPressed :: String }

update action state input = case action of
  (KeyUp ev) ->
    { state: { lastKeyPressed: ev.key }
    , effects: []
    }

view state children = div $ do
  p $ "Last key pressed: " ++ state.lastKeyPressed
  input ! onKeyUp (send KeyUp) ! placeholder "Type something"
```

To learn which events provide extra action arguments, refer to the
[`Pux.DOM.HTML.Attributes`](docs/Pux/DOM/HTML/Attributes.md) type signatures.

### Update

Now that we have our action, state, and view, we need to update the state based
on the user's actions via the `Update` function.

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

### Rendering

Finally, we render our application to the DOM:

```purescript
main = renderToDOM (ElementId "app") =<< app
  { state: initialState
  , view: view
  , update: update
  -- | additional action signals to merge into input
  , inputs: []
  }
```

### Routing

See the [routing documentation](docs/Pux/Router.md) for information and
examples.

### AJAX

See the [ajax example](examples/ajax/).
