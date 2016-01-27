# Pux

[![Latest release](http://img.shields.io/bower/v/purescript-pux.svg)](https://github.com/alexmingoia/purescript-pux/releases)
[![Build Status](https://travis-ci.org/alexmingoia/purescript-pux.svg?branch=master)](https://travis-ci.org/alexmingoia/purescript-pux)
[![Gitter Chat](https://img.shields.io/gitter/room/gitterHQ/gitter.svg)](https://gitter.im/alexmingoia/purescript-pux)

A PureScript FRP interface to React.

- Build React UIs as a fold of actions to state via [`purescript-signal`](https://github.com/bodil/purescript-signal/)
- Type-safe routing
- Server-side rendering
- Hot-reloading of components

---

- [Guide](http://alexmingoia.github.io/purescript-pux)
- [API Reference](http://alexmingoia.github.io/purescript-pux/docs/API/Pux.html)
- Examples:
  - [Basic counter](https://github.com/alexmingoia/purescript-pux/tree/master/examples/basic/)
  - [AJAX](https://github.com/alexmingoia/purescript-pux/tree/master/examples/ajax/)
  - [Routing](https://github.com/alexmingoia/purescript-pux/tree/master/examples/routing/)

## Introduction

Pux is a simple FRP interface for building web applications with React,
inspired by the
[Elm app architecture](https://github.com/evancz/elm-architecture-tutorial)
and [Flux](https://facebook.github.io/flux/). Views are produced from a global
state atom, which is updated by a function that folds actions from input.
Rendering is handled by React, and PureScript provides functional type-safe
application code.

Pux applications consist of:

- A type for application state.
- A type for actions taken by the user and sent to `Input`.
- A `View` function, which produces HTML from the current state.
- An `Update` function, which produces a new state from the signal of actions.

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

main = renderToDOM "#app" =<< app
  { state: initialState
  , update: update
  , view: view
  , inputs: []
  }
```
