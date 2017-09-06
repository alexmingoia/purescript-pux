<div align="center">
<h1>PUX</h1>
<p align="center">
<em>Build type-safe web applications with PureScript.</em>
</p>
<a href="https://www.purescript-pux.org">Documentation</a>
| <a href="https://github.com/alexmingoia/purescript-pux/tree/master/examples/">Examples</a>
| <a href="https://gitter.im/alexmingoia/purescript-pux">Chat</a>
</div>

<hr />

[![Latest Release](http://img.shields.io/github/release/alexmingoia/purescript-pux.svg)](https://pursuit.purescript.org/packages/purescript-pux)
[![ComVer](https://img.shields.io/badge/comver-compliant-brightgreen.svg)](https://github.com/staltz/comver)
[![Build Status](https://travis-ci.org/alexmingoia/purescript-pux.svg?branch=master)](https://travis-ci.org/alexmingoia/purescript-pux)
[![Gitter Chat](https://img.shields.io/gitter/room/gitterHQ/gitter.svg)](https://gitter.im/alexmingoia/purescript-pux)

Pux is a PureScript library for building web applications. Interactive
UI is modeled as a single state transition function,
`Event -> State -> (State, HTML)` which is run for every event. Pux also
provides tooling such as:

- Isomorphic routing and rendering
- Hot reloading
- Render to React (or any virtual DOM library)
- Time-travelling debug extension

### Quick start

The [starter app](http://github.com/alexmingoia/pux-starter-app) provides
everything you need to get started:

```sh
git clone git://github.com/alexmingoia/pux-starter-app.git my-awesome-pux-app
cd my-awesome-pux-app
npm install
npm start
```

### Example

The following chunk of code sets up a basic counter that can be incremented and
decremented:

```purescript
module Main where

import Prelude hiding (div)
import Control.Monad.Eff (Eff)
import Pux (CoreEffects, EffModel, start)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)
import Text.Smolder.HTML (button, div, span)
import Text.Smolder.Markup (text, (#!))

data Event = Increment | Decrement

type State = Int

-- | Return a new state (and effects) from each event
foldp :: ∀ fx. Event -> State -> EffModel State Event fx
foldp Increment n = { state: n + 1, effects: [] }
foldp Decrement n = { state: n - 1, effects: [] }

-- | Return markup from the state
view :: State -> HTML Event
view count =
  div do
    button #! onClick (const Increment) $ text "Increment"
    span $ text (show count)
    button #! onClick (const Decrement) $ text "Decrement"

-- | Start and render the app
main :: ∀ fx. Eff (CoreEffects fx) Unit
main = do
  app <- start
    { initialState: 0
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input
```

## Benchmarks

![Table of benchmarks comparing rendering speed of similar libraries](https://cdn-images-1.medium.com/max/1600/1*6EjJTf1mhlTxd4QWsygCwA.png)

### Why is Pux slow?

Pux has not focused on performance yet. The slow performance arises from
translating Pux's (smolder) virtual DOM to React's virtual DOM. The goal is to
write a purescript virtual DOM module for smolder, which would avoid that
translation step and could be optimized for a monadic datastructure. I suspect
this would achieve performance on par with Halogen.

Below are the render steps for the other libraries compared, which shows that
Pux is the only one that has an intermediate virtual DOM representation (it has
to render to React first then React has to render):

Elm = Virtual DOM -> DOM patch
React = Virtual DOM -> DOM patch
Thermite = Virtual DOM -> DOM patch
Halogen = Virtual DOM -> DOM patch
Pux = Smolder Markup -> React Virtual DOM -> DOM patch
