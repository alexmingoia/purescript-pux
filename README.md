<div align="center">
<h1>PUX</h1>
<p align="center">
<em>Build purely functional type-safe web applications with PureScript.</em>
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
`Event -> State -> (State, HTML)` which is run for every event. Pux
provides the necessary tooling to build modern web applications, including:

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
