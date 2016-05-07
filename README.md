# Pux

[![Latest release](https://img.shields.io/bower/v/purescript-pux.svg)](https://github.com/alexmingoia/purescript-pux/releases)
[![Build Status](https://travis-ci.org/alexmingoia/purescript-pux.svg?branch=master)](https://travis-ci.org/alexmingoia/purescript-pux)
[![Gitter Chat](https://img.shields.io/gitter/room/gitterHQ/gitter.svg)](https://gitter.im/alexmingoia/purescript-pux)

Pux is an FRP interface to React, similar to the [Elm app
architecture](https://github.com/evancz/elm-architecture-tutorial). It is a
simple pattern for modular, nested components that are easy to test, refactor,
and debug - making it simple and straightforward to build complex web
applications.

- Build React UIs as a fold of actions to state.
- Type-safe routing
- Server-side rendering (isomorphic applications)
- Hot-reloading of components
- Easy interop with existing React components

---

- [Guide](http://alexmingoia.github.io/purescript-pux)
- [API Reference](http://alexmingoia.github.io/purescript-pux/docs/API/Pux.html)
- [Starter app](https://github.com/alexmingoia/pux-starter-app)

### Installation

The easiest way to get started is to clone the
[starter app](http://github.com/alexmingoia/pux-starter-app),
which includes a hot-reloading setup using webpack:

```sh
git clone git://github.com/alexmingoia/pux-starter-app.git example
cd example
npm install
npm start
```

Pux is also available as the bower package `purescript-pux`.

### Example

The following chunk of code sets up a basic counter that you can increment and
decrement:

```purescript
import Prelude (Unit, bind, const, show, (-), (+))

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Signal.Channel (CHANNEL)

import Pux (renderToDOM, fromSimple, start)
import Pux.Html (Html, text, button, span, div)
import Pux.Html.Events (onClick)

data Action = Increment | Decrement

type State = Int

update :: Action -> State -> State
update Increment count = count + 1
update Decrement count = count - 1

view :: State -> Html Action
view count =
  div
    []
    [ button [ onClick (const Increment) ] [ text "Increment" ]
    , span [] [ text (show count) ]
    , button [ onClick (const Decrement) ] [ text "Decrement" ]
    ]

main :: forall e. Eff (err :: EXCEPTION, channel :: CHANNEL | e) Unit
main = do
  app <- start
    { initialState: 0
    , update: fromSimple update
    , view: view
    , inputs: []
    }

  renderToDOM "#app" app.html
```
