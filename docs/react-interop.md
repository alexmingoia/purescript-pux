# Interoperate with React

## Using Pux components in React

Pux components can be rendered to a React class instead of the DOM for use
inside an existing React application.

Use `start` to initialize your component with state, then use
[`renderToReact`](https://pursuit.purescript.org/packages/purescript-pux/9.0.0/docs/Pux.Renderer.React#v:renderToReact)
to return a React class.

```purescript
module FancyComponent where

-- State, Event, foldp, view functions...

toReact :: ∀ props fx. State -> Eff (CoreEffects fx) (ReactClass props)
toReact state = do
  app <- start
    { initialState: state
    , view
    , foldp
    , inputs: []
    }

  renderToReact app.markup app.input
```

After your PureScript has been compiled, call this module's `toReact` method to
return your class:

```javascript
const FancyComponent = PS.FancyComponent.toReact(state)()
```

## Using React components in Pux

Expose your React component by creating a `.js` and `.purs` file of the same
name, and using the PureScript FFI to bind them:

```javascript
const React = require('react')

exports.fancyComponent = React.createClass(..)
```

In your PureScript code, add a foreign module definition for the
fancyComponent, and use the
[`reactClass`](https://pursuit.purescript.org/packages/purescript-pux/9.0.0/docs/Pux.Renderer.React#v:reactClass)
function to create an HTML element constructor which you can use in your views.

```purescript
-- | src/FancyComponent.purs

module FancyComponent where

import React (ReactClass)

foreign import fancyClass :: ∀ props. ReactClass props

fancy :: ∀ ev. HTML ev
fancy = reactClass fancyClass "fancy"
```

You can then use `fancy` in your views:

```purescript
view count =
  div
    button #! onClick (const Increment) $ text "Increment"
    fancy $ text ("Fancy " <> (show count))
    button #! onClick (const Decrement) $ text "Decrement" 
```

> To pass arbitrary props to a react class use [`reactClassWithProps`](https://pursuit.purescript.org/packages/purescript-pux/9.0.0/docs/Pux.Renderer.React#v:reactClassWithProps).
