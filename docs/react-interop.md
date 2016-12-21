# Interoperate with React

## Using Pux components in React

Pux components can be rendered to a React class instead of the DOM for use
inside an existing React application.

Use `start` to initialize your component with state, then use
[`renderToReact`](/docs/API/Pux/Renderer/React#renderToReact) to return a React
class.

```purescript
module FancyComponent where

-- State, Event, update, view functions...

toReact :: ∀ props fx. Eff (CoreEffects fx) (ReactClass props)
toReact state = do
  app <- start
    { initialState: state
    , view: view
    , update: update
    , inputs: []
    }

  pure $ renderToReact app.markup app.input
```

After your PureScript has been compiled, call this module's `toReact` method to
return your class:

```javascript
const FancyComponent = PS.FancyComponent.toReact(state)()
```

## Using React components in Pux

Expose your React component by creating a `.js` and `.purs` file of the same
name, and using the PureScript FFI to bind them:

src/FancyComponent.js :

```javascript
const React = require('react')

exports.fancyComponent = React.createClass(..)
```

In your PureScript code, add a foreign module definition for the
`fancyComponent` function:

src/FancyComponent.purs:

```purescript
module FancyComponent where

import React (ReactClass)

foreign import fancyComponent :: ∀ props. ReactClass props
```

To use this component in your view use the
[`reactClass`](/docs/API/Pux/Renderer/React#reactClass) function to create an
HTML element constructor which you can use in your views.

```purescript
view count =
  div
    button #! onClick (const Increment) $ text "Increment"
    reactClass "fancy" fancyComponent $ text ("Fancy " <> (show count))
    button #! onClick (const Decrement) $ text "Decrement" 
```

`reactClass` takes a unique key which is used to replace the element with the
React class during rendering.
