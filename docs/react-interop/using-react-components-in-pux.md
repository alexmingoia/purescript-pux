# Using React components in Pux

Expose a PureScript element by using `fromReact` in your JavaScript module:

```javascript
// module FancyComponent

const React = require('react')
const Pux = require('purescript-pux')

const FancyComponent = React.createClass(..)

exports.fromReact = Pux.fromReact(FancyComponent)
```

> The comment `// module FancyComponent` is required to declare this module as a
> PureScript foreign module.

In your PureScript code, add a foreign module definition for the `fromReact`
function:

```purescript
module FancyComponent where

import Pux.Html (Html, Attribute)

foreign import fromReact :: forall a.
                            Array (Attribute a) ->
                            Array (Html a) ->
                            Html a
```

Then embed the component in your view like so:

```purescript
view count =
  div
    []
    [ button [ onClick (const Increment) ] [ text "Increment" ]
    , FancyComponent.fromReact [] [ text ("Fancy " ++ (show count)) ]
    , button [ onClick (const Decrement) ] [ text "Decrement" ]
    ]
```

If you need to pass initial props that do not have constructor functions from
`Pux.Html.Attributes` then use the
[`attr`](../API/Pux/Html/Attributes.html#attr) function:

```purescript
FancyComponent.fromReact
  [ attr "customProp" true ]
  [ text ("Fancy " ++ (show count)) ]
```
