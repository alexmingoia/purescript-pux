# Using Pux components in React

Pux components can be rendered to a React class instead of the DOM, for use
inside an existing React application.

Use `Pux.start` to initialize your component with state, then use `Pux.toReact`
to return a React class:

```purescript
module Counter where

import Control.Monad.Eff (Eff)
import Prelude ((+), (-), bind, const, show)
import Pux as Pux
import Pux (CoreEffects)
import React (ReactClass)
import Pux.Html (Html, div, span, button, text)
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


toReact :: forall props. State -> Eff CoreEffects (ReactClass props)
toReact state = do
  comp <- Pux.start
    { initialState: state
    , update: fromSimple update
    , view: view
    , inputs: []
    }

  Pux.toReact comp.html
```

After your PureScript has been compiled, call this module's `toReact` method to
return your class:

```javascript
const Counter = PS.Counter.toReact(state)()
```
