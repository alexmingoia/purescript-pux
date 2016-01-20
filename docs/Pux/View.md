## Module Pux.View

#### `View`

``` purescript
type View state = state -> VDom -> VDom
```

`View` is a rendering function that receives state, children, and returns
a `VDom`. `VDom` is a monadic DSL for constructing React virtual DOM using
`do` notation:

```purescript
import Pux (View())
import Pux.DOM.HTML.Elements (div, p, button, text)
import Pux.DOM.HTML.Attributes (onClick, send)

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
onClick (send Increment <> preventDefault <> stopPropagation)
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
`Pux.DOM.HTML.Attributes` type signatures.

#### `VDomM`

``` purescript
data VDomM a
  = Node String (Maybe VDom) (Array Attr) (Array MakeHandler) (VDomM a)
  | Content String (VDomM a)
  | Return a
```

##### Instances
``` purescript
Semigroup (VDomM a)
Monoid (VDomM Unit)
Functor VDomM
Apply VDomM
Applicative VDomM
Bind VDomM
Monad VDomM
Attributable (VDomM a)
Attributable (VDomM a -> VDomM a)
```

#### `VDom`

``` purescript
type VDom = VDomM Unit
```

#### `Attrs`

``` purescript
data Attrs
  = Attrs (Array Attr) (Array MakeHandler)
```

##### Instances
``` purescript
Semigroup Attrs
Monoid Attrs
```

#### `EventOpt`

``` purescript
data EventOpt
  = PreventDefault
  | StopPropagation
```

#### `Handler`

``` purescript
data Handler ev action eff
  = Handler (List action) (List (ev -> Eff eff Unit))
```

A handler is a collection of actions and effects.

##### Instances
``` purescript
Semigroup (Handler ev action eff)
```

#### `Attributable`

``` purescript
class Attributable a where
  with :: a -> Attrs -> a
```

##### Instances
``` purescript
Attributable (VDomM a)
Attributable (VDomM a -> VDomM a)
```

#### `(!)`

``` purescript
(!) :: forall a. (Attributable a) => a -> Attrs -> a
```

_left-associative / precedence 4_


