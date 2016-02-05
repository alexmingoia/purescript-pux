# Sending Actions

## `send` and other handlers

Actions are created by delegating handlers to event attributes:

```purescript
button ! onClick (send (DeleteTodo todo.id)) $ text "delete"
```

`send` creates a handler that sends actions to input. There are also
other handlers for preventing the default event behavior or its propagation,
which can be combined using append:

```purescript
a ! onClick (send (DeleteTodo todo.id) <> preventDefault) $ text "delete"
```

## Actions with event data

Handlers take either an action, or a function that constructs an action
from an event object. Some events, such as keyboard or mouse events,
provide an object which contains event information.

For example, `onKeyUp` provides the `KeyboardEvent` object, which contains
information about the key pressed:

```purescript
data Action = KeyUp KeyboardEvent

type State = { lastKeyPressed :: String }

update action state input = case action of
  (KeyUp ev) ->
    { state: { lastKeyPressed: ev.key }
    , effects: []
    }

view state = div $ do
  p $ "Last key pressed: " ++ state.lastKeyPressed
  input ! onKeyUp (send KeyUp) ! placeholder "Type something"
```

To learn which events provide extra action arguments, refer to the
[`Pux.DOM.HTML.Attributes`](../API/Pux/DOM/HTML/Attributes.md) type signatures.
