# Architecture

A Pux application consists of two types and two functions:

1. A type for the app's **State**.
2. A type for **Event**s such as the user clicking a button.
3. A function which produces a new state from events, **`foldp`**.
4. A function which produces HTML from the current state, **`view`**.

For every event, `foldp` produces a new state which is used by the `view`
function to produce HTML to render.

### 1. State

A type for the application's state. For example, the state of a simple counter
that can be incremented or decremented is just an int, but for most web apps the
state will be more complex:

```purescript
type State = Int
```

### 2. Events

Pux listens for DOM events and reifies them using the application's event type.
For example, a simple counter needs events for incrementing and decrementing in
response to clicking buttons:

```purescript
data Event = Increment | Decrement
```

### 3. Updating state with `foldp`

Whenever an event occurs a new state is produced by folding it with the current
state using [`foldp`](/docs/events#Folding_over_the_past_with_foldp). Continuing
the counter example, the previous count is combined with the current event to
produce a new count:

```purescript
foldp :: ∀ fx. Event -> State -> EffModel State Event fx
foldp Increment n = { state: n + 1, effects: [] }
foldp Decrement n = { state: n - 1, effects: [] }
```

### 4. Viewing state

The view function takes state and returns the corresponding HTML.

```purescript
view :: State -> HTML Event
view count =
  div do
    button #! onClick (const Increment) $ text "Increment"
    span $ text (show count)
    button #! onClick (const Decrement) $ text "Decrement"
```

> #### Next: [Events](/docs/events)
