# Events

Pux listens for DOM events and reifies them using the application's event type.
For example, a simple counter needs events for incrementing and decrementing in
response to clicking buttons:

```purescript
data Event = Increment | Decrement
```

When raw DOM events are needed, they can be wrapped by the application event
type:

```purescript
data Event = Increment DOMEvent | Decrement DOMEvent
```

`DOMEvent` from Pux.DOM.Events is a type alias for
[purescript-dom](https://pursuit.purescript.org/packages/purescript-dom)'s event
type.

## Folding over the past with foldp

Whenever an event occurs a new state is produced by folding it with the current
state. This function is called **foldp** and is analogous to foldl, but
instead of folding from left to right it folds from past to present. Continuing
the counter example, the previous count is combined with the current event to
produce a new count:

```purescript
foldp :: ∀ fx. Event -> State -> EffModel State Event fx
foldp Increment (State n) = { state: State (n + 1), effects: [] }
foldp Decrement (State n) = { state: State (n - 1), effects: [] }
```

### The EffModel

foldp produces a new state along with effects. These are wrapped in a record
that Pux calls an
[`EffModel`](https://pursuit.purescript.org/packages/purescript-pux/8.0.0/docs/Pux#t:EffModel).

```purescript
type EffModel st ev fx =
  { state   :: st
  , effects :: Array (Aff (CoreEffects fx) (Maybe ev))
  }
```

### Effectful computations

Effectful computations returned by foldp are asynchronous and handled by the
`Aff` monad from
[purescript-aff](https://pursuit.purescript.org/packages/purescript-aff).  These
computations may return new events which are again handled by foldp.

For example, to log to the console when an event occurs we can use
purescript-aff's `log`:

```purescript
foldp :: Event -> State -> EffModel State Event (console :: CONSOLE)
foldp Increment (State count) =
  { state: State (count + 1)
  , effects: [ log "increment" *> pure Nothing ]
  }
```

Returning `Nothing` after logging denotes that the computation does not generate a new event.

Some effectful computations return a value and new event. For example, an HTTP
request to remotely load todos might have an event `RequestTodos`. When this
event occurs `foldp` returns an effect – the AJAX request – which returns a new
event `ReceiveTodos`:

```purescript
foldp :: Event -> State -> EffModel State Event (ajax :: AJAX)
foldp (RequestTodos) (State s) =
  { state: state { status = "Fetching todos..." }
  , effects: [ do
      res <- attempt $ get "http://jsonplaceholder.typicode.com/users/1/todos"
      let todos = either (Left <<< show) (decodeJson r.response :: Either String Todos)
      pure $ Just $ ReceiveTodos todos
    ]
  }

foldp (ReceiveTodos t) (State s) =
  noEffects $ State $ case t of
    Left err -> s { status = "Error fetching todos: " <> show err }
    Right todos -> s { todos = todos, status = "" }
```

### Nesting events

Sometimes you want to compose foldp functions with different event and state
types. To do this, create a parent event type wrapping the child's type and
embed the child state in the parent state:

```purescript
-- | Wrap the child event type
data Event
  = PageView Route
  | ChildEvent Child.Event

-- | Extend the state with the child's state
data State = State
  { route :: Route
  , child :: Child.State
  }
```

Whenever a child event occurs pass it to the child's foldp function, then map
over the returned EffModel using Pux's
[`mapEffects`](https://pursuit.purescript.org/packages/purescript-pux/8.0.0/docs/Pux#v:mapEffects)
and [`mapState`](https://pursuit.purescript.org/packages/purescript-pux/8.0.0/docs/Pux#v:mapState):

```purescript
foldp :: ∀ fx. Event -> State -> EffModel State Event fx
foldp (PageView r) (State s) = noEffects $ State s { route = r }
foldp (ChildEvent e) (State s) =
  Child.foldp e s.child
    # mapEffects ChildEvent
    # mapState \sb -> State s { child = sb }
```

> #### Next: [Markup](/docs/markup)
> #### Previous: [Architecture](/docs/architecture)
