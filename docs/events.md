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
foldp :: Event -> State -> EffModel State Event
foldp Increment n = { state: n + 1, effects: [] }
foldp Decrement n = { state: n - 1, effects: [] }
```

### The EffModel

foldp produces a new state along with effects. These are wrapped in a record
that Pux calls an
[`EffModel`](https://pursuit.purescript.org/packages/purescript-pux/9.0.0/docs/Pux#t:EffModel).

```purescript
type EffModel st ev =
  { state   :: st
  , effects :: Array (Aff (Maybe ev))
  }
```

### Effectful computations

Effectful computations returned by foldp are asynchronous and handled by the
`Aff` monad from
[purescript-aff](https://pursuit.purescript.org/packages/purescript-aff).  These
computations may return new events which are again handled by foldp.

For example, to log to the console when an event occurs we can use
a lifted `log`:

```purescript
liftedLog = liftEff <<< log

foldp :: Event -> State -> EffModel State Event (console :: CONSOLE)
foldp Increment count =
  { state: count + 1
  , effects: [ liftedLog "increment" *> pure Nothing ]
  }
```

Returning `Nothing` after logging denotes that the computation does not generate a new event.

Some effectful computations return a value and new event. For example, an HTTP
request to remotely load todos might have an event `RequestTodos`. When this
event occurs `foldp` returns an effect – the AJAX request – which returns a new
event `ReceiveTodos`:

```purescript
foldp :: Event -> State -> EffModel State Event
foldp (RequestTodos) st =
  { state: st { status = "Fetching todos..." }
  , effects: [ do
      res <- attempt $ get "http://jsonplaceholder.typicode.com/users/1/todos"
      let todos = either (Left <<< show) (decodeJson res.response :: Either String Todos)
      pure $ Just $ ReceiveTodos todos
    ]
  }

foldp (ReceiveTodos t) st =
  noEffects $ case t of
    Left err -> st { status = "Error fetching todos: " <> show err }
    Right todos -> st { todos = todos, status = "" }
```

### Nesting events

> Read the [Components](/docs/components) section to learn more
> about organizing your app.

Sometimes you want to compose foldp functions with different event and state
types when using external libraries or splitting your foldp function into
submodules. To do this, create a parent event type wrapping the child's type and
embed the child state in the parent state:

```purescript
-- | Wrap the child event type
data Event
  = PageView Route
  | ChildEvent Child.Event

-- | Extend the state with the child's state
type State =
  { route :: Route
  , child :: Child.State
  }
```

Whenever a child event occurs pass it to the child's foldp function, then map
over the returned EffModel using Pux's
[`mapEffects`](https://pursuit.purescript.org/packages/purescript-pux/9.0.0/docs/Pux#v:mapEffects)
and [`mapState`](https://pursuit.purescript.org/packages/purescript-pux/9.0.0/docs/Pux#v:mapState):

```purescript
foldp :: Event -> State -> EffModel State Event
foldp (PageView r) st = noEffects $ st { route = r }
foldp (ChildEvent e) st =
  Child.foldp e st.child
    # mapEffects ChildEvent
    # mapState \sb -> st { child = sb }
```

> #### Next: [Markup](/docs/markup)
> #### Previous: [Architecture](/docs/architecture)
