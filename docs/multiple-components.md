## Multiple components

### A pair of counters

> [Example code](https://github.com/alexmingoia/purescript-pux/tree/master/examples/pair-of-counters/)

We can reuse components without knowing anything about their implementation
details, only relying on their exposed `Action`, `State`, `view`, and `update`
functions. Components may also expose an `init` function which returns an
initial state.

To demonstrate multiple components, we'll build an application with two
counters using the counter component we built in the previous section,
[A basic counter](/components.html#a-basic-counter).

We'll start as we did before by defining a type for application state. In this
case, we have two counts to keep track of:

```purescript
type State =
  { topCount :: Counter.State
  , bottomCount :: Counter.State }
```

Next we'll describe our set of actions:

```purescript
data Action
  = Top (Counter.Action)
  | Bottom (Counter.Action)
  | Reset
```

We don't need to know anything about the action types for child components or
their particular constructors, we just need to route them to the appropriate
update function:

```purescript
update :: Action -> State -> State
update (Top action) state =
  state { topCount = Counter.update action state.topCount }

update (Bottom action) state =
  state { bottomCount = Counter.update action state.bottomCount }

update Reset state = state { topCount = 0, bottomCount = 0 }
```

As you can see, the internals of components are completely opaque to their
parents, while at the same time allowing parent components to respond to
child actions.

Finally, we create a view function that combines both counters along with a
reset button. `map` is used to forward child `Html` actions to their parent
action by mapping over `Html` that sends actions of type `a` to return `Html`
that sends actions of type `b`: `(a -> b) -> Html a -> Html b`.

```purescript
view :: State -> Html Action
view state =
  div
    []
    [ map Top $ Counter.view state.topCount
    , map Bottom $ Counter.view state.bottomCount
    , button [ onClick (const Reset) ] [ text "Reset" ]
    ]
```

That's it! Composing components in this manner is straightfoward, modular, and
infinitely nestable. Each layer of components only needs to know about its
immediate children one layer below, and child components don't need to know
anything about their parents.

Despite the simplicity of this approach, one drawback is that parent
components must delegate communication between child components. A child has
no way of directly handling actions of another child without going through its
parent. In practice, this may add a small amount of "glue" code, *but it's a
small price to pay for straightfoward and modular components.*
