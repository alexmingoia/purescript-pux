# Components

## A basic counter

> [Example code](https://github.com/alexmingoia/purescript-pux/tree/master/examples/basic-counter/)

A Pux component is a module that exports four parts:

- A type that represents actions taken by the user.
- A type that represents the component's state.
- An update function which produces a new state from actions.
- A view function which produces HTML from the current state.

```purescript
data Action = Increment | Decrement

type State :: Int

update :: Action -> State -> State

view :: State -> Html Action
```

We'll explain each of these parts by building an application out of a simple
counter component. The counter will display a count, along with buttons to
increment and decrement it.

First, we define a type for the state of our application.
In a complex web app our state would be modeled using a record type, but
because our counter component only needs to keep the current count we can
simply create a type alias for `Int`:

```purescript
type State = Int
```

Every appliction or component must provide an update function, which produces a
new state from actions taken by the user. In our case, we need to define
actions that represent incrementing and decrementing the counter:

```purescript
data Action = Increment | Decrement
```

Using a
[union type](https://leanpub.com/purescript/read#leanpub-auto-algebraic-data-types)
clearly describes the available actions, and if necessary can also encapsulate
data associated with an action. We'll see how actions can carry data in a later
section.

Next, we define an update function which produces a new state in response to
user actions. The logic for our counter is very simple:

```purescript
update :: Action -> State -> State
update Increment count = count + 1
update Decrement count = count - 1
```

> You'll notice that the `update` function above is defined twice, for both
> `Increment` and `Decrement`. This is called
> [pattern matching](https://leanpub.com/purescript/read#leanpub-auto-pattern-matching),
> and can be used as a simpler alternative to `case` expressions.

The `update` function is analogous to `foldl`, which receives the current
action taken by the user, along with the previous state and returns a new
state. If you're wondering about effects (such as data fetching or logging),
that is covered in a later section [Fetching data](/fetching-data.html).

Now that the business logic is defined, we need a way to view the component
state. Pux provides an `Html` type for constructing views:

```purescript
import Prelude (const)
import Pux.Html (Html, (!), (#), bind, div, span, button, text)
import Pux.Html.Events (onClick)

view :: Action -> State -> Html Action
view count =
  div # do
    button ! onClick (const Increment) # text "Increment"
    span # text (show count)
    button ! onClick (const Decrement) # text "Decrement"
```

`Html a` is the type that represents the virtual DOM tree (React elements), and
is paramerized by an action type `a`, which represent the actions a view may
send to the input channel. Those actions are fed into the update function we
defined earlier to produce a new counter state. `!` is used to combine
attributes like `onClick` with elements, and `#` is used to append children.

In the example above, [`do`
notation](https://leanpub.com/purescript/read#leanpub-auto-do-notation) is
being used to append elements. This is made possible by using a version of
`bind` provided by Pux. If you don't know what that is, it's the function that
`do` desugars to. `do` notation is not required, and you can simply use array
literals if preferred:

```purescript
view count =
  div []
    [ button [ onClick (const Increment) ] [ text "Increment" ]
    , span [ text (show count) ]
    , button ! onClick (const Decrement) ] [ text "Decrement" ]
    ]
```


`onClick`, along with other event handlers from
[`Pux.Html.Events`](/API/Pux/Html/Events.html), are used to send actions
to the update function. You'll also notice the use of `const`, which creates
a function that ignores its second argument and returns the first. This is
because `onClick` does not take an action, but a function that receives a
`MouseClick` event and returns an action. This is useful if you want to
include event data as part of an action. But our counter component doesn't
need the `MouseClick` event, so `const` is used to ignore it.

### Rendering to the DOM

Our counter component is finished and ready to be rendered. This is the job of
the [`start`](/API/Pux.html#start) function. It wires together the initial state
with the update and view functions to create an application that can be
rendered to the DOM. An application is a record that consists of an `html` and
`state` signal. For now, we just need to feed the `app.html` signal into the
DOM using `renderToDOM`:

```purescript
import Prelude (bind)
import Pux (start, fromSimple, renderToDOM)
import Counter (update, view)

main = do
  app <- start
    { initialState: 0
    , update: fromSimple update
    , view: view
    , inputs: [] }

  renderToDOM "#app" app.html
```

`fromSimple` is used on the `update` function because `start` expects an update
function that returns an [`EffModel`](/API/Pux.html#effmodel), containing state
and effects.  The counter's `update` function is simpler, so we use
[`fromSimple`](/API/Pux.html#fromsimple) to create the type of the more
advanced `update` function with effects. This is covered in a later section,
[Fetching data](/fetching-data.html).
