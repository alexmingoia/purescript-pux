# Fetching data

> [Example code](https://github.com/alexmingoia/purescript-pux/tree/master/examples/ajax/)

In this section, we'll learn about handling effects by building a component
that fetches a list of todos from a remote server.

First, we need an action for requesting todos, and an action for receiving
todos:

```purescript
data Action = RequestTodos | ReceiveTodos (Either String Todos)
```

We also need to describe the state of our component:

```purescript
type State =
  { todos :: Todos
  , status :: String }

type Todos = Array Todo

newtype Todo = Todo
  { id :: Int
  , title :: String }
```

To parse the JSON received from the server into `Todos`, we implement the
`decodeJson` method for the typeclass provided by
[`purescript-argonaut`](https://github.com/purescript-contrib/purescript-argonaut):

```purescript
instance decodeJsonTodo :: DecodeJson Todo where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .? "id"
    title <- obj .? "title"
    pure $ Todo { id: id, title: title }
```

In the previous sections our update function had a simple type `Action ->
State -> State`. For fetching remote data, our update function will need to
handle side-effects as well. This is accomplished by our update function returning an array of effects
along with a new state - which Pux calls an [`EffModel`](/API/Pux.html#effmodel):

```purescript
type EffModel state action eff =
  { state :: state
  , effects :: Array (Aff (channel :: CHANNEL | eff) action)
  }
```

Effects return a new action and run asynchronously using the `Aff` monad from
[`purescript-aff`](https://github.com/slamdata/purescript-aff).

Our update function takes care of fetching todos and updating the state using
[`purescript-affjax`](https://github.com/slamdata/purescript-affjax) which
performs AJAX requests:

```purescript
update :: Action -> State -> EffModel State Action (ajax :: AJAX)

update (ReceiveTodos (Left err)) state =
  noEffects $ state { status = "Error fetching todos: " ++ show err }

update (ReceiveTodos (Right todos)) state =
  noEffects $ state { todos = todos, status = "Todos" }

update (RequestTodos) state =
  { state: state { status = "Fetching todos..." }
  , effects: [ do
      res <- attempt $ get "http://jsonplaceholder.typicode.com/users/1/todos"
      let decode res = decodeJson res.response :: Either String Todos
      let todos = either (Left <<< show) decode res
      return $ ReceiveTodos todos
    ]
  }
```

What if we wanted to fetch the todos on page-load? We could use
`Pux.Router` to trigger actions on location changes. See the
[Routing guide](/routing.html) for more information.

Finally, we create a simple view of the todos along with a button to fetch
them:

```purescript
view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ text state.status ]
    , div
        []
        [ button [ onClick (const RequestTodos) ] [ text "Fetch todos" ]
        , ol [] $ map todo state.todos
        ]
    ]

todo :: Todo -> Html Action
todo (Todo state) =
  li [ key (show state.id), className "todo" ] [ text state.title ]
```
