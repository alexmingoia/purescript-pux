## Routing

> [Example code](https://github.com/alexmingoia/purescript-pux/tree/master/examples/routing/)

In Pux, routing is a set of applicatives for matching URLs to data types, along
with a signal for location changes. These are provided by `Pux.Router`.

URL changes are mapped to an action. A data type for routes is used
to contain the matched URL along with parameter and query data.

```purescript
data Action = PageView Route

data Route = Home | Users String | User Int | NotFound
```

We also need a function that constructs a route action from a url, which we
build using routing applicatives and pass to `router` to return the
matched route:

```purescript
match :: String -> Action
match url = PageView $ fromMaybe NotFound $ router url $
  Home <$ end
  <|>
  Users <$> (lit "users" *> param "sortBy") <* end
  <|>
  Users "name" <$ (lit "users") <* end
  <|>
  User <$> (lit "users" *> int) <* end
```

`Pux.Router` provides applicatives `lit`, `str`, `num`, `int`, `bool`, `param`,
`params`, `any`, and `end` for mapping url parts to route values.

As you can see above, `lit` matches a literal string, and its value is ignored.
`int` matches the second part of `/users/123` to the integer value of `User`.
`end` matches the end of the URL. `param` matches a query parameter, in our case
"sortBy". Queries are a bit tricky as they are optional. Therefore `end` also
matches if there is a query present. This means the order of our `Users` rules
is essential. If we had the `param` less version first, the one with `param`
would never be tried!

Now that we have a function for making a route from a url, we can map it over
the url signal provided by `sampleUrl`:

```purescript
main = do
  urlSignal <- sampleUrl
  let routeSignal = urlSignal ~> match

  app <- start
    { initialState: { currentRoute: Home }
    , update: update
    , view: home
    , inputs: [routeSignal]
    }
```

Everytime the location changes, we can update our state with the current route,
which includes any captured path or query parameters:

```purescript
update :: Action -> State -> State
update (PageView route) state = { currentRoute = route }
```

Finally, we might want to create links to our routes and show different
views for them. The `link` element can be used to push a new location to
HTML5 history, and a simple case expression is used to determine the
correct view:

```purescript
view :: State -> Html Action
view state =
  div [] [ navigation, page state.currentRoute ]

page :: Route -> Html Action
page Home      = h1 [] [ text "Home" ]
page (Users sortBy) = h1 [] [ text ("Users sorted by:" <> sortBy) ]
page (User id) = h1 [] [ text ("User: " <> show id) ]
page NotFound  = h1 [] [ text "Not Found" ]

navigation :: Html Action
navigation =
  nav
    []
    [ ul
      []
      [ li [] [ link "/" [] [ text "Home" ] ]
      , li [] [ link "/users" [] [ text "Users" ] ]
      , li [] [ link "/users?sortBy=age" [] [ text "Users sorted by age." ] ]
      , li [] [ link "/users/123" [] [ text "User 123" ] ]
      , li [] [ link "/foobar" [] [ text "Not found" ] ]
      ]
    ]
```
