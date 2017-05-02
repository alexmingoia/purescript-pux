# Routing

In Pux, routing is a set of applicatives for matching URLs to data types, along
with a signal for location changes. These are provided by `Pux.Router`.

URL changes are mapped to an event. A data type for routes is used
to contain the matched URL along with parameter and query data.

```purescript
data Event = PageView Route | Navigate String DOMEvent

data Route = Home | Users String | User Int | NotFound
```

We also need a function that constructs a route event from a url, which we
build using routing applicatives and pass to `router` to return the
matched route:

```purescript
match :: String -> Event
match url = PageView $ fromMaybe NotFound $ router url $
  Home <$ end
  <|>
  Users <$> (lit "users" *> param "sortBy") <* end
  <|>
  Users "name" <$ (lit "users") <* end
  <|>
  User <$> (lit "users" *> int) <* end
```

> Learn applicative basics in
> [Functors, Applicatives, and Monads in Pictures](http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html#monads).

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
the url signal provided by
[`sampleURL`](https://pursuit.purescript.org/packages/purescript-pux/9.0.0/docs/Pux.DOM.History#v:sampleURL)
takes a window history object from purescript-dom and returns a signal of the
URL which is updated whenever the `popstate` event occurs:

```purescript
main = do
  url <- sampleURL =<< window
  let routeSignal = urlSignal ~> match

  app <- start
    { initialState: { currentRoute: Home }
    , view
    , foldp
    , inputs: [routeSignal]
    }
```

Every time the location changes and the `PageView` event occurs, we can update
our state with the current route, which includes any captured path or query
parameters. `Navigate` is used to change the location in response to the user
clicking a link, using the HTML5 History methods provided by purescript-dom.

```purescript
foldp :: ∀ fx. Event -> State -> EffModel State Event (history :: HISTORY, dom :: DOM | fx)
foldp (PageView route) st =
  noEffects $ st { currentRoute = route }
foldp (Navigate url ev) st =
  onlyEffects st [ liftEff do
                     preventDefault ev
                     h <- history =<< window
                     pushState (toForeign {}) (DocumentTitle "") (URL url) h
                     pure $ Just $ PageView (match url)
                 ]
```

Finally, we might want to create links to our routes and show different views
for them. We can use purescript-dom to push a new URL into HTML5 History and
generate a `Navigate` event.

```purescript
view :: State -> Html Event
view state =
  div do
    navigation
    page state.currentRoute

page                :: Route -> Html Event
page Home           = h1 $ text "Home"
page (Users sortBy) = h1 $ text ("Users sorted by:" <> sortBy)
page (User id)      = h1 $ text ("User: " <> show id)
page NotFound       = h1 $ text "Not Found"

navigation :: Html Event
navigation =
  nav do
    ul
      li $ a ! href "/" #! onClick (Navigate "/") $ text "Home"
      li $ a ! href "/users" #! onClick (Navigate "/users") $ text "Users"
      li $ a ! href "/users?sortBy=age" #! onClick (Navigate "/users?sortBy=age") $ text "Users sorted by age."
      li $ a ! href "/users/123" #! onClick (Navigate "/users/123") $ text "User 123"
      li $ a ! href "/foobar" #! onClick (Navigate "/foobar") $ text "Not found"
```

> #### Next: [CSS](/docs/css)
> #### Previous: [Forms](/docs/forms)
