## Module Pux.Router

Routing means many different things. In Pux, routing is a set of
applicatives for matching URLs to data types, along with a signal for
location changes.

First, URL changes need to be mapped to an action or actions. We also need
a data type for routes that can contain parameter and query data:

```purescript
data Action = PageView Route

data Route = Home | Users | User Int | NotFound
```

We also need a function that constructs a route action from a url, which we
build using routing applicatives and pass to `router` to return the
matched route:

```purescript
match :: String -> Action
match url = PageView $ fromMaybe NotFound $ router url
  Home <$ end
  <|>
  Users <$ (lit "users") <* end
  <|>
  User <$> (lit "users" *> int) <* end
```

`Pux.Router` provides applicatives `lit`, `str`, `num`, `int`, `bool`, `param`,
`params`, `any`, and `end` for mapping url parts to route values.

As you can see above, `lit` matches a literal string, and its value is ignored.
`int` matches the second part of `/users/123` to the integer value of `User`.
`end` matches the end of the URL.

Now that we have a function for making a route from a url, we can map it over
the url signal provided by `sampleUrl`:

```purescript
main = do
  urlSignal <- sampleUrl
  let routeSignal = urlSignal ~> match

  renderToDOM "#app" =<< app
    { state: { currentRoute: Home }
    , update: update
    , view: home
    , inputs: [routeSignal]
    }
```

Everytime the location changes, we can update our state with the current route,
which includes any captured path or query parameters:

```purescript
update :: forall eff. Update eff State Action
update action state input = case action of
  (PageView route) ->
    { state: { currentRoute: route }
    , effects: []
    }
```

Finally, we might want to create links to our routes and show different
views for them. The `link` attribute can be used to push a new location to
HTML5 history, and a simple case expression is used to determine the
correct view:

```purescript
view :: View State
view state children = div $ do
  div $ case state.currentRoute of
    Home -> h1 $ text "Home"
    Users -> h1 $ text "Users"
    (User id )-> h1 $ text ("User: " ++ show id)
    _ -> h1 $ text "Not Found"
  ul $ do
    li $ a ! link "/" $ text "Home"
    li $ a ! link "/users" $ text "Users"
    li $ a ! link "/users/123" $ text "User 123"
    li $ a ! link "/foobar" $ text "Not found"
```

See it all together in the [routing example](../../examples/routing/).

#### `sampleUrl`

``` purescript
sampleUrl :: forall eff. Eff (dom :: DOM | eff) (Signal String)
```

Returns a signal containing the current window location path and query.

#### `link`

``` purescript
link :: String -> Attrs
```

Creates an attribute that pushes new location to HTML5 history.

```purescript
a ! route "/" $ text "Home"
```

#### `navigate`

``` purescript
navigate :: forall action eff. String -> Handler Event action (dom :: DOM | eff)
```

Handler that pushes new location to HTML5 history.
`ev.preventDefault()` is called if the event target is an `A` element.

#### `RoutePart`

``` purescript
data RoutePart
  = Path String
  | Query (Map String String)
```

#### `Match`

``` purescript
newtype Match a
```

##### Instances
``` purescript
Functor Match
Alt Match
Apply Match
Plus Match
Applicative Match
```

#### `end`

``` purescript
end :: Match Unit
```

#### `lit`

``` purescript
lit :: String -> Match Unit
```

#### `num`

``` purescript
num :: Match Number
```

#### `int`

``` purescript
int :: Match Int
```

#### `bool`

``` purescript
bool :: Match Boolean
```

#### `str`

``` purescript
str :: Match String
```

#### `param`

``` purescript
param :: String -> Match String
```

#### `params`

``` purescript
params :: Match (Map String String)
```

#### `any`

``` purescript
any :: Match Unit
```

#### `router`

``` purescript
router :: forall a. String -> Match a -> Maybe a
```


