## Module Pux.Router

#### `RoutePart`

``` purescript
data RoutePart
  = Path String
  | Query (Map String String)
```

#### `Route`

``` purescript
type Route = List RoutePart
```

#### `Match`

``` purescript
newtype Match a
  = Match (Route -> Maybe (Tuple Route a))
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


