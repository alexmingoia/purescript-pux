## Module Pux.Router

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

#### `Route`

``` purescript
type Route = List RoutePart
```

#### `RoutePart`

``` purescript
data RoutePart
  = Path String
  | Query (Map String String)
```

#### `router`

``` purescript
router :: forall a. String -> Match a -> Maybe a
```

#### `lit`

``` purescript
lit :: String -> Match Unit
```

#### `str`

``` purescript
str :: Match String
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

#### `parseSegment`

``` purescript
parseSegment :: forall a. (String -> Maybe a) -> Match a
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

#### `end`

``` purescript
end :: Match Unit
```


