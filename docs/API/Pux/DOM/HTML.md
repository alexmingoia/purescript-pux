## Module Pux.DOM.HTML

#### `HTML`

``` purescript
type HTML ev = Markup (DOMEvent -> ev)
```

A type for HTML markup, parametized by the events it may trigger. It is a
synonym for the `Markup` monad from
[purescript-smolder](https://pursuit.purescript.org/packages/purescript-smolder).

#### `child`

``` purescript
child :: forall s a b. (a -> b) -> (s -> HTML a) -> (s -> HTML b)
```

Memoize child view and map event handlers with parent event type.

#### `mapEvent`

``` purescript
mapEvent :: forall a b. (a -> b) -> HTML a -> HTML b
```

Map HTML with event type `a` to HTML with event type `b`.

#### `memoize`

``` purescript
memoize :: forall st ev. (st -> HTML ev) -> (st -> HTML ev)
```

Memoize view. Works with records, ADTs, and arrays.

#### `style`

``` purescript
style :: forall ev. CSS -> HTML ev
```

Render CSS stylesheet and return a style element.


