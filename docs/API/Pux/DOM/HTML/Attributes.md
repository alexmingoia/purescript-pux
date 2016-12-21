## Module Pux.DOM.HTML.Attributes

#### `data_`

``` purescript
data_ :: String -> String -> Attribute
```

An attribute prefixed with "data-".

#### `focused`

``` purescript
focused :: Attribute
```

Control cursor focus declaratively. Whichever element has this attribute
will be focused after rendering if it was not already focused.

#### `key`

``` purescript
key :: String -> Attribute
```

Key attribute for improved rendering performance.
See: https://facebook.github.io/react/docs/lists-and-keys.html

#### `style`

``` purescript
style :: CSS -> Attribute
```

Render inline CSS and return a style attribute.


