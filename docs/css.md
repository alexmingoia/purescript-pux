# CSS

## Type-safe CSS

CSS can and should be composed in a type-safe manner by using
[purescript-css](https://github.com/slamdata/purescript-css) and
[purescript-pux-css](https://github.com/alexmingoia/pux-css).

[purescript-css](https://github.com/slamdata/purescript-css) provides a monad
for specifying styles, and
[purescript-pux-css](https://github.com/alexmingoia/pux-css) provides the `css`
method for rendering to tuples, and a `style` attribute that takes `CSS`
directly and returns an `Attribute`.

For example:

```purescript
import Pux.CSS (color, em, fontSize, fontWeight, marginTop, px, rgb, size, style, weight)
import Pux.Html (h1, text)

h1
  [ style $ do
      color (rgb 66 66 84)
      fontSize (1.2 # em)
      fontWeight (weight 400.0)
      marginTop (0.0 # px)
  ]
  [ text "Styled header" ]
```

## Inline styles

The [`Pux.style`](/API/Pux/Html/Attributes.html#style) attribute can be used to
specify inline styles, and takes an array of tuples, representing a CSS rule
name and value:

```purescript
h1
  [ style
      [ Tuple "fontSize" "1.2em"
      , Tuple "fontWeight" "400"
      ]
  ]
  [ text "Styled header" ]
```

## Component-specific styles

One approach to rendering a stylesheet for a specific component is to insert a
`style` element in the component's view with that component's CSS. It will be
added when the component mounts and removed when it unmounts.
