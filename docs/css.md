# CSS

CSS can and should be composed in a type-safe manner by using
[purescript-css](https://github.com/slamdata/purescript-css), which provides the
`CSS` monad. Pux provides `style` constructors that take `CSS` and return a
style element or attribute.

## Inline styles

For example, the [`style`](https://pursuit.purescript.org/packages/purescript-pux/8.7.0/docs/Pux.DOM.HTML.Attributes#v:style)
attribute constructor can be used for inline styles:

```purescript
import CSS (color, fontSize, fontWeight, marginTop, lighter, rgb, em, px)
import Control.Bind (bind)
import Data.Function (($) (#))
import Pux.DOM.HTML (HTML, h1)
import Pux.DOM.HTML.Attributes (style)
import Text.Smolder.Markup ((!), text)

view :: ∀ ev st. st -> HTML ev
view st =
  h1 ! style do
         color (rgb 66 66 84)
         fontSize (1.2 # em)
         fontWeight lighter
         marginTop (0.0 # px)
     $ text "Styled header"
```

## Stylesheets

One approach to rendering a stylesheet for a specific component is to insert a
[`style`](https://pursuit.purescript.org/packages/purescript-pux/8.7.0/docs/Pux.DOM.HTML#v:style)
element in the component's view with that component's CSS. It will be added when
the component mounts and removed when it unmounts.

```purescript
import CSS ((?), fromString, color, fontSize, fontWeight, marginTop, lighter, rgb, em, px)
import Control.Bind (bind)
import Data.Function (($) (#))
import Pux.DOM.HTML (HTML, h1, style)
import Text.Smolder.Markup ((!), text)

view :: ∀ ev st. st -> HTML ev
view st =
  div do
    style do
      fromString ".header" ? do
        color (rgb 66 66 84)
        fontSize (1.2 # em)
        fontWeight lighter
        marginTop (0.0 # px)
    h1 $ text "Styled header"
```

> #### Next: [React interop](/docs/react-interop)
> #### Previous: [Routing](/docs/routing)
