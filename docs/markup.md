# Markup

Pux uses
[purescript-smolder](https://pursuit.purescript.org/packages/purescript-smolder)'s
markup monad. `HTML ev` is an alias for `Markup (DOMEvent -> ev)`, which is used
with `do` notation to describe a state's view:

```purescript
view :: State -> HTML Event
view count =
  div do
    button #! onClick (const Increment) $ text "Increment"
    span $ text (show count)
    button #! onClick (const Decrement) $ text "Decrement"
```

> Learn more about do notation and monad basics in
> [Functors, Applicatives, and Monads in Pictures](http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html#monads).

Smolder provides constructors for each element or attribute, like `div` or
`button`, or `className` and `href`. When applicable, constructors take an
argument for a child element or attribute value. Also provided are operators for
combining elements with attributes or event handlers.

## Elements

Element constructors are provided by
[`Text.Smolder.HTML`](https://pursuit.purescript.org/packages/purescript-smolder/6.0.1/docs/Text.Smolder.HTML),
with container elements taking an argument for children.

Children can be
[`text`](https://pursuit.purescript.org/packages/purescript-smolder/6.0.0/docs/Text.Smolder.Markup#v:text):

```purescript
h1 $ text "Hello, World"
```

Or another element:

```purescript
div $ p (text "PureScript")
```

Or multiple elements using do notation:

```purescript
div do
  h1 $ text "Hello, World"
  p $ text "I'm HTML."
```

Sometimes you want an empty container element. Because HTML is a monoid, you can
use
[`mempty`](https://pursuit.purescript.org/packages/purescript-monoid/2.2.0/docs/Data.Monoid#v:mempty):

```purescript
div mempty
```

## Attributes

The
[`!`](https://pursuit.purescript.org/packages/purescript-smolder/6.0.0/docs/Text.Smolder.Markup#v:%28!%29)
operator is used to add attributes to elements:

```purescript
a ! href "purescript.org" ! className "yellow" $ text "purescript.org"
```

Optional attributes can be declared using the
[`!?`](https://pursuit.purescript.org/packages/purescript-smolder/6.0.0/docs/Text.Smolder.Markup#v:%28!?%29)
operator in combination with a boolean:

```purescript
button !? state.loading (className "loading") $ text "Save"
```

## Event handlers

The
[`#!`](https://pursuit.purescript.org/packages/purescript-smolder/6.0.0/docs/Text.Smolder.Markup#v:%28#!%29)
operator is used with constructors from
[`Pux.DOM.Events`](https://pursuit.purescript.org/packages/purescript-pux/8.7.0/docs/Pux.DOM.Events) to add event handlers to
elements:

```purescript
button #! onClick (const Increment) $ text "Increment"
```

Pux provides constructors for creating event handlers for all the DOM events via
[`Pux.DOM.Events`](https://pursuit.purescript.org/packages/purescript-pux/8.7.0/docs/Pux.DOM.Events).
These constructors take an argument of type `DOMEvent -> ev` where `ev` is the
type of your application events. This is so that data in the raw DOM event can
be used in the application events. If the DOM event is not needed by the
application event, it can be ignored using the
[`const`](https://pursuit.purescript.org/packages/purescript-prelude/2.1.0/docs/Data.Function#v:const)
function as shown in the example above.

> `DOMEvent` is an alias for
> [purescript-dom](https://pursuit.purescript.org/packages/purescript-dom/3.3.1)'s
> `Event`, so you can name your application event type the same without a
> name collision.

## Pux elements and attributes

In addition to purescript-smolder, Pux provides its own element and attribute
constructors in [`Pux.DOM.HTML`](https://pursuit.purescript.org/packages/purescript-pux/8.7.0/docs/Pux.DOM.HTML) and
[`Pux.DOM.HTML.Attributes`](https://pursuit.purescript.org/packages/purescript-pux/8.7.0/docs/Pux.DOM.HTML.Attributes):

- The `style` [element](https://pursuit.purescript.org/packages/purescript-pux/8.7.0/docs/Pux.DOM.HTML#v:style) and
  [attribute](https://pursuit.purescript.org/packages/purescript-pux/8.7.0/docs/Pux.DOM.HTML.Attributes#v:style) take `CSS` from
  purescript-css.
- [`focus`](https://pursuit.purescript.org/packages/purescript-pux/8.7.0/docs/Pux.DOM.HTML.Attributes#v:focus) declaratively controls
  input focus. 
- [`key`](https://pursuit.purescript.org/packages/purescript-pux/8.7.0/docs/Pux.DOM.HTML.Attributes#v:key) is for rendering optimization
  (used by React and others).
- [`data_`](https://pursuit.purescript.org/packages/purescript-pux/8.7.0/docs/Pux.DOM.HTML.Attributes#v:data_) constructs properties
  prefixed with "data-".

## HTML from lists

Often elements need to be generated from a
[`Foldable`](https://pursuit.purescript.org/packages/purescript-foldable-traversable/2.0.0/docs/Data.Foldable),
whether a List, an Array, etc. Use
[`for_`](https://pursuit.purescript.org/packages/purescript-foldable-traversable/2.0.0/docs/Data.Foldable#v:for_)
from
[purescript-foldable-traversable](https://pursuit.purescript.org/packages/purescript-foldable-traversable):

```purescript
colors :: Array String
colors = ["purple","red","green"]

color :: String -> HTML Event
color s = li $ text s

view :: HTML Event
view = ul $ for_ colors color
```

## Memoization

Use Pux's [`memoize`](https://pursuit.purescript.org/packages/purescript-pux/8.7.0/docs/Pux.DOM.HTML#v:memoize)
where it's important to avoid unnecessary rendering:

```purescript
view :: State -> HTML Event
view = memoize \state -> div $ text state.message
```

> It's important that `memoize` is only used at a top-level declaration and
> not inside a view. This is because PureScript is eagerly evaluated like
> JavaScript. If `memoize` is used inside a view it will recreate the memoized
> function every time the view is called.

> #### Next: [Rendering](/docs/rendering)
> #### Previous: [Events](/docs/events)
