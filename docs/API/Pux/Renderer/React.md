## Module Pux.Renderer.React

#### `dangerouslySetInnerHTML`

``` purescript
dangerouslySetInnerHTML :: String -> Attribute
```

#### `renderToDOM`

``` purescript
renderToDOM :: forall ev. String -> Signal (HTML ev) -> Channel (List ev) -> Effect Unit
```

```purescript
main = do
 app <- start
   { initialState
   , view
   , foldp
   , inputs: [] }

 renderToDOM "#app" app.markup app.input
```

#### `renderToString`

``` purescript
renderToString :: forall ev. Signal (HTML ev) -> Effect String
```

Return an HTML string from a component's HTML signal. The HTML returned
includes React-specific attributes for fast mounting in the browser.

#### `renderToStaticMarkup`

``` purescript
renderToStaticMarkup :: forall ev. Signal (HTML ev) -> Effect String
```

Return an HTML string from a component's HTML signal. The HTML returned is
stripped of all React-specific attributes.

#### `renderToReact`

``` purescript
renderToReact :: forall ev props. Signal (HTML ev) -> Channel (List ev) -> Effect (ReactClass props)
```

Return a ReactClass from a component's HTML signal.

#### `reactClass`

``` purescript
reactClass :: forall ev props. ReactClass props -> String -> (HTML ev -> HTML ev)
```

Create an HTML constructor for a React class using a unique name. When
rendered this element is replaced with the class.

#### `reactClassWithProps`

``` purescript
reactClassWithProps :: forall ev props. ReactClass props -> String -> (props -> HTML ev -> HTML ev)
```

Create an HTML constructor for a React class using a unique name. When
rendered this element is replaced with the class. The returned constructor
takes an arbitrary props argument, which will be passed to the React class
when rendered.


