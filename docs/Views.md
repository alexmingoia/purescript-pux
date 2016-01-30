# Views

A Pux view has type `View State` and produces a virtual DOM from the state:

```purescript
view :: View State
view state children = div $ do
  p $ text ("Counter: " ++ show state.counter)
  p $ do
    button ! onClick (send Increment) $ text "Increment"
    button ! onClick (send Decrement) $ text "Decrement"
```

Views are constructed using `VDom`, a monadic DSL for constructing React
virtual DOM using `do` notation. Pux provides constructors for all the elements
and attributes supported by React, along with the `!` operator to combine
attributes with elements.
