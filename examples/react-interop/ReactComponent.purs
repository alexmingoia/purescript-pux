module ReactInteropExample.ReactComponent where

import React (ReactClass)
import Pux.Renderer.React (reactClassWithProps)

foreign import component_ :: ∀ props. ReactClass props

component = reactClassWithProps component_ "MyComponent"
