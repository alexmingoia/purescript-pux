module Test.React where

import React (ReactClass)

import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (reactClass)

foreign import listComponent :: ∀ props. ReactClass props
list :: ∀ ev.  HTML ev -> HTML ev
list = reactClass listComponent "list"
