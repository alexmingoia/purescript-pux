module Pux.Render.DOM (renderToDOM) where

import Prelude

import Control.Monad.Eff (Eff())
import Data.Nullable (toMaybe)
import Data.Maybe.Unsafe (fromJust)
import DOM (DOM())
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToParentNode)
import DOM.HTML.Window (document)
import DOM.Node.ParentNode (querySelector)
import DOM.Node.Types (Element())
import Signal.Channel (CHANNEL())

import Pux.React.Types

foreign import renderFF :: forall eff.
                           ReactClass ->
                           Element ->
                           Eff (dom :: DOM | eff) Unit

-- | Renders an application in the DOM element found using given `ElementId`.
renderToDOM :: forall eff.
               String ->
               ReactClass ->
               Eff (channel :: CHANNEL, dom :: DOM | eff) Unit
renderToDOM selector app = do
  container <- findContainer selector
  renderFF app container
  return unit

-- | Returns `Element` used to render.
findContainer :: forall eff. String -> Eff (dom :: DOM | eff) Element
findContainer selector = do
  win <- window
  doc <- document win
  elem <- querySelector selector (htmlDocumentToParentNode doc)
  return (fromJust $ toMaybe elem)
