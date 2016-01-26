module Pux.Render.HTML (renderToHTML) where

import Prelude

import Control.Monad.Eff (Eff())
import DOM (DOM())
import Pux.React.Types
import Signal.Channel (Chan())

foreign import renderToStringFF :: forall eff.
                                   ReactClass ->
                                   Eff (dom :: DOM | eff) String

-- | Returns HTML for a given application.
renderToHTML :: forall eff.
               ReactClass ->
               Eff (chan :: Chan, dom :: DOM | eff) String
renderToHTML app = renderToStringFF app
