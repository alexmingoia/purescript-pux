module Pux.Render.HTML (renderToHTML) where

import Prelude

import Control.Monad.Eff (Eff())
import DOM (DOM())
import Pux.React.Types
import Signal.Channel (CHANNEL())

foreign import renderToStringFF :: forall eff.
                                   ReactClass ->
                                   Eff (dom :: DOM | eff) String

-- | Returns HTML for a given application.
renderToHTML :: forall eff.
               ReactClass ->
               Eff (channel :: CHANNEL, dom :: DOM | eff) String
renderToHTML app = renderToStringFF app
