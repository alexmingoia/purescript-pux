module Pux.DOM.History (sampleURL) where

import Control.Applicative (pure)
import Control.Bind (bind)
import Data.Semigroup ((<>))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Prelude (discard)
import Signal (Signal)
import Signal.Channel (channel, send, subscribe)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML.Location (pathname, search)
import Web.HTML.Window (Window, location, toEventTarget)

-- | Returns a signal containing the current window.location pathname and search query,
-- | which is updated on every "popstate" event.
sampleURL :: Window -> Effect (Signal String)
sampleURL win = do
  loc <- location win
  path <- pathname loc
  search <- search loc
  chan <- channel (path <> search)

  let listener = eventListener \ev -> do
        url <- pathname loc
        send chan url

  addEventListener (EventType "popstate") (unsafePerformEffect listener) false (toEventTarget win)

  pure (subscribe chan)
