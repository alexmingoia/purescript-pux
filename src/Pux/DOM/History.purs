module Pux.DOM.History (sampleURL) where

import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Event.Types (EventType(..))
import DOM.HTML.Location (pathname, search)
import DOM.HTML.Types (HISTORY, Window, windowToEventTarget)
import DOM.HTML.Window (location)
import Data.Semigroup ((<>))
import Signal (Signal)
import Signal.Channel (CHANNEL, channel, send, subscribe)

-- | Returns a signal containing the current window.location pathname and search query,
-- | which is updated on every "popstate" event.
sampleURL :: ∀ eff. Window -> Eff (channel :: CHANNEL, history :: HISTORY, dom :: DOM | eff) (Signal String)
sampleURL win = do
  loc <- location win
  path <- pathname loc
  search <- search loc
  chan <- channel (path <> search)

  let listener = eventListener \ev -> do
        url <- pathname loc
        send chan url

  addEventListener (EventType "popstate") listener false (windowToEventTarget win)

  pure (subscribe chan)
