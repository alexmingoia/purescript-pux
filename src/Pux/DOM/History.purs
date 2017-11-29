module Pux.DOM.History (sampleURL, sampleHashURL) where

import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Event.Types (EventType(..))
import DOM.HTML.Location (pathname, search, hash)
import DOM.HTML.Types (HISTORY, Window, Location, windowToEventTarget)
import DOM.HTML.Window (location)
import Data.Semigroup ((<>))
import Prelude (discard)
import Signal (Signal)
import Signal.Channel (CHANNEL, channel, send, subscribe)

-- | Returns a signal containing the current window.location pathname and search query,
-- | which is updated on every "popstate" event.
sampleURL :: ∀ eff. Window -> Eff (channel :: CHANNEL, history :: HISTORY, dom :: DOM | eff) (Signal String)
sampleURL = sampleLocationWith pathname

sampleHashURL :: ∀ eff. Window -> Eff (channel :: CHANNEL, dom :: DOM | eff) (Signal String)
sampleHashURL = sampleLocationWith hash

sampleLocationWith :: ∀ eff. (Location -> Eff (channel :: CHANNEL, dom :: DOM| eff) String) -> Window -> Eff (channel :: CHANNEL, dom :: DOM | eff) (Signal String)
sampleLocationWith f win = do
  loc <- location win
  path <- f loc
  search <- search loc
  chan <- channel (path <> search)

  let listener = eventListener \ev -> do
        url <- f loc
        send chan url

  addEventListener (EventType "popstate") listener false (windowToEventTarget win)

  pure (subscribe chan)
