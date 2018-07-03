module CounterPairExample.App where

import Prelude (discard)
import CounterPairExample.Counter as Counter
import Data.Function (const, ($), (#))
import Pux (EffModel, noEffects, mapEffects, mapState)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML, child)
import Text.Smolder.HTML (button, div)
import Text.Smolder.Markup (text, (#!))

data Event
  = Top (Counter.Event)
  | Bottom (Counter.Event)
  | Reset

type State =
  { topCount :: Counter.State
  , bottomCount :: Counter.State }

init :: Int -> State
init count =
  { topCount: Counter.init count
  , bottomCount: Counter.init count }

foldp :: Event -> State -> EffModel State Event
foldp (Top ev) st =
  Counter.foldp ev st.topCount
    # mapEffects Top # mapState \s -> st { topCount = s }

foldp (Bottom ev) st =
  Counter.foldp ev st.bottomCount
    # mapEffects Bottom # mapState \s -> st { bottomCount = s }

foldp Reset st =
  noEffects $ st { topCount = Counter.init 0, bottomCount = Counter.init 0 }

view :: State -> HTML Event
view state = div do
  child Top Counter.view $ state.topCount
  child Bottom Counter.view $ state.bottomCount
  button #! onClick (const Reset) $ text "Reset"
