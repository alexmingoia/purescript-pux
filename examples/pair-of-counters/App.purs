module CounterPairExample.App where

import CounterPairExample.Counter as Counter
import Prelude (($), const, map)
import Pux.Html (Html, (!), (#), bind, div, button, text)
import Pux.Html.Events (onClick)

data Action
  = Top (Counter.Action)
  | Bottom (Counter.Action)
  | Reset

type State =
  { topCount :: Counter.State
  , bottomCount :: Counter.State }

init :: Int -> State
init count =
  { topCount: Counter.init count
  , bottomCount: Counter.init count }

update :: Action -> State -> State
update (Top action) state =
  state { topCount = Counter.update action state.topCount }
update (Bottom action) state =
  state { bottomCount = Counter.update action state.bottomCount }
update Reset state = state { topCount = 0, bottomCount = 0 }

view :: State -> Html Action
view state = div # do
  map Top $ Counter.view state.topCount
  map Bottom $ Counter.view state.bottomCount
  button ! onClick (const Reset) # text "Reset"
