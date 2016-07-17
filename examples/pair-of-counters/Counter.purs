module CounterPairExample.Counter where

import Prelude (($), (-), (+), const, show)
import Pux.Html (Html, div, span, button, text)
import Pux.Html.Events (onClick)
import Debug.Trace as Debug
import Signal (Signal, (~>))

data Action = Increment | Decrement

type State = Int

init :: Int -> State
init count = count

update :: Action -> State -> State
update Increment count = count + 1
update Decrement count = count - 1

view :: Signal State -> Signal (Html Action)
view count' =
  count' ~> \count ->
    Debug.spy $ div
      []
      [ button [ onClick (const Increment) ] [ text "Increment" ]
      , span [] [ text (show $ Debug.spy count) ]
      , button [ onClick (const Decrement) ] [ text "Decrement" ]
      ]
