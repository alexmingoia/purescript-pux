module CounterPairExample.Counter where

import Prelude ((-), (+), const, show)
import Pux.Html (Html, (!), (##), (#>), div, span, button)
import Pux.Html.Events (onClick)

data Action = Increment | Decrement

type State = Int

init :: Int -> State
init count = count

update :: Action -> State -> State
update Increment count = count + 1
update Decrement count = count - 1

view :: State -> Html Action
view count =
  div ##
    [ button ! onClick (const Increment) #> "Increment"
    , span #> show count
    , button ! onClick (const Decrement) #> "Decrement"
    ]
