module BasicExample.Counter where

import Prelude ((+), (-), const, show)
import Pux.Html (Html, div, text, textarea)
import Pux.Html.Attributes (value)
import Pux.Html.Events (onInput, FormEvent)
import Data.String (toUpper)

data Action = TextChange FormEvent

type State = String

init :: State
init = ""

update :: Action -> State -> State
update (TextChange ev) state = toUpper ev.target.value

view :: State -> Html Action
view state =
div [] [ textarea [ onInput TextChange, value state ] [ text state ] ]
