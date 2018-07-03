module BasicExample.Counter where

import Prelude (discard)
import Data.Function (const, ($))
import Data.Ring ((+), (-))
import Data.Show (show)
import Pux (EffModel)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (button, div, span)
import Text.Smolder.Markup (text, (#!))

data Event = Increment | Decrement

type State = Int

foldp :: Event -> State -> EffModel State Event
foldp Increment n = { state: n + 1, effects: [] }
foldp Decrement n = { state: n - 1, effects: [] }

view :: State -> HTML Event
view count =
  div do
    button #! onClick (const Increment) $ text "Increment"
    span $ text (show count)
    button #! onClick (const Decrement) $ text "Decrement"
