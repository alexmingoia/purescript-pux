module BasicExample.Counter where

import Control.Bind (bind)
import Data.Function (const, ($))
import Data.Ring ((+), (-))
import Data.Show (show)
import Pux (EffModel)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (button, div, span)
import Text.Smolder.Markup (text, (#!))

data Event = Increment | Decrement

newtype State = State Int

foldp :: ∀ fx. Event -> State -> EffModel State Event fx
foldp Increment (State n) = { state: State (n + 1), effects: [] }
foldp Decrement (State n) = { state: State (n - 1), effects: [] }

view :: State -> HTML Event
view (State n) =
  div do
    button #! onClick (const Increment) $ text "Increment"
    span $ text (show n)
    button #! onClick (const Decrement) $ text "Decrement"
