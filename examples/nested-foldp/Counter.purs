module CounterPairExample.Counter where

import Control.Bind (bind)
import Data.Function (($), const)
import Data.Ring ((+), (-))
import Data.Show (show)
import Pux (EffModel, noEffects)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (button, div, span)
import Text.Smolder.Markup (text, (#!))

data Event = Increment | Decrement

type State = { count :: Int }

init :: Int -> State
init count = { count: count }

foldp :: ∀ fx. Event -> State -> EffModel State Event fx
foldp Increment st = noEffects $ st { count = st.count + 1 }
foldp Decrement st = noEffects $ st { count = st.count - 1 }

view :: State -> HTML Event
view st =
  div do
    button #! onClick (const Increment) $ text "Increment"
    span $ text (show st.count)
    button #! onClick (const Decrement) $ text "Decrement"
