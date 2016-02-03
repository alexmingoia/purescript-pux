module Main where

import Prelude hiding (div)

import Control.Bind ((=<<))
import Control.Monad.Eff.Console (CONSOLE(), log)
import Pux
import Pux.DOM.HTML.Elements (div, p, button, text)
import Pux.DOM.HTML.Attributes (onClick, send)
import Pux.Render.DOM

data Action = Increment | Decrement

type State = { counter :: Int }

initialState :: State
initialState = { counter: 0 }

update :: forall eff. Update (console :: CONSOLE | eff) State Action
update action state input =
  case action of
    Increment ->
      { state: state { counter = state.counter + 1 }
      , effects: [ do log "increment" ] }
    Decrement ->
      { state: state { counter = state.counter - 1 }
      , effects: [ do log "decrement" ] }

view :: State -> VirtualDOM
view state = div $ do
  p $ text ("Counter: " ++ show state.counter)
  p $ do
    button ! onClick (send Increment) $ text "Increment"
    button ! onClick (send Decrement) $ text "Decrement"

main = renderToDOM "#app" =<< app
  { state: initialState
  , update: update
  , view: view
  , inputs: []
  }
