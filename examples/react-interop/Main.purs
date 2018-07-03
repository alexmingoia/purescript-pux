module ReactInteropExample where

import Prelude hiding (div)

import Effect (Effect)
import Pux (FoldP, start)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToDOM)
import ReactInteropExample.ReactComponent (component)
import Text.Smolder.HTML (div, h1)
import Text.Smolder.Markup (empty, text)

type State = Unit

data Event = Event

foldp :: FoldP State Event
foldp event state = { state, effects: [] }

view :: State -> HTML Event
view state =
  div do
    h1 $ text "Rendered by PureScript"
    component { message: "Rendered by external React component" } empty

main :: Effect Unit
main = do
  app <- start
    { initialState: unit
    , view
    , foldp
    , inputs: []
    }

  renderToDOM "#app" app.markup app.input
