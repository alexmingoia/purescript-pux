module Test.Main where

import Prelude hiding (div)

import Effect (Effect)
import Effect.Class (liftEffect)
import Pux (start)
import Pux.DOM.HTML (HTML)
import Pux.Renderer.React (renderToStaticMarkup)
import Test.React (list)
import Test.Unit (test)
import Test.Unit.Assert (equal)
import Test.Unit.Main (runTest)
import Text.Smolder.HTML (div, li)
import Text.Smolder.Markup (text)
data Event = Noop

main :: Effect Unit
main = runTest do
  test "render simple div to string" $ do
    result <- liftEffect $ do
      let foldp Noop st = { state: st, effects: []}
          view _ = div $ text "hi"
      component <- start
        { initialState: 0
        , view
        , foldp
        , inputs: []
        }
      rendered <- renderToStaticMarkup component.markup
      pure rendered
    equal """<div>hi</div>""" result

  test "renders react-interop list component to string" $ do
    result <- liftEffect $ do
      let foldp Noop st = { state: st, effects: []}
          view _ = list $ do
            li $ text "1"
            li $ text "2"
            li $ text "3"
      component <- start
        { initialState: 0
        , view
        , foldp
        , inputs: []
        }
      rendered <- renderToStaticMarkup component.markup
      pure rendered
    equal """<ul><li>1</li><li>2</li><li>3</li></ul>""" result

  test "renders react-interop list component with a single element to string" $ do
    result <- liftEffect $ do
      let foldp Noop st = { state: st, effects: []}
          view _ = list $ do
            li $ text "1"
      component <- start
        { initialState: 0
        , view
        , foldp
        , inputs: []
        }
      rendered <- renderToStaticMarkup component.markup
      pure rendered
    equal """<ul><li>1</li></ul>""" result

  test "renders react-interop list component with no elements to string" $ do
    result <- liftEffect $ do
      let foldp Noop st = { state: st, effects: []}
          view :: Int -> HTML Event
          view _ = list $ text ""
      component <- start
        { initialState: 0
        , view
        , foldp
        , inputs: []
        }
      rendered <- renderToStaticMarkup component.markup
      pure rendered
    equal """<ul></ul>""" result
