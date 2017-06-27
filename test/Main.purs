module Test.Main where

import Prelude hiding (div)

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)

import Pux (CoreEffects,  start)
import Pux.Renderer.React (renderToStaticMarkup)
import Test.Unit (test)
import Test.Unit.Assert (equal)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Text.Smolder.HTML (div)
import Text.Smolder.HTML (div, li)
import Text.Smolder.Markup (text, (!))

import Test.React (list)
data Event = Noop

type TestEffects = (console :: CONSOLE, testOutput :: TESTOUTPUT, avar:: AVAR)

main :: Eff (CoreEffects TestEffects) Unit
main = runTest do
  test "render simple div to string" $ do
    res <- liftEff $ do
      let foldp Noop st = { state: st,  effects: []}
    result <- liftEff $ do
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
    result <- liftEff $ do
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
    result <- liftEff $ do
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
    result <- liftEff $ do
      let foldp Noop st = { state: st, effects: []}
          view _ = list $ do
            mempty
      component <- start
        { initialState: 0
        , view
        , foldp
        , inputs: []
        }
      app_html <- renderToStaticMarkup testDivApp.markup
      pure app_html
    equal """<div>hi</div>""" res
      rendered <- renderToStaticMarkup component.markup
      pure rendered
    equal """<ul></ul>""" result
