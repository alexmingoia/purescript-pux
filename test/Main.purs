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
import Text.Smolder.Markup (text, (!))

data Event = Noop

type TestEffects = (console :: CONSOLE, testOutput :: TESTOUTPUT, avar:: AVAR)

main :: Eff (CoreEffects TestEffects) Unit
main = runTest do
  test "render simple div to string" $ do
    res <- liftEff $ do
      let foldp Noop st = { state: st,  effects: []}
          view _ = div $ text "hi"
      testDivApp <- start
        { initialState: 0
        , view
        , foldp
        , inputs: []
        }
      app_html <- renderToStaticMarkup testDivApp.markup
      pure app_html
    equal """<div>hi</div>""" res
