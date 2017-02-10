module Pux
  ( App
  , Config
  , renderToDOM
  , renderToString
  , start
  , toReact
  , module Pux.Base
  ) where

import Control.Monad.Eff (Eff)
import Data.Function.Uncurried (Fn3)
import Prelude (Unit, ($))
import Pux.Html (Html)
import React (ReactClass)
import Signal (Signal)

import Pux.Base (Update, EffModel, CoreEffects, fromSimple, noEffects, onlyEffects, mapState, mapEffects)
import Pux.Base as B

-- | Start an application. The resulting html signal is fed into `renderToDOM`.
-- |
-- | ```purescript
-- | main = do
-- |   app <- start
-- |     { update: update
-- |     , view: view
-- |     , initialState: initialState
-- |     , inputs: [] }
-- |
-- |   renderToDOM "#app" app.html
-- | ```
start :: forall state action eff.
         Config state action eff ->
         Eff (B.CoreEffects eff) (App state action)
start = B.start' $ wrapRender B.render


foreign import wrapRender :: forall a eff target. B.Renderer Html a eff -> Fn3 (a -> Eff eff Unit) (a -> a) (target a) (target a)


-- | The configuration of an app consists of update and view functions along
-- | with an initial state.
-- |
-- | The `update` and `view` functions describe how to step the state and view
-- | the state.
-- |
-- | The `inputs` array is for any external signals you might need. These will
-- | be merged into the app's input signal.
type Config state action eff = B.Config Html state action eff

-- | An `App` consists of three signals:
-- |
-- | * `html` – A signal of `Html` representing the current view of your
-- |   app. This should be fed into `renderToDOM`.
-- |
-- | * `state` – A signal representing the application's current state.
type App state action = B.App Html state action

foreign import renderToDOM :: forall a eff. String -> Signal (Html a) -> Eff eff Unit

foreign import renderToString :: forall a eff. Signal (Html a) -> Eff eff String

-- | Return a ReactClass from a Pux component's html signal.
foreign import toReact :: forall a props eff.
                          Signal (Html a) ->
                          Eff eff (ReactClass props)
