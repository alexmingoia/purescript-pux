module Pux
  ( App
  , Update
  , EffModel
  , CoreEffects
  , noEffects
  , fromSimple
  , mapState
  , mapEffects
  , ReactClass
  , renderToDOM
  , renderToString
  , start
  , toReact
  ) where

import Control.Monad.Aff (Aff, launchAff, later)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Foldable (foldl, sequence_)
import Data.Function (Fn3, runFn3)
import Data.List (List(Nil), singleton, (:), reverse, fromFoldable)
import Data.Maybe.Unsafe (fromJust)
import Prelude (Unit, ($), (<<<), map, pure)
import Pux.Html (Html)
import Signal (Signal, (~>), mergeMany, foldp, runSignal)
import Signal.Channel (CHANNEL, channel, subscribe, send)

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
         Eff (CoreEffects eff) (App state action)
start config = do
  actionChannel <- channel Nil
  let actionSignal = subscribe actionChannel
      input = fromJust $ mergeMany $
        reverse (actionSignal : map (map singleton) (fromFoldable $ config.inputs))
      foldState effModel action = config.update action effModel.state
      foldActions actions effModel =
        foldl foldState (noEffects effModel.state) actions
      effModelSignal =
        foldp foldActions (noEffects config.initialState) input
      stateSignal = effModelSignal ~> _.state
      htmlSignal = stateSignal ~> \state ->
        (runFn3 render) (send actionChannel <<< singleton) (\a -> a) (config.view state)
      mapAffect affect = launchAff $ do
        action <- later affect
        liftEff $ send actionChannel (singleton action)
      effectsSignal = effModelSignal ~> map mapAffect <<< _.effects
  runSignal $ effectsSignal ~> sequence_
  pure $ { html: htmlSignal, state: stateSignal }
  where bind = Prelude.bind

foreign import render :: forall a eff. Fn3 (a -> Eff eff Unit) (a -> a) (Html a) (Html a)

-- | The configuration of an app consists of the basic model / view / update
-- | pattern seen in the Elm app architecture.
-- |
-- | The `update` and `view` functions describe how to step the state and view
-- | the state.
-- |
-- | The `inputs` array is for any external signals you might need. These will
-- | be merged into the app's input signal.
type Config state action eff =
  { update :: Update state action eff
  , view :: state -> Html action
  , initialState :: state
  , inputs :: Array (Signal action)
  }

-- | The set of effects every Pux app needs to allow through when using `start`.
-- | Extend this type with your own app's effects, for example:
-- |
-- | ```purescript
-- | type AppEffects = (console :: CONSOLE, dom :: DOM)
-- |
-- | main :: State -> Eff (CoreEffects AppEffects) (App State Action)
-- | main state = do
-- |   -- ...
-- | ```
type CoreEffects eff = (channel :: CHANNEL, err :: EXCEPTION | eff)

-- | An `App` consists of three signals:
-- |
-- | * `html` – A signal of `Html` representing the current view of your
-- |   app. This should be fed into `renderToDOM`.
-- |
-- | * `state` – A signal representing the application's current state.
type App state action =
  { html :: Signal (Html action)
  , state :: Signal state }

-- | Synonym for an update function that returns state and an array of
-- | asynchronous effects that return an action.
type Update state action eff = action -> state -> EffModel state action eff

-- | `EffModel` is a container for state and a collection of asynchronous
-- | effects which return an action.
type EffModel state action eff =
  { state :: state
  , effects :: Array (Aff (channel :: CHANNEL | eff) action) }

-- | Create an `Update` function from a simple step function.
fromSimple :: forall s a eff. (a -> s -> s) -> Update s a eff
fromSimple update = \action state -> noEffects $ update action state

-- | Create an `EffModel` with no effects from a given state.
noEffects :: forall state action eff. state -> EffModel state action eff
noEffects state = { state: state, effects: [] }

-- | Map over the state of an `EffModel`.
mapState :: forall sa sb a e. (sa -> sb) -> EffModel sa a e -> EffModel sb a e
mapState a2b effmodel =
  { state: a2b effmodel.state, effects: effmodel.effects }

-- | Map over the effectful actions of an `EffModel`.
mapEffects :: forall s a b e. (a -> b) -> EffModel s a e -> EffModel s b e
mapEffects action effmodel =
  { state: effmodel.state, effects: map (map action) effmodel.effects }

foreign import renderToDOM :: forall a eff. String -> Signal (Html a) -> Eff eff Unit

foreign import renderToString :: forall a eff. Signal (Html a) -> Eff eff String

-- | Return a React class from a Pux component's html signal.
foreign import toReact :: forall a eff. Signal (Html a) -> Eff eff ReactClass

foreign import data ReactClass :: *
