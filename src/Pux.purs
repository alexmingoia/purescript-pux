module Pux
  ( App
  , Config
  , FoldP
  , EffModel
  , CoreEffects
  , noEffects
  , onlyEffects
  , mapState
  , mapEffects
  , start
  , waitEvent
  , waitState
  ) where

import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Aff (Aff, later, launchAff, makeAff)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Foldable (foldl, sequence_)
import Data.Function (($), (<<<))
import Data.Functor (map)
import Data.List (List(Nil), singleton, (:), reverse, fromFoldable)
import Data.Maybe (fromJust, Maybe(..))
import Data.Unit (Unit, unit)
import Partial.Unsafe (unsafePartial)
import Signal (Signal, dropRepeats', foldp, mergeMany, runSignal, (~>))
import Signal.Channel (CHANNEL, Channel, channel, subscribe, send)
import Text.Smolder.Markup (Markup)

-- | Create an application, which exposes a markup signal that can be used by
-- | renderers.
-- |
-- | ```purescript
-- | main = do
-- |   app <- start
-- |    { initialState
-- |    , view
-- |    , foldp
-- |    , inputs: [] }
-- |
-- |   renderToDOM "#app" app.markup app.input
-- | ```
start :: ∀ e ev st fx. Config e ev st fx -> Eff (CoreEffects fx) (App e ev st)
start config = do
  evChannel <- channel Nil
  let evSignal = subscribe evChannel
      input = unsafePartial $ fromJust $ mergeMany $
        reverse (evSignal : map (map singleton) (fromFoldable $ config.inputs))
      foldState effModel ev = config.foldp ev effModel.state
      foldEvents evs effModel =
        foldl foldState (noEffects effModel.state) evs
      effModelSignal =
        foldp foldEvents (noEffects config.initialState) input
      stateSignal = dropRepeats' $ effModelSignal ~> _.state
      htmlSignal = stateSignal ~> config.view
      mapAffect affect = launchAff $ unsafeCoerceAff do
        ev <- affect
        later $ case ev of
          Nothing -> pure unit
          Just e -> liftEff $ send evChannel (singleton e)
      effectsSignal = effModelSignal ~> map mapAffect <<< _.effects
  runSignal $ effectsSignal ~> sequence_
  pure $ start_ $
    { markup: htmlSignal
    , state: stateSignal
    , events: input
    , input: evChannel
    }

foreign import start_ :: ∀ e ev st. App e ev st -> App e ev st

-- | The configuration of an app consists of foldp and view functions along
-- | with an initial state. The `foldp` and `view` functions describe how to
-- | step the state and view | the state.
-- |
-- | The `inputs` array is for any external inputs you might need. These will
-- | be merged into the app's input signal.
type Config e ev st fx =
  { initialState :: st
  , view :: st -> Markup e
  , foldp :: FoldP st ev fx
  , inputs :: Array (Signal ev)
  }

-- | The set of effects every Pux app needs to allow through when using `start`.
-- | Extend this type with your own app's effects, for example:
-- |
-- | ```purescript
-- | type AppEffects = (console :: CONSOLE, dom :: DOM)
-- |
-- | main :: State -> Eff (CoreEffects AppEffects) (App DOMEvent State Event)
-- | main state = do
-- |   -- ...
-- | ```
type CoreEffects fx = (channel :: CHANNEL, err :: EXCEPTION | fx)

-- | An `App` is a record consisting of:
-- |
-- | * `markup` – A signal of `Markup e` representing the current view of the
-- |   app. This is consumed by renderers.
-- |
-- | * `state` – A signal representing the application's current state.
-- |
-- | * `input` – A channel representing the application's event input.
type App e ev st =
  { markup :: Signal (Markup e)
  , state :: Signal st
  , events :: Signal (List ev)
  , input :: Channel (List ev)
  }

-- | Return an `EffModel` from the current event and state.
type FoldP st ev fx = ev -> st -> EffModel st ev fx

-- | `EffModel` is a container for state and asynchronous effects which return
-- | an event.
type EffModel st ev fx =
  { state :: st
  , effects :: Array (Aff (CoreEffects fx) (Maybe ev)) }

-- | Create an `EffModel` with no effects from a given state.
noEffects :: ∀ st ev fx. st -> EffModel st ev fx
noEffects state = { state: state, effects: [] }

onlyEffects :: ∀ st ev fx.
               st -> Array (Aff (CoreEffects fx) (Maybe ev)) -> EffModel st ev fx
onlyEffects state effects = { state: state, effects: effects }

-- | Map over the state of an `EffModel`.
mapState :: ∀ a b ev fx. (a -> b) -> EffModel a ev fx -> EffModel b ev fx
mapState a2b effmodel =
  { state: a2b effmodel.state, effects: effmodel.effects }

-- | Map over the effects of an `EffModel`.
mapEffects :: ∀ a b st fx. (a -> b) -> EffModel st a fx -> EffModel st b fx
mapEffects a2b effmodel =
  { state: effmodel.state, effects: map (map (map a2b)) effmodel.effects }

-- | Wait for a specific event until returning the app state.
waitEvent :: ∀ e ev st fx.
             (ev -> Boolean) -> App e ev st -> Aff fx st
waitEvent until app = makeAff \error success -> waitEvent_ until app success

foreign import waitEvent_ :: ∀ e ev st fx
                             .  (ev -> Boolean)
                             -> App e ev st
                             -> (st -> Eff fx Unit)
                             -> Eff fx Unit

-- | Wait for a specific state before returning the app state.
waitState :: ∀ e ev st fx.
             (st -> Boolean) -> App e ev st -> Aff fx st
waitState until app = makeAff \error success -> waitState_ until app success

foreign import waitState_ :: ∀ e ev st fx
                             .  (st -> Boolean)
                             -> App e ev st
                             -> (st -> Eff fx Unit)
                             -> Eff fx Unit
