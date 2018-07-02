module Pux
  ( App
  , Config
  , FoldP
  , EffModel
  , noEffects
  , onlyEffects
  , mapState
  , mapEffects
  , start
  , waitEvent
  , waitState
  ) where

import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Effect.Aff (Aff, launchAff, makeAff, delay)
import Effect (Effect)
import Effect.Class (liftEffect)
import Data.Array (snoc)
import Data.Foldable (foldl, sequence_)
import Data.Function (($), (<<<))
import Data.Functor (map, (<$))
import Data.List (List(Nil), singleton)
import Data.Maybe (fromJust, Maybe(..))
import Data.Monoid (mempty)
import Data.Time.Duration (Milliseconds(Milliseconds))
import Data.Unit (Unit, unit)
import Partial.Unsafe (unsafePartial)
import Signal (Signal, dropRepeats', foldp, mergeMany, runSignal, (~>))
import Signal.Channel (Channel, channel, channel, subscribe, send)
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
start :: ∀ e ev st. Config e ev st -> Effect (App e ev st)
start config = do
  evChannel <- channel Nil
  let evSignal = subscribe evChannel
      input = unsafePartial $ fromJust $ mergeMany $
        snoc (map (map singleton) config.inputs) evSignal
      foldState effModel ev = config.foldp ev effModel.state
      foldEvents evs effModel =
        foldl foldState (noEffects effModel.state) evs
      effModelSignal =
        foldp foldEvents (noEffects config.initialState) input
      stateSignal = dropRepeats' (effModelSignal ~> _.state)
      htmlSignal = stateSignal ~> config.view
      mapAffect affect = launchAff do
        ev <- affect
        case ev of
          Nothing -> pure unit
          Just e -> do
            delay (Milliseconds 0.0)
            liftEffect (send evChannel (singleton e))
      effectsSignal = effModelSignal ~> map mapAffect <<< _.effects
  runSignal $ effectsSignal ~> sequence_
  pure $ start_
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
type Config e ev st =
  { initialState :: st
  , view :: st -> Markup e
  , foldp :: FoldP st ev
  , inputs :: Array (Signal ev)
  }

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
type FoldP st ev = ev -> st -> EffModel st ev

-- | `EffModel` is a container for state and asynchronous effects which return
-- | an event.
type EffModel st ev =
  { state :: st
  , effects :: Array (Aff (Maybe ev)) }

-- | Create an `EffModel` with no effects from a given state.
noEffects :: ∀ st ev. st -> EffModel st ev
noEffects state = { state: state, effects: [] }

onlyEffects :: ∀ st ev.
               st -> Array (Aff (Maybe ev)) -> EffModel st ev
onlyEffects state effects = { state: state, effects: effects }

-- | Map over the state of an `EffModel`.
mapState :: ∀ a b ev. (a -> b) -> EffModel a ev -> EffModel b ev
mapState a2b effmodel =
  { state: a2b effmodel.state, effects: effmodel.effects }

-- | Map over the effects of an `EffModel`.
mapEffects :: ∀ a b st. (a -> b) -> EffModel st a -> EffModel st b
mapEffects a2b effmodel =
  { state: effmodel.state, effects: map (map (map a2b)) effmodel.effects }

-- | Wait for a specific event until returning the app state.
waitEvent :: ∀ e ev st.
             (ev -> Boolean) -> App e ev st -> Aff st
waitEvent until app = makeAff \cb -> mempty <$ waitEvent_ until app (cb <<< pure)

foreign import waitEvent_ :: ∀ e ev st
                             .  (ev -> Boolean)
                             -> App e ev st
                             -> (st -> Effect Unit)
                             -> Effect Unit

-- | Wait for a specific state before returning the app state.
waitState :: ∀ e ev st.
             (st -> Boolean) -> App e ev st -> Aff st
waitState until app = makeAff \cb -> mempty <$ waitState_ until app (cb <<< pure)

foreign import waitState_ :: ∀ e ev st
                             .  (st -> Boolean)
                             -> App e ev st
                             -> (st -> Effect Unit)
                             -> Effect Unit
