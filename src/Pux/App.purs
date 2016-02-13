module Pux.App
  ( app
  , Config()
  , EffModel()
  , Input()
  , Update()
  ) where

import Control.Monad.Eff (Eff())
import Data.Array as A
import Data.List (List(Nil), fromFoldable, singleton, (:), reverse)
import Data.Foldable (sequence_, foldl)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import Data.Maybe.Unsafe (fromJust)
import DOM (DOM())
import Prelude (Unit, map, (<>), ($), pure, return, bind)
import Pux.DOM (VirtualDOM, VDomM(Return, Content, Node))
import Pux.React (makeReactTextFF, makeReactElementFF, makeReactComponentFF, writeStateFF)
import Pux.React.Types (Attr, MakeHandler, ReactThis, ReactElement, ReactClass)
import Signal (foldp, mergeMany, (~>), runSignal, Signal())
import Signal.Channel (channel, subscribe, CHANNEL(), Channel())

-- | Initialize a Pux application.
-- |
-- | ```purescript
-- | main = renderToDOM "#app" =<< app
-- |   { state: initialState
-- |   , view: view
-- |   , update: update
-- |   -- | additional action signals to merge into input
-- |   , inputs: []
-- |   }
-- | ```
app :: forall eff state action.
       Config eff state action ->
       Eff (channel :: CHANNEL, dom :: DOM | eff) ReactClass
app config = do
  actionChannel <- channel Nil
  let actionSignal = subscribe actionChannel
      noEffects state = { state: state, effects: [] }
      input = fromJust $ mergeMany $
        reverse (actionSignal : map (map singleton) (fromFoldable $ config.inputs))
      foldState effState action =
        config.update action effState.state actionChannel
      foldActions actions effState =
        foldl foldState (noEffects effState.state) actions
      effStateSignal =
        foldp foldActions (noEffects config.state) input
      componentWillMount ctx = do
        let runEffects effState = do
              writeStateFF ctx
                { input: input
                , state: effState.state
                }
              sequence_ effState.effects
        let renderSignal = effStateSignal ~> runEffects
        runSignal renderSignal
      render ctx state =
        return $ renderToReact ctx $ config.view state.state
      component = makeReactComponentFF render componentWillMount
  pure component

-- | Render React element from virtual dom tree, threading context through the
-- | event handlers.
renderToReact :: ReactThis -> VirtualDOM -> ReactElement
renderToReact ctx vdom = fromMaybe emptydiv $ A.head (renderNode ctx vdom)
  where emptydiv = makeReactElementFF "div" [] []

renderNode :: ReactThis -> VirtualDOM -> Array ReactElement
renderNode ctx (Node name (Just children) props handlers rest) =
  makeReactElementFF name (props <> map (renderHandler ctx) handlers)
    (renderNode ctx children) A.: renderNode ctx rest
renderNode ctx (Node name Nothing props handlers rest) =
  makeReactElementFF name (props <> map (renderHandler ctx) handlers)
    [] A.: renderNode ctx rest
renderNode ctx (Content text rest) =
  makeReactTextFF text A.: renderNode ctx rest
renderNode ctx (Return _) = []

renderHandler :: ReactThis -> MakeHandler -> Attr
renderHandler ctx mkhandler = mkhandler ctx

type Config eff state action =
  { state :: state
  , view :: state -> VirtualDOM
  , update :: Update eff state action
  , inputs :: Array (Signal action)
  }

-- | `Input` is a channel which receives actions from the view.
type Input action = Channel (List action)

-- | `Update` receives actions from `Input`, the current state, the input
-- | channel (for asynchronous state changes), and returns a new state and
-- | collection of effects to run.
-- |
-- | ```purescript
-- | update :: forall eff. Update (console :: CONSOLE | eff) State Action
-- | update action state input =
-- |   case action of
-- |     Increment ->
-- |       { state: state { counter = state.counter + 1 }
-- |       , effects: [ do log "increment" ] }
-- |     Decrement ->
-- |       { state: state { counter = state.counter - 1 }
-- |       , effects: [ do log "decrement" ] }
-- | ```
type Update eff state action = action ->
                               state ->
                               Input action ->
                               EffModel eff state

-- | `EffModel` is a container for state and an associated collection of effects
-- | returned by the `Update` function.
type EffModel eff state =
  { state :: state
  , effects :: Array
    ( Eff
      ( dom :: DOM
      , channel :: CHANNEL
      | eff
      )
      Unit
    )
  }
