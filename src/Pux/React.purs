module Pux.React where

import Control.Monad.Eff (Eff())
import DOM (DOM())
import Data.List
import Prelude
import Pux.React.Types
import Pux.View
import qualified Signal.Channel as S

foreign import getInputFF :: forall eff input. ReactThis -> Eff eff input

foreign import writeStateFF :: forall eff state.
                             ReactThis -> state -> Eff eff Unit

foreign import makeReactComponentFF :: forall eff state.
                                       ReactRender eff state ->
                                       ReactComponentWillMount eff ->
                                       ReactClass

foreign import makeAttrFF :: forall val. String -> val -> Attr
foreign import makeAttrWithObjFF :: forall vals. String -> { | vals } -> Attr
foreign import makeHandlerFF :: forall eff ev. String -> EventHandler eff ev -> MakeHandler
foreign import preventDefaultFF :: forall eff ev. ev -> Eff (dom :: DOM | eff) Unit
foreign import stopPropagationFF :: forall eff ev. ev -> Eff (dom :: DOM | eff) Unit

foreign import makeReactElementFF :: String ->
                                     Array Attr ->
                                     Array ReactElement ->
                                     ReactElement
foreign import makeReactTextFF :: String -> ReactElement

makeAttr :: forall a. String -> a -> Attrs
makeAttr k v = Attrs [makeAttrFF k v] []

makeAttrWithObj :: forall a. String -> { | a } -> Attrs
makeAttrWithObj k v = Attrs [makeAttrWithObjFF k v] []

makeHandler :: forall eff ev action.
               String -> List (ev -> Eff eff Unit) -> (ev -> List action) -> Attrs
makeHandler k effects actions = Attrs [] [makeHandlerFF k h]
  where
  h ctx ev = do
    let fx = map (\fx -> fx ev) effects
    input <- getInputFF ctx
    S.send input (actions ev)
