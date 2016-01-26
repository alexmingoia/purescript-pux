module Pux.React.Types where

import Control.Monad.Eff (Eff())
import DOM (DOM())
import Prelude (Unit())

foreign import data Event :: *
foreign import data Attr :: *

foreign import data ReactClass :: *
foreign import data ReactElement :: *
foreign import data ReactThis :: *
foreign import data EventListener :: * -> *

type MakeHandler = ReactThis -> Attr

type EventHandler eff ev = ReactThis -> ev -> Eff (dom :: DOM | eff) Unit

type ReactComponentWillMount eff = ReactThis -> Eff (dom :: DOM | eff) Unit

type ReactRender eff state = ReactThis ->
                             state ->
                             Eff (dom :: DOM | eff) ReactElement
