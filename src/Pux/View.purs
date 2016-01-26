module Pux.View
  ( (!)
  , Attrs(..)
  , Attributable
  , EventOpt(..)
  , Handler(..)
  , VDomM(..)
  , VDom()
  , View()
  , with
  ) where

import Control.Monad.Eff (Eff())
import Control.Apply ((*>))
import Data.List
import Data.Maybe
import Data.Monoid
import Prelude
import Pux.React.Types

-- | `View` is a rendering function that receives state, children, and returns
-- | a `VDom`. `VDom` is a monadic DSL for constructing React virtual DOM using
-- | `do` notation:
-- |
-- | ```purescript
-- | import Pux (View())
-- | import Pux.DOM.HTML.Elements (div, p, button, text)
-- | import Pux.DOM.HTML.Attributes (onClick, send)
-- |
-- | view :: View State
-- | view state children = div $ do
-- |   p $ text ("Counter: " ++ show state.counter)
-- |   p $ do
-- |     button ! onClick (send Increment) $ text "Increment"
-- |     button ! onClick (send Decrement) $ text "Decrement"
-- | ```
-- |
-- | The `!` operator combines elements with attributes.
-- |
-- | `send` creates a handler that sends actions to input. There are also
-- | handlers for preventing the default event behavior or its propagation,
-- | which can be combined using append:
-- |
-- | ```purescript
-- | onClick (send Increment <> preventDefault <> stopPropagation)
-- | ```
-- |
-- | Handlers take either an action, or a function that constructs an action
-- | from an event object. Some events, such as keyboard or mouse events,
-- | provide an object that contains event information such as key pressed,
-- | mouse position, etc.
-- |
-- | For example, `onKeyUp` provides the `KeyboardEvent` object:
-- |
-- | ```purescript
-- | data Action = KeyUp KeyboardEvent
-- |
-- | type State = { lastKeyPressed :: String }
-- |
-- | update action state input = case action of
-- |   (KeyUp ev) ->
-- |     { state: { lastKeyPressed: ev.key }
-- |     , effects: []
-- |     }
-- |
-- | view state children = div $ do
-- |   p $ "Last key pressed: " ++ state.lastKeyPressed
-- |   input ! onKeyUp (send KeyUp) ! placeholder "Type something"
-- | ```
-- |
-- | To learn which events provide extra action arguments, refer to the
-- | `Pux.DOM.HTML.Attributes` type signatures.
type View state = state -> VDom -> VDom

data VDomM a
  = Node String (Maybe VDom) (Array Attr) (Array MakeHandler) (VDomM a)
  | Content String (VDomM a)
  | Return a

type VDom = VDomM Unit

instance semigroupVDomM :: Semigroup (VDomM a) where
  append x y = x *> y

instance monoidVDom :: Monoid (VDomM Unit) where
  mempty = Return unit

instance functorVDomM :: Functor VDomM where
  map f (Node el children attrs handlers rest) =
    Node el children attrs handlers (map f rest)
  map f (Content s rest) = Content s (map f rest)
  map f (Return a) = Return (f a)

instance applyVDomM :: Apply VDomM where
  apply = ap

instance applicativeVDomM :: Applicative VDomM where
  pure = Return

instance bindVDomM :: Bind VDomM where
  bind (Node el children attrs handlers rest) f =
    Node el children attrs handlers (bind rest f)
  bind (Content s rest) f = Content s (bind rest f)
  bind (Return a) f = f a

instance monadVDomM :: Monad VDomM

data Attrs = Attrs (Array Attr) (Array MakeHandler)

data EventOpt = PreventDefault | StopPropagation

-- | A handler is a collection of actions and effects.
data Handler ev action eff = Handler (List action) (List (ev -> Eff eff Unit))

instance handlerSemigroup :: Semigroup (Handler ev action eff) where
  append (Handler a x) (Handler b y) = Handler (a <> b) (x <> y)

instance semigroupAttrs :: Semigroup Attrs where
  append (Attrs as xs) (Attrs bs ys) = Attrs (append as bs) (append xs ys)

instance monoidAttrs :: Monoid Attrs where
  mempty = Attrs mempty mempty

class Attributable a where
  with :: a -> Attrs -> a

instance attributableVDomM :: Attributable (VDomM a) where
  with (Node el children attrs handlers rest) (Attrs as xs) =
    Node el children (attrs <> as) (handlers <> xs) rest
  with (Content children rest) vattrs = Content children rest
  with (Return a) vattrs = Return a

instance attributableVDomMF :: Attributable (VDomM a -> VDomM a) where
  with k xs m = k m `with` xs

infixl 4 !

(!) :: forall a. (Attributable a) => a -> Attrs -> a
(!) = with
