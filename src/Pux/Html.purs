module Pux.Html
  ( module Elements
  , (!)
  , (#)
  , bind
  , forwardTo
  , withAttr
  , withChildren
  ) where

import Data.Array ((:), singleton)
import Data.Function (Fn2, runFn2)
import Prelude (Unit, ($), unit)
import Pux.Html.Elements as Elements
import Pux.Html.Elements (Html, Attribute)

-- | This version of bind is for appending `Html` using a `do` block.
-- |
-- | ```purescript
-- | import Pux.Html (Html, (#), (!), bind, div, span, button, text)
-- | import Pux.Html.Events (onClick)
-- |
-- | view :: State -> Html Action
-- | view state =
-- |   div # do
-- |     button ! onClick (const Increment) # text "Increment"
-- |     span # text (show count)
-- |     button ! onClick (const Decrement) # text "Decrement"
-- | ```
bind :: forall a. Html a -> (Unit -> Html a) -> Html a
bind x f = runFn2 append x (f unit)

foreign import append :: forall a. Fn2 (Html a) (Html a) (Html a)

-- | Forward child `Html` actions to their parent action. `forwardTo` maps
-- | over `Html` that sends actions of type `a` and returns `Html` that sends
-- | actions of type `b`.
-- |
-- | ```purescript
-- | view :: State -> Html Action
-- | view state =
-- |   div # do
-- |     forwardTo Top $ Counter.view state.topCount
-- |     forwardTo Bottom $ Counter.view state.bottomCount
-- |     button ! onClick (const Reset) # text "Reset"
-- | ```
forwardTo :: forall a b. (a -> b) -> Html a -> Html b
forwardTo a2b html = runFn2 mapActions a2b html

foreign import mapActions :: forall a b. Fn2 (a -> b) (Html a) (Html b)

-- | Combine elements with attributes.
-- |
-- | ```purescript
-- | button ! className "primary" ! onClick (const Increment) # text "Increment"
-- | ```
withAttr :: forall a.
            (Array (Attribute a) -> Array (Html a) -> Html a) ->
            Attribute a ->
            (Array (Attribute a) -> Array (Html a) -> Html a)
withAttr f attr = \attrs children -> f (attr : attrs) children

infixl 4 withAttr as !

-- | Append children to parent element.
-- |
-- | ```purescript
-- | div # do
-- |   button ! onClick (const Increment) # text "Increment"
-- |   span # text ("Counter: " ++ show count)
-- |   button ! onClick (const Decrement) # text "Decrement"
-- | ```
withChildren :: forall a.
                (Array (Attribute a) -> Array (Html a) -> Html a) ->
                Html a ->
                Html a
withChildren f html = f [] $ singleton html

infixl 4 withChildren as #
