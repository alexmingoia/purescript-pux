module Pux.Html
  ( module Elements
  , (!)
  , (#)
  , (##)
  , bind
  , withAttr
  , withChild
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

-- | Append child to parent element.
-- |
-- | ```purescript
-- | div # do
-- |   button ! onClick (const Increment) # text "Increment"
-- |   span # text ("Counter: " ++ show count)
-- |   button ! onClick (const Decrement) # text "Decrement"
-- | ```
withChild :: forall a.
                (Array (Attribute a) -> Array (Html a) -> Html a) ->
                Html a ->
                Html a
withChild f html = f [] $ singleton html

infixl 4 withChild as #

withChildren :: forall a.
             (Array (Attribute a) -> Array (Html a) -> Html a) ->
             Array (Html a) ->
             Html a
withChildren f htmls = f [] htmls

infixl 4 withChildren as ##
