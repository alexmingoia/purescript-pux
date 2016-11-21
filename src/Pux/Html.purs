module Pux.Html
  ( module Elements
  , (!)
  , (#)
  , (#>)
  , (##)
  , withAttr
  , withTextChild
  , withChild
  , withChildren
  ) where

import Data.Array ((:), singleton)
import Pux.Html.Elements as Elements
import Pux.Html.Elements (Html, Attribute)

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

infixl 1 withAttr as !

-- | Append child to parent element.
-- |
-- | ```purescript
-- | div ##
-- |   [ button ! onClick (const Increment) # text "Increment"
-- |   , span # text ("Counter: " <> show count)
-- |   , button ! onClick (const Decrement) # text "Decrement"
-- |   ]
-- | ```
withChild :: forall a.
                (Array (Attribute a) -> Array (Html a) -> Html a) ->
                Html a ->
                Html a
withChild f html = f [] (singleton html)

infixr 0 withChild as #

-- | Append a single text child to parent element.
-- |
-- | Cleans up repetitive `# text "foo"` usage.
-- | Here is the previous example again with `#>`:
-- |
-- | ```purescript
-- | div ##
-- |   [ button ! onClick (const Increment) #> "Increment"
-- |   , span #> "Counter: " <> show count
-- |   , button ! onClick (const Decrement) #> "Decrement"
-- |   ]
-- | ```
withTextChild :: forall a.
             (Array (Attribute a) -> Array (Html a) -> Html a) ->
             String ->
             Html a
withTextChild f txt = f # Elements.text txt

infixr 0 withTextChild as #>

withChildren :: forall a.
             (Array (Attribute a) -> Array (Html a) -> Html a) ->
             Array (Html a) ->
             Html a
withChildren f htmls = f [] htmls

infixr 0 withChildren as ##
