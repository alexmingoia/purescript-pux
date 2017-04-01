module Pux.Renderer.React
  ( dangerouslySetInnerHTML
  , renderToDOM
  , renderToString
  , renderToStaticMarkup
  , renderToReact
  , reactClass
  ) where

import Control.Applicative (pure)
import Control.Bind (bind, (=<<))
import Control.Monad.Eff (Eff)
import Data.Array ((:))
import Data.CatList (CatList)
import Data.Foldable (foldl)
import Data.Function (($), (>>>))
import Data.Function.Uncurried (Fn7, runFn7)
import Data.Functor (map)
import Data.List (List(..), singleton)
import Data.Nullable (Nullable, toNullable)
import Data.StrMap (fromFoldable) as StrMap
import Data.StrMap (StrMap, empty, insert)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (key)
import React (ReactClass, ReactElement)
import Signal (Signal, (~>))
import Signal.Channel (CHANNEL, Channel, channel, send)
import Text.Smolder.Markup (Attr(..), Attribute, EventHandler(EventHandler), Markup, MarkupM(..), attribute, parent, (!))

-- | ```purescript
-- | main = do
-- |  app <- start
-- |    { initialState
-- |    , view
-- |    , foldp
-- |    , inputs: [] }
-- |
-- |  renderToDOM "#app" app.markup app.input
-- | ```
renderToDOM :: ∀ ev fx
               .  String
               -> Signal (HTML ev)
               -> Channel (List ev)
               -> Eff (channel :: CHANNEL | fx) Unit
renderToDOM selector markup input =
  renderToDOM_ selector =<< renderToReact markup input

-- | Return an HTML string from a component's HTML signal. The HTML returned
-- | includes React-specific attributes for fast mounting in the browser.
renderToString :: ∀ ev fx
                  .  Signal (HTML ev)
                  -> Eff (channel :: CHANNEL | fx) String
renderToString markup = do
  input <- channel Nil
  renderToString_ =<< renderToReact markup input

-- | Return an HTML string from a component's HTML signal. The HTML returned is
-- | stripped of all React-specific attributes.
renderToStaticMarkup :: ∀ ev fx
                        .  Signal (HTML ev)
                        -> Eff (channel :: CHANNEL | fx) String
renderToStaticMarkup markup = do
  input <- channel Nil
  renderToStaticMarkup_ =<< renderToReact markup input

-- | Return a ReactClass from a component's HTML signal.
renderToReact :: ∀ ev props fx
                 .  Signal (HTML ev)
                 -> Channel (List ev)
                 -> Eff (channel :: CHANNEL | fx) (ReactClass props)
renderToReact markup input =
  pure $ toReact $ markup ~> renderNodes (hook input)

-- | Create an HTML constructor for a React class using a unique key. When
-- | rendered this element is replaced with the class.
reactClass :: ∀ ev props. ReactClass props -> String -> (HTML ev -> HTML ev)
reactClass component key' = \children ->
  registerClass component key' $ parent "reactclass" children ! key key'

dangerouslySetInnerHTML :: String -> Attribute
dangerouslySetInnerHTML = attribute "dangerouslySetInnerHTML"

foreign import toReact :: ∀ props. Signal (Array ReactElement) -> ReactClass props
foreign import registerClass :: ∀ ev props. ReactClass props -> String -> HTML ev -> HTML ev
foreign import renderToDOM_ :: ∀ props fx. String -> ReactClass props -> Eff fx Unit
foreign import renderToString_ :: ∀ props fx. ReactClass props -> Eff fx String
foreign import renderToStaticMarkup_ :: ∀ props fx. ReactClass props -> Eff fx String
foreign import reactElement :: ∀ a e fx. Fn7 ((a -> Eff fx Unit) -> Markup e -> Array ReactElement) (Markup e) (a -> Eff fx Unit) String (StrMap String) (StrMap e) (Nullable (Markup e)) ReactElement
foreign import reactText :: String -> ReactElement

renderNodes :: ∀ a e fx. (a -> Eff (channel :: CHANNEL | fx) Unit) -> Markup e -> Array ReactElement
renderNodes input node@(Element n c a e r) =
  runFn7 reactElement renderNodes node input n (renderAttrs a) (toStrMap e) (toNullable c) : renderNodes input r
renderNodes input (Content t r) =
  reactText t : renderNodes input r
renderNodes input (Return _) = []

renderAttrs :: CatList Attr -> StrMap String
renderAttrs = map toTuple >>> StrMap.fromFoldable
  where
  toTuple (Attr key value) = Tuple key value

hook :: ∀ a fx. Channel (List a) -> (a -> Eff (channel :: CHANNEL | fx) Unit)
hook input = \a -> do
  send input (singleton a)

toStrMap :: ∀ a. CatList (EventHandler a) -> StrMap a
toStrMap handlers = foldl (\prev (EventHandler k v) -> insert k v prev) empty handlers
