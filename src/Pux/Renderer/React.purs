module Pux.Renderer.React
  ( dangerouslySetInnerHTML
  , renderToDOM
  , renderToString
  , renderToStaticMarkup
  , renderToReact
  , reactClass
  , reactClassWithProps
  ) where

import Control.Applicative (pure)
import Control.Bind (bind, (=<<))
import Control.Monad.Eff (Eff)
import Data.Array ((:))
import Data.CatList (CatList)
import Data.Function (($))
import Data.Function.Uncurried (Fn4, runFn4)
import Data.Functor ((<$>), map)
import Data.List (List(..), singleton)
import Data.Nullable (Nullable, toNullable)
import Data.Semigroup ((<>))
import Data.StrMap (fromFoldable) as StrMap
import Data.StrMap (StrMap)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (data_)
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
  pure $ toReact $ markup ~> renderNodes (reactHandler (hook input))

-- | Create an HTML constructor for a React class using a unique name. When
-- | rendered this element is replaced with the class.
reactClass :: ∀ ev props. ReactClass props -> String -> (HTML ev -> HTML ev)
reactClass component key' = \children ->
  registerClass component key'
    $ parent "reactclass" children ! (data_ "pux-react-class" key')

-- | Create an HTML constructor for a React class using a unique name. When
-- | rendered this element is replaced with the class. The returned constructor
-- | takes an arbitrary props argument, which will be passed to the React class
-- | when rendered.
reactClassWithProps :: ∀ ev props. ReactClass props -> String -> (props -> HTML ev -> HTML ev)
reactClassWithProps component key' = \props children ->
  registerClass component key'
    $ parent "reactclass" children
      ! registerProps props (data_ "pux-react-props") ! data_ "pux-react-class" key'

dangerouslySetInnerHTML :: String -> Attribute
dangerouslySetInnerHTML = attribute "dangerouslySetInnerHTML"

foreign import toReact :: ∀ props. Signal (Array ReactElement) -> ReactClass props
foreign import registerClass :: ∀ ev props. ReactClass props -> String -> HTML ev -> HTML ev
foreign import registerProps :: ∀ props. props -> (String -> Attribute) -> Attribute
foreign import renderToDOM_ :: ∀ props fx. String -> ReactClass props -> Eff fx Unit
foreign import renderToString_ :: ∀ props fx. ReactClass props -> Eff fx String
foreign import renderToStaticMarkup_ :: ∀ props fx. ReactClass props -> Eff fx String
foreign import reactElement :: ∀ e. Fn4 (Markup e) String (StrMap ReactAttribute) (Nullable (Array ReactElement)) ReactElement
foreign import reactText :: String -> ReactElement
foreign import reactHandler :: ∀ a e fx. (a -> Eff (channel :: CHANNEL | fx) Unit) -> e -> ReactAttribute
foreign import reactAttr :: String -> ReactAttribute

foreign import data ReactAttribute :: Type

renderNodes :: ∀ e. (e -> ReactAttribute) -> Markup e -> Array ReactElement
renderNodes input node@(Element n (Just (Return _)) a e r) =
  runFn4 reactElement node n (renderAttrs input a e) (toNullable Nothing) : renderNodes input r
renderNodes input node@(Element n c a e r) =
  runFn4 reactElement node n (renderAttrs input a e) (toNullable (renderNodes input <$> c)) : renderNodes input r
renderNodes input (Content t r) =
  reactText t : renderNodes input r
renderNodes input (Return _) = []

renderAttrs :: ∀ e. (e -> ReactAttribute) -> CatList Attr -> CatList (EventHandler e) -> StrMap ReactAttribute
renderAttrs input attrs handlers = StrMap.fromFoldable tuples
  where
  tuples = map toTupleA attrs <> map toTupleH handlers
  toTupleH (EventHandler key value) = Tuple key (input value)
  toTupleA (Attr key value) = Tuple key (reactAttr value)

hook :: ∀ a fx. Channel (List a) -> (a -> Eff (channel :: CHANNEL | fx) Unit)
hook input = \a -> do
  send input (singleton a)
