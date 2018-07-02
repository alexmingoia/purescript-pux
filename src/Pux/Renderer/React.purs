module Pux.Renderer.React
  ( dangerouslySetInnerHTML
  , renderToDOM
  , renderToString
  , renderToStaticMarkup
  , renderToReact
  , reactClass
  , reactClassWithProps
  ) where

import Prelude

import Effect (Effect)
import Control.Monad.Free (foldFree)
import Control.Monad.State (State, state, execState)
import Data.Array (snoc)
import Data.CatList (CatList)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.List (List(..), singleton)
import Data.Maybe (Maybe(..))
import Data.NaturalTransformation (NaturalTransformation)
import Data.Nullable (Nullable, toNullable)
import Foreign.Object (Object)
import Foreign.Object (fromFoldable) as Object
import Data.Tuple (Tuple(..))
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (data_)
import React (ReactClass, ReactElement)
import Signal (Signal, (~>))
import Signal.Channel (Channel, channel, send)
import Text.Smolder.Markup (Attr(..), Attribute, EventHandler(EventHandler), Markup, MarkupM(..), NS(..), attribute, parent, (!))

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
renderToDOM :: ∀ ev
               .  String
               -> Signal (HTML ev)
               -> Channel (List ev)
               -> Effect Unit
renderToDOM selector markup input =
  renderToDOM_ selector =<< renderToReact markup input

-- | Return an HTML string from a component's HTML signal. The HTML returned
-- | includes React-specific attributes for fast mounting in the browser.
renderToString :: ∀ ev
                  .  Signal (HTML ev)
                  -> Effect String
renderToString markup = do
  input <- channel Nil
  renderToString_ =<< renderToReact markup input

-- | Return an HTML string from a component's HTML signal. The HTML returned is
-- | stripped of all React-specific attributes.
renderToStaticMarkup :: ∀ ev
                        .  Signal (HTML ev)
                        -> Effect String
renderToStaticMarkup markup = do
  input <- channel Nil
  renderToStaticMarkup_ =<< renderToReact markup input

-- | Return a ReactClass from a component's HTML signal.
renderToReact :: ∀ ev props
                 .  Signal (HTML ev)
                 -> Channel (List ev)
                 -> Effect (ReactClass props)
renderToReact markup input =
  pure $ toReact $ markup ~> renderNodes (reactHandler (hook input))

-- | Create an HTML constructor for a React class using a unique name. When
-- | rendered this element is replaced with the class.
reactClass :: ∀ ev props. ReactClass props -> String -> (HTML ev -> HTML ev)
reactClass component key' = \children ->
  registerClass component key'
    $ parent HTMLns "reactclass" children ! (data_ "pux-react-class" key')

-- | Create an HTML constructor for a React class using a unique name. When
-- | rendered this element is replaced with the class. The returned constructor
-- | takes an arbitrary props argument, which will be passed to the React class
-- | when rendered.
reactClassWithProps :: ∀ ev props. ReactClass props -> String -> (props -> HTML ev -> HTML ev)
reactClassWithProps component key' = \props children ->
  registerClass component key'
    $ parent HTMLns "reactclass" children
      ! registerProps props (data_ "pux-react-props") ! data_ "pux-react-class" key'

dangerouslySetInnerHTML :: String -> Attribute
dangerouslySetInnerHTML = attribute "dangerouslySetInnerHTML"

foreign import toReact :: ∀ props. Signal (Array ReactElement) -> ReactClass props
foreign import registerClass :: ∀ ev props. ReactClass props -> String -> HTML ev -> HTML ev
foreign import registerProps :: ∀ props. props -> (String -> Attribute) -> Attribute
foreign import renderToDOM_ :: ∀ props. String -> ReactClass props -> Effect Unit
foreign import renderToString_ :: ∀ props. ReactClass props -> Effect String
foreign import renderToStaticMarkup_ :: ∀ props. ReactClass props -> Effect String
foreign import reactElement :: Fn3 String (Object ReactAttribute) (Nullable (Array ReactElement)) ReactElement
foreign import reactText :: String -> ReactElement
foreign import reactHandler :: ∀ a e. (a -> Effect Unit) -> e -> ReactAttribute
foreign import reactAttr :: String -> ReactAttribute

foreign import data ReactAttribute :: Type

renderItem :: ∀ e. (e -> ReactAttribute) -> NaturalTransformation (MarkupM e) (State (Array ReactElement))
renderItem input (Element _ n c a e r) =
  let kids = renderNodes input c
      el = runFn3 reactElement n (renderAttrs input a e) (toNullable (Just kids))
  in state \s -> Tuple r $ snoc s el
renderItem input (Content t r) =
  state \s -> Tuple r $ snoc s $ reactText t
renderItem input (Empty r) = pure r

renderNodes :: ∀ e. (e -> ReactAttribute) -> Markup e -> Array ReactElement
renderNodes input markup = execState (foldFree (renderItem input) markup) []

renderAttrs :: ∀ e. (e -> ReactAttribute) -> CatList Attr -> CatList (EventHandler e) -> Object ReactAttribute
renderAttrs input attrs handlers = Object.fromFoldable tuples
  where
  tuples = map toTupleA attrs <> map toTupleH handlers
  toTupleH (EventHandler key value) = Tuple key (input value)
  toTupleA (Attr key value) = Tuple key (reactAttr value)

hook :: ∀ a. Channel (List a) -> (a -> Effect Unit)
hook input = \a -> do
  send input (singleton a)
