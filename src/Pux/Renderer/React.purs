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
import Data.Array (fromFoldable)
import Data.CatList (CatList)
import Data.Foldable (foldl)
import Data.Function (($), (<<<))
import Data.Function.Uncurried (Fn5, runFn5)
import Data.Functor (map)
import Data.List (List(..), singleton)
import Data.StrMap (StrMap, empty, insert)
import Data.Unit (Unit)
import Pux.DOM.HTML (HTML)
import Pux.DOM.HTML.Attributes (key)
import React (ReactClass, ReactElement)
import Signal (Signal, (~>))
import Signal.Channel (CHANNEL, Channel, channel, send)
import Text.Smolder.Markup (Attribute, EventHandler(EventHandler), attribute, parent, (!))
import Text.Smolder.Renderer.Util (Node(..), renderMarkup)

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
  pure $ toReact $ markup ~> renderNodes input <<< renderMarkup

-- | Create an HTML constructor for a React class using a unique key. When
-- | rendered this element is replaced with the class.
reactClass :: ∀ ev props. ReactClass props -> String -> (HTML ev -> HTML ev)
reactClass component key' = \children ->
  registerClass component key' $ parent "reactclass" children ! key key'

dangerouslySetInnerHTML :: String -> Attribute
dangerouslySetInnerHTML = attribute "dangerouslySetInnerHTML"

foreign import toReact :: ∀ props. Signal ReactElement -> ReactClass props
foreign import registerClass :: ∀ ev props. ReactClass props -> String -> HTML ev -> HTML ev
foreign import renderToDOM_ :: ∀ props fx. String -> ReactClass props -> Eff fx Unit
foreign import renderToString_ :: ∀ props fx. ReactClass props -> Eff fx String
foreign import renderToStaticMarkup_ :: ∀ props fx. ReactClass props -> Eff fx String
foreign import reactElement :: ∀ a e fx. Fn5 (a -> Eff fx Unit) String (StrMap String) (StrMap e) (Array ReactElement) ReactElement
foreign import reactText :: String -> ReactElement

renderNodes :: ∀ a e. Channel (List a) -> List (Node e) -> ReactElement
renderNodes input nodes = foldl (\prev curr -> renderNode input curr) (reactText "") nodes

renderNode :: ∀ a e. Channel (List a) -> Node e -> ReactElement
renderNode input (Element n a e c) =
  runFn5 reactElement (hook input) n a (toStrMap e) (fromFoldable (map (renderNode input) c))
renderNode input (Text s) =
  reactText s

hook :: ∀ a fx. Channel (List a) -> (a -> Eff (channel :: CHANNEL | fx) Unit)
hook input = \a -> do
  send input (singleton a)

toStrMap :: ∀ a. CatList (EventHandler a) -> StrMap a
toStrMap handlers = foldl (\prev (EventHandler k v) -> insert k v prev) empty handlers
