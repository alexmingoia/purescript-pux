module App.Sidebar where

import App.Style (textColor)
import CSS.Border (borderLeft, solid)
import CSS.Color (rgb)
import CSS.Common (none)
import CSS.Display (fixed, position, relative)
import CSS.Font (color)
import CSS.Geometry (margin, marginLeft, marginTop, padding, paddingLeft, right, top, width)
import CSS.ListStyle.Type (listStyleType)
import CSS.Media (screen)
import CSS.Media (maxWidth) as MediaQuery
import CSS.Size (pct, px, em)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, query, (?))
import CSS.Text (letterSpacing)
import Control.Bind (bind)
import Data.Eq ((==))
import Data.Function ((#), ($))
import Data.NonEmpty (singleton)
import Pux.DOM.HTML (HTML, style)
import Pux.DOM.HTML.Attributes (key)
import Text.Smolder.HTML (a, div, h2, li, nav, ul)
import Text.Smolder.HTML.Attributes (className, href)
import Text.Smolder.Markup (text, (!))

navLink :: forall ev. String -> String -> String -> HTML ev
navLink url _url label =
  li $ do
    if (url == _url) then
      a ! className "selected" ! href _url $ text label
      else
        a ! href _url $ text label

view :: forall ev. String -> HTML ev
view st = do
  style viewStyle ! key "sidebar-style"

  nav ! className "sidebar" $ div ! className "inner" $ do
    h2 $ a ! href "/" $ text "Pux"

    ul do
      li $ navLink st "/" "Introduction"

    ul do
      li $ navLink st "/docs/architecture" "Architecture"
      li $ navLink st "/docs/events" "Events"
      li $ navLink st "/docs/markup" "Markup"
      li $ navLink st "/docs/rendering" "Rendering"
      li $ navLink st "/docs/components" "Components"

    ul do
      li $ navLink st "/docs/forms" "Forms"
      li $ navLink st "/docs/routing" "Routing"
      li $ navLink st "/docs/css" "CSS"
      li $ navLink st "/docs/react-interop" "React Interop"

    ul do
      li $ navLink st "https://pursuit.purescript.org/packages/purescript-pux" "API Reference"
      li $ navLink st "https://github.com/alexmingoia/purescript-pux/tree/master/examples/" "Examples"
      li $ navLink st "https://github.com/alexmingoia/pux-devtool" "Devtool Extension"

    ul do
      li $ navLink st "https://github.com/alexmingoia/purescript-pux/" "GitHub"
      li $ navLink st "https://gitter.im/alexmingoia/purescript-pux" "Chat (Gitter)"

    ul do
      li $ navLink st "/docs/learn-purescript" "Learn PureScript"

viewStyle :: CSS
viewStyle = do
  fromString ".sidebar" ? do
    position fixed
    right (0.0 #px)
    top (100.0 #px)
    width (284.0 #px)

    fromString "h2" ? do
      color (rgb 134 133 220)
      letterSpacing (1.0 #px)
      margin (0.0 #px) (0.0 #px) (0.8 #em) (0.0 #px)
      paddingLeft (15.0 #px)

    fromString "li a" ? do
      borderLeft solid (3.0 #px) (rgb 255 255 255)
      paddingLeft (12.0 #px)
      color textColor

    fromString "a:hover" ? do
      color (rgb 73 204 156)

    fromString ".selected" ? do
      borderLeft solid (3.0 #px) (rgb 78 217 166)

    fromString ".inner" ? do
      borderLeft solid (1.0 #px) (rgb 230 230 230)
      marginLeft (24.0 #px)
      padding (0.0 #px) (36.0 #px) (36.0 #px) (0.0 #px)

    fromString "ul" ? do
      listStyleType none
      marginTop (0.0 #px)
      paddingLeft (0.0 #px)

    fromString "ul ul li a" ? do
      paddingLeft (24.0 #px)

  query screen (singleton (MediaQuery.maxWidth (1140.0 #px))) do
    fromString ".sidebar" ? do
      width (234.0 #px)

  query screen (singleton (MediaQuery.maxWidth (720.0 #px))) do
    fromString ".sidebar" ? do
      position relative
      marginTop (24.0 #px)
      top (0.0 #px)
      width (100.0 #pct)
