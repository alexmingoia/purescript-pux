module App.Sidebar where

import App.Style (textColor, lightColor, darkColor, greenColor)
import CSS (backgroundColor)
import CSS.Border (borderLeft, solid)
import CSS.Color (rgb)
import CSS.Common (none)
import CSS.Display (absolute, position, relative)
import CSS.Font (color, fontWeight, lighter)
import CSS.Geometry (lineHeight, margin, marginLeft, marginTop, padding, paddingLeft, left, bottom, right, top, width)
import CSS.ListStyle.Type (listStyleType)
import CSS.Media (screen)
import CSS.Media (maxWidth) as MediaQuery
import CSS.Size (pct, px, em)
import CSS.String (fromString)
import CSS.Stylesheet (CSS, query, (?))
import CSS.Text (letterSpacing)
import CSS.Text.Transform (textTransform, uppercase)
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
      li $ navLink st "/docs/architecture" "Architecture"
      li $ navLink st "/docs/events" "Events"
      li $ navLink st "/docs/markup" "Markup"
      li $ navLink st "/docs/rendering" "Rendering"
      li $ navLink st "/docs/components" "Components"
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
    backgroundColor darkColor
    position absolute
    left (0.0 #px)
    top (0.0 #px)
    bottom (0.0 #px)
    width (248.0 #px)

    fromString "h2" ? do
      letterSpacing (1.0 #px)
      margin (0.0 #px) (0.0 #px) (32.0 #px) (0.0 #px)
      paddingLeft (15.0 #px)
      lineHeight (100.0 #pct)
      textTransform uppercase

      fromString "a" ? do
        color lightColor

    fromString "li a" ? do
      borderLeft solid (3.0 #px) darkColor
      paddingLeft (12.0 #px)
      color lightColor
      fontWeight lighter

    fromString "a:hover" ? do
      color greenColor

    fromString ".selected" ? do
      borderLeft solid (3.0 #px) greenColor

    fromString ".inner" ? do
      marginLeft (24.0 #px)
      padding (52.0 #px) (16.0 #px) (16.0 #px) (16.0 #px)

    fromString "ul" ? do
      listStyleType none
      marginTop (0.0 #px)
      paddingLeft (0.0 #px)

    fromString "ul ul li a" ? do
      paddingLeft (24.0 #px)

  query screen (singleton (MediaQuery.maxWidth (720.0 #px))) do
    fromString ".sidebar" ? do
      position relative
      top (0.0 #px)
      width (100.0 #pct)
