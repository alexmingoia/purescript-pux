module App.Style where

import CSS (CSS, backgroundColor, px, rgb)
import CSS.Border (border, borderLeft, borderRadius, borderRight, solid)
import CSS.Color (Color)
import CSS.Common (auto, none)
import CSS.Common (top) as Common
import CSS.Display (absolute, block, display, float, floatLeft, floatRight, inlineBlock, position, relative)
import CSS.Font (bolder, color, fontFamily, fontSize, fontWeight, sansSerif)
import CSS.Geometry (bottom, height, left, lineHeight, margin, marginRight, marginTop, maxWidth, padding, right, top, width)
import CSS.ListStyle.Type (listStyleType)
import CSS.Media (maxWidth, screen) as Media
import CSS.Overflow (overflow, overflowAuto)
import CSS.Property (value)
import CSS.Size (em, pct)
import CSS.String (fromString)
import CSS.Stylesheet (key, query, (?))
import CSS.Text (noneTextDecoration, textDecoration)
import CSS.TextAlign (rightTextAlign, textAlign)
import CSS.VerticalAlign (verticalAlign)
import Color (lighten)
import Control.Bind (bind)
import Data.Function ((#))
import Data.NonEmpty (singleton)

textColor :: Color
textColor = rgb 65 68 74

darkColor :: Color
darkColor = rgb 40 44 52

lightColor :: Color
lightColor = rgb 245 245 245

greenColor :: Color
greenColor = rgb 78 217 166

appStyle :: CSS
appStyle = do
  fromString "body" ? do
    margin (0.0 #px) (0.0 #px) (0.0 #px) (0.0 #px)
    padding (0.0 #px) (0.0 #px) (0.0 #px) (0.0 #px)
    backgroundColor (rgb 253 253 253)
    fontSize (15.0 #px)
    key (fromString "font-family") (value "-apple-system,BlinkMacSystemFont,\"Segoe UI\",Roboto,Oxygen-Sans,Ubuntu,Cantarell,\"Helvetica Neue\",sans-serif")
    lineHeight (165.0 #pct)
    color textColor

  fromString ".strong" ? do
    fontWeight bolder

  fromString "blockquote" ? do
    backgroundColor (rgb 250 250 250)
    overflow overflowAuto
    margin (0.0 #px) (0.0 #px) (1.5 #em) (0.0 #px)
    width (100.0 #pct)

    fromString "p" ? do
      margin (0.0 #em) (0.0 #px) (0.0 #px) (0.0 #px)
      borderLeft solid (4.0 #px) (rgb 220 220 220)
      padding (1.0 #em) (0.0 #px) (1.0 #em) (24.0 #px)

    fromString "h4" ? do
      margin (1.0 #em) (0.0 #px) (0.0 #px) (0.0 #px)
      textAlign rightTextAlign

    fromString "h4:nth-child(1)" ? do
      display inlineBlock
      padding (1.0 #em) (24.0 #px) (1.0 #em) (0.0 #px)
      borderRight solid (4.0 #px) (rgb 220 220 220)
      float floatRight
      textAlign rightTextAlign

    fromString "h4:nth-child(2)" ? do
      borderLeft solid (4.0 #px) (rgb 220 220 220)
      padding (1.0 #em) (0.0 #px) (1.0 #em) (24.0 #px)
      display inlineBlock
      float floatLeft

  fromString "hr" ? do
    backgroundColor (rgb 220 220 220)
    height (1.0 #px)
    border solid (0.0 #px) (rgb 220 220 220)

  fromString "h1, h2, h3, h4" ? do
    lineHeight (140.0 #pct)

  fromString "h1" ? do
    fontSize (28.0 #px)
    margin (0.0 #px) (0.0 #px) (1.0 #em) (0.0 #px)

  fromString "h2" ? do
    fontSize (21.0 #px)
    margin (1.6 #em) (0.0 #px) (0.7 #em) (0.0 #px)

  fromString "h3" ? do
    fontSize (17.0 #px)
    margin (1.8 #em) (0.0 #px) (0.7 #em) (0.0 #px)

  fromString "h4" ? do
    fontSize (17.0 #px)
    margin (2.0 #em) (0.0 #px) (0.7 #em) (0.0 #px)

  fromString "h4" ? fromString "code" ? do
    fontSize (21.0 #px)

  fromString "ul" ? do
    padding (0.0 #px) (0.0 #px) (0.0 #px) (24.0 #px)

  fromString "ol" ? do
    padding (0.0 #px) (0.0 #px) (0.0 #px) (24.0 #px)

  fromString "a" ? do
    color (rgb 134 133 220)
    textDecoration noneTextDecoration

  fromString "a:hover" ? do
    color (rgb 73 204 156)
    textDecoration noneTextDecoration

  fromString "li" ? do
    fromString "code" ? do
      height (17.0 #px)
      lineHeight (17.0 #px)
      padding (0.0 #px) (4.0 #px) (0.0 #px) (4.0 #px)

  fromString "p" ? do
    fromString "code" ? do
      padding (0.0 #px) (4.0 #px) (0.0 #px) (4.0 #px)

  fromString "code" ? do
    display inlineBlock
    borderRadius (2.0 #px) (2.0 #px) (2.0 #px) (2.0 #px)
    lineHeight (1.6 #em)
    fontSize (14.0 #px)
    maxWidth (720.0 #px)
    backgroundColor (rgb 245 245 245)

  fromString "pre" ? fromString "code" ? do
    display block
    backgroundColor (rgb 50 41 49)
    color (rgb 217 212 216)
    overflow overflowAuto
    padding (12.0 #px) (12.0 #px) (12.0 #px) (12.0 #px)

  fromString "p" ? do
    margin (0.0 #px) (0.0 #px) (1.0 #em) (0.0 #px)
    maxWidth (720.0 #px)

  fromString ".main" ? do
    position absolute
    left (248.0 #px)
    right (0.0 #px)
    top (0.0 #px)
    bottom (0.0 #px)
    overflow overflowAuto

    fromString ".inner" ? do
      padding (48.0 #px) (36.0 #px) (16.0 #px) (36.0 #px)

    fromString "h1" ? do
      lineHeight (100.0 #pct)
      marginTop (0.0 #px)

    fromString "li" ? do
      fontSize (15.0 #px)
      margin (0.3 #em) (0.0 #px) (0.3 #em) (0.0 #px)
      listStyleType none

    fromString "li:before" ? do
        fontSize (11.0 #px)
        marginRight (6.0 #px)
        verticalAlign Common.top

    fromString "ol" ? do
      key (fromString "counter-reset") (value "item")

      fromString "li:before" ? do
        color (lighten 0.2 textColor)
        key (fromString "content") (value "counter(item) \". \"")
        key (fromString "counter-increment") (value "item")

    fromString "ul" ? do
      fromString "li:before" ? do
        color (lighten 0.6 textColor)
        key (fromString "content") (value "\"★\"")

  fromString ".homepage" ? do
    fromString ".main" ? do
      fromString "ul" ? do
        fontSize (17.0 #px)

  query Media.screen (singleton (Media.maxWidth (720.0 #px))) do
    fromString ".main" ? do
      position relative
      right (0.0 #px)
      top (0.0 #px)
      left (0.0 #px)
