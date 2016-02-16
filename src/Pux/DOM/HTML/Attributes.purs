-- | Example:
-- |
-- | ```purescript
-- | import Pux
-- | import Pux.DOM.HTML.Elements (div, p, button, text)
-- | import Pux.DOM.HTML.Attributes (onClick, send)
-- |
-- | view :: State -> VirtualDOM
-- | view state = div $ do
-- |   p $ text ("Counter: " ++ show state.counter)
-- |   p $ do
-- |     button ! onClick (send Increment) $ text "Increment"
-- |     button ! onClick (send Decrement) $ text "Decrement"
-- | ```

module Pux.DOM.HTML.Attributes where

import Data.List (List(Nil), singleton)
import DOM (DOM())
import Prelude (($), map)
import Pux.DOM (Attrs, Handler(Handler))
import Pux.React (makeAttr, makeAttrWithObj, makeHandler, stopPropagationFF, preventDefaultFF)
import Pux.React.Types (Event)
import Signal.Channel (CHANNEL())

type MouseEvent =
  { pageX :: Number
  , pageY :: Number
  }

type KeyboardEvent =
  { altKey   :: Boolean
  , ctrlKey  :: Boolean
  , charCode :: Int
  , key      :: String
  , keyCode  :: Int
  , locale   :: String
  , location :: Int
  , metaKey  :: Boolean
  , repeat   :: Boolean
  , shiftKey :: Boolean
  , which    :: Int
  }

-- | ```purescript
-- | button ! onClick (send Increment)
-- | ```
send :: forall ev action eff. action -> Handler ev action eff
send a = Handler (singleton a) Nil

-- | ```purescript
-- | form ! onSubmit preventDefault
-- | ```
preventDefault :: forall ev action eff. Handler ev action (dom :: DOM | eff)
preventDefault = Handler Nil $ singleton preventDefaultFF

-- | ```purescript
-- | button ! onClick (send Increment <> stopPropagation)
-- | ```
stopPropagation :: forall ev action eff. Handler ev action (dom :: DOM | eff)
stopPropagation = Handler Nil $ singleton stopPropagationFF

onCopy :: forall action eff. Handler Event action (dom :: DOM, channel :: CHANNEL | eff) -> Attrs
onCopy (Handler actions effects) = makeHandler "onCopy" effects $ \ev -> actions

onCut :: forall action eff. Handler Event action (dom :: DOM, channel :: CHANNEL | eff) -> Attrs
onCut (Handler actions effects) = makeHandler "onCut" effects $ \ev -> actions

onPaste :: forall action eff. Handler Event action (dom :: DOM, channel :: CHANNEL | eff) -> Attrs
onPaste (Handler actions effects) = makeHandler "onPaste" effects $ \ev -> actions

onKeyDown :: forall action eff. Handler KeyboardEvent (KeyboardEvent -> action) (dom :: DOM, channel :: CHANNEL | eff) -> Attrs
onKeyDown (Handler actions effects) = makeHandler "onKeyDown" effects $ \ev -> map (\a -> a ev) actions

onKeyPress :: forall action eff. Handler KeyboardEvent (KeyboardEvent -> action) (dom :: DOM, channel :: CHANNEL | eff) -> Attrs
onKeyPress (Handler actions effects) = makeHandler "onKeyPress" effects $ \ev -> map (\a -> a ev) actions

onKeyUp :: forall action eff. Handler KeyboardEvent (KeyboardEvent -> action) (dom :: DOM, channel :: CHANNEL | eff) -> Attrs
onKeyUp (Handler actions effects) = makeHandler "onKeyUp" effects $ \ev -> map (\a -> a ev) actions

onFocus :: forall action eff. Handler KeyboardEvent (KeyboardEvent -> action) (dom :: DOM, channel :: CHANNEL | eff) -> Attrs
onFocus (Handler actions effects) = makeHandler "onFocus" effects $ \ev -> actions

onBlur :: forall action eff. Handler KeyboardEvent (KeyboardEvent -> action) (dom :: DOM, channel :: CHANNEL | eff) -> Attrs
onBlur (Handler actions effects) = makeHandler "onBlur" effects $ \ev -> actions

onChange :: forall action eff. Handler KeyboardEvent (KeyboardEvent -> action) (dom :: DOM, channel :: CHANNEL | eff) -> Attrs
onChange (Handler actions effects) = makeHandler "onChange" effects $ \ev -> actions

onInput :: forall action eff. Handler KeyboardEvent (KeyboardEvent -> action) (dom :: DOM, channel :: CHANNEL | eff) -> Attrs
onInput (Handler actions effects) = makeHandler "onInput" effects $ \ev -> actions

onSubmit :: forall action eff. Handler KeyboardEvent (KeyboardEvent -> action) (dom :: DOM, channel :: CHANNEL | eff) -> Attrs
onSubmit (Handler actions effects) = makeHandler "onSubmit" effects $ \ev -> actions

onClick :: forall action eff. Handler Event action (dom :: DOM, channel :: CHANNEL | eff) -> Attrs
onClick (Handler actions effects) = makeHandler "onClick" effects $ \ev -> actions

onDoubleClick :: forall action eff. Handler Event action (dom :: DOM, channel :: CHANNEL | eff) -> Attrs
onDoubleClick (Handler actions effects) = makeHandler "onDoubleClick" effects $ \ev -> actions

onDrag :: forall action eff. Handler Event action (dom :: DOM, channel :: CHANNEL | eff) -> Attrs
onDrag (Handler actions effects) = makeHandler "onDrag" effects $ \ev -> actions

onDragEnd :: forall action eff. Handler Event action (dom :: DOM, channel :: CHANNEL | eff) -> Attrs
onDragEnd (Handler actions effects) = makeHandler "onDragEnd" effects $ \ev -> actions

onDragEnter :: forall action eff. Handler Event action (dom :: DOM, channel :: CHANNEL | eff) -> Attrs
onDragEnter (Handler actions effects) = makeHandler "onDragEnter" effects $ \ev -> actions

onDragExit :: forall action eff. Handler Event action (dom :: DOM, channel :: CHANNEL | eff) -> Attrs
onDragExit (Handler actions effects) = makeHandler "onDragExit" effects $ \ev -> actions

onDragLeave :: forall action eff. Handler Event action (dom :: DOM, channel :: CHANNEL | eff) -> Attrs
onDragLeave (Handler actions effects) = makeHandler "onDragLeave" effects $ \ev -> actions

onDragOver :: forall action eff. Handler Event action (dom :: DOM, channel :: CHANNEL | eff) -> Attrs
onDragOver (Handler actions effects) = makeHandler "onDragOver" effects $ \ev -> actions

onDragStart :: forall action eff. Handler Event action (dom :: DOM, channel :: CHANNEL | eff) -> Attrs
onDragStart (Handler actions effects) = makeHandler "onDragStart" effects $ \ev -> actions

onDrop :: forall action eff. Handler Event action (dom :: DOM, channel :: CHANNEL | eff) -> Attrs
onDrop (Handler actions effects) = makeHandler "onDrop" effects $ \ev -> actions

onMouseDown :: forall action eff. Handler MouseEvent (MouseEvent -> action) (dom :: DOM, channel :: CHANNEL | eff) -> Attrs
onMouseDown (Handler actions effects) = makeHandler "onMouseDown" effects $ \ev -> map (\a -> a ev) actions

onMouseEnter :: forall action eff. Handler MouseEvent (MouseEvent -> action) (dom :: DOM, channel :: CHANNEL | eff) -> Attrs
onMouseEnter (Handler actions effects) = makeHandler "onMouseEnter" effects $ \ev -> map (\a -> a ev) actions

onMouseLeave :: forall action eff. Handler MouseEvent (MouseEvent -> action) (dom :: DOM, channel :: CHANNEL | eff) -> Attrs
onMouseLeave (Handler actions effects) = makeHandler "onMouseLeave" effects $ \ev -> map (\a -> a ev) actions

onMouseMove :: forall action eff. Handler MouseEvent (MouseEvent -> action) (dom :: DOM, channel :: CHANNEL | eff) -> Attrs
onMouseMove (Handler actions effects) = makeHandler "onMouseMove" effects $ \ev -> map (\a -> a ev) actions

onMouseOut :: forall action eff. Handler MouseEvent (MouseEvent -> action) (dom :: DOM, channel :: CHANNEL | eff) -> Attrs
onMouseOut (Handler actions effects) = makeHandler "onMouseOut" effects $ \ev -> map (\a -> a ev) actions

onMouseOver :: forall action eff. Handler MouseEvent (MouseEvent -> action) (dom :: DOM, channel :: CHANNEL | eff) -> Attrs
onMouseOver (Handler actions effects) = makeHandler "onMouseOver" effects $ \ev -> map (\a -> a ev) actions

onMouseUp :: forall action eff. Handler MouseEvent (MouseEvent -> action) (dom :: DOM, channel :: CHANNEL | eff) -> Attrs
onMouseUp (Handler actions effects) = makeHandler "onMouseUp" effects $ \ev -> map (\a -> a ev) actions

onTouchCancel :: forall action eff. Handler Event action (dom :: DOM, channel :: CHANNEL | eff) -> Attrs
onTouchCancel (Handler actions effects) = makeHandler "onTouchCancel" effects $ \ev -> actions

onTouchEnd :: forall action eff. Handler Event action (dom :: DOM, channel :: CHANNEL | eff) -> Attrs
onTouchEnd (Handler actions effects) = makeHandler "onTouchEnd" effects $ \ev -> actions

onTouchMove :: forall action eff. Handler Event action (dom :: DOM, channel :: CHANNEL | eff) -> Attrs
onTouchMove (Handler actions effects) = makeHandler "onTouchMove" effects $ \ev -> actions

onTouchStart :: forall action eff. Handler Event action (dom :: DOM, channel :: CHANNEL | eff) -> Attrs
onTouchStart (Handler actions effects) = makeHandler "onTouchStart" effects $ \ev -> actions

onScroll :: forall action eff. Handler Event action (dom :: DOM, channel :: CHANNEL | eff) -> Attrs
onScroll (Handler actions effects) = makeHandler "onScroll" effects $ \ev -> actions

onWheel :: forall action eff. Handler Event action (dom :: DOM, channel :: CHANNEL | eff) -> Attrs
onWheel (Handler actions effects) = makeHandler "onWheel" effects $ \ev -> actions

aria :: forall ariaAttrs. { | ariaAttrs } -> Attrs
aria = makeAttrWithObj "aria"

data_ :: forall dataAttrs. { | dataAttrs } -> Attrs
data_ = makeAttrWithObj "data"

style :: forall style. { | style } -> Attrs
style = makeAttrWithObj "style"

dangerouslySetInnerHTML :: { __html :: String } -> Attrs
dangerouslySetInnerHTML = makeAttr "dangerouslySetInnerHTML"

accept :: String -> Attrs
accept = makeAttr "accept"

acceptCharset :: String -> Attrs
acceptCharset = makeAttr "acceptCharset"

accessKey :: String -> Attrs
accessKey = makeAttr "accessKey"

action :: String -> Attrs
action = makeAttr "action"

allowFullScreen :: String -> Attrs
allowFullScreen = makeAttr "allowFullScreen"

allowTransparency :: String -> Attrs
allowTransparency = makeAttr "allowTransparency"

alt :: String -> Attrs
alt = makeAttr "alt"

async :: String -> Attrs
async = makeAttr "async"

autoComplete :: String -> Attrs
autoComplete = makeAttr "autoComplete"

autoFocus :: Boolean -> Attrs
autoFocus = makeAttr "autoFocus"

autoPlay :: String -> Attrs
autoPlay = makeAttr "autoPlay"

cellPadding :: String -> Attrs
cellPadding = makeAttr "cellPadding"

cellSpacing :: String -> Attrs
cellSpacing = makeAttr "cellSpacing"

charSet :: String -> Attrs
charSet = makeAttr "charSet"

checked :: String -> Attrs
checked = makeAttr "checked"

classID :: String -> Attrs
classID = makeAttr "classID"

className :: String -> Attrs
className = makeAttr "className"

cols :: String -> Attrs
cols = makeAttr "cols"

colSpan :: String -> Attrs
colSpan = makeAttr "colSpan"

content :: String -> Attrs
content = makeAttr "content"

contentEditable :: String -> Attrs
contentEditable = makeAttr "contentEditable"

contextMenu :: String -> Attrs
contextMenu = makeAttr "contextMenu"

controls :: String -> Attrs
controls = makeAttr "controls"

coords :: String -> Attrs
coords = makeAttr "coords"

crossOrigin :: String -> Attrs
crossOrigin = makeAttr "crossOrigin"

dateTime :: String -> Attrs
dateTime = makeAttr "dateTime"

defer :: String -> Attrs
defer = makeAttr "defer"

dir :: String -> Attrs
dir = makeAttr "dir"

disabled :: Boolean -> Attrs
disabled = makeAttr "disabled"

download :: String -> Attrs
download = makeAttr "download"

draggable :: String -> Attrs
draggable = makeAttr "draggable"

encType :: String -> Attrs
encType = makeAttr "encType"

form :: String -> Attrs
form = makeAttr "form"

formAction :: String -> Attrs
formAction = makeAttr "formAction"

formEncType :: String -> Attrs
formEncType = makeAttr "formEncType"

formMethod :: String -> Attrs
formMethod = makeAttr "formMethod"

formNoValidate :: String -> Attrs
formNoValidate = makeAttr "formNoValidate"

formTarget :: String -> Attrs
formTarget = makeAttr "formTarget"

frameBorder :: String -> Attrs
frameBorder = makeAttr "frameBorder"

height :: String -> Attrs
height = makeAttr "height"

hidden :: String -> Attrs
hidden = makeAttr "hidden"

href :: String -> Attrs
href = makeAttr "href"

hrefLang :: String -> Attrs
hrefLang = makeAttr "hrefLang"

htmlFor :: String -> Attrs
htmlFor = makeAttr "htmlFor"

httpEquiv :: String -> Attrs
httpEquiv = makeAttr "httpEquiv"

icon :: String -> Attrs
icon = makeAttr "icon"

id_ :: String -> Attrs
id_ = makeAttr "id"

key :: String -> Attrs
key = makeAttr "key"

label :: String -> Attrs
label = makeAttr "label"

lang :: String -> Attrs
lang = makeAttr "lang"

list :: String -> Attrs
list = makeAttr "list"

loop :: String -> Attrs
loop = makeAttr "loop"

manifest :: String -> Attrs
manifest = makeAttr "manifest"

marginHeight :: String -> Attrs
marginHeight = makeAttr "marginHeight"

marginWidth :: String -> Attrs
marginWidth = makeAttr "marginWidth"

max :: String -> Attrs
max = makeAttr "max"

maxLength :: String -> Attrs
maxLength = makeAttr "maxLength"

media :: String -> Attrs
media = makeAttr "media"

mediaGroup :: String -> Attrs
mediaGroup = makeAttr "mediaGroup"

method :: String -> Attrs
method = makeAttr "method"

min :: String -> Attrs
min = makeAttr "min"

multiple :: String -> Attrs
multiple = makeAttr "multiple"

muted :: String -> Attrs
muted = makeAttr "muted"

name :: String -> Attrs
name = makeAttr "name"

noValidate :: String -> Attrs
noValidate = makeAttr "noValidate"

open :: String -> Attrs
open = makeAttr "open"

pattern :: String -> Attrs
pattern = makeAttr "pattern"

placeholder :: String -> Attrs
placeholder = makeAttr "placeholder"

poster :: String -> Attrs
poster = makeAttr "poster"

preload :: String -> Attrs
preload = makeAttr "preload"

radioGroup :: String -> Attrs
radioGroup = makeAttr "radioGroup"

readOnly :: String -> Attrs
readOnly = makeAttr "readOnly"

rel :: String -> Attrs
rel = makeAttr "rel"

required :: String -> Attrs
required = makeAttr "required"

role :: String -> Attrs
role = makeAttr "role"

rows :: String -> Attrs
rows = makeAttr "rows"

rowSpan :: String -> Attrs
rowSpan = makeAttr "rowSpan"

sandbox :: String -> Attrs
sandbox = makeAttr "sandbox"

scope :: String -> Attrs
scope = makeAttr "scope"

scrolling :: String -> Attrs
scrolling = makeAttr "scrolling"

seamless :: String -> Attrs
seamless = makeAttr "seamless"

selected :: String -> Attrs
selected = makeAttr "selected"

shape :: String -> Attrs
shape = makeAttr "shape"

size :: String -> Attrs
size = makeAttr "size"

sizes :: String -> Attrs
sizes = makeAttr "sizes"

span_ :: String -> Attrs
span_ = makeAttr "span"

spellCheck :: String -> Attrs
spellCheck = makeAttr "spellCheck"

src :: String -> Attrs
src = makeAttr "src"

srcDoc :: String -> Attrs
srcDoc = makeAttr "srcDoc"

srcSet :: String -> Attrs
srcSet = makeAttr "srcSet"

start :: String -> Attrs
start = makeAttr "start"

step :: String -> Attrs
step = makeAttr "step"

tabIndex :: String -> Attrs
tabIndex = makeAttr "tabIndex"

target :: String -> Attrs
target = makeAttr "target"

title :: String -> Attrs
title = makeAttr "title"

type_ :: String -> Attrs
type_ = makeAttr "type"

useMap :: String -> Attrs
useMap = makeAttr "useMap"

value :: String -> Attrs
value = makeAttr "value"

width :: String -> Attrs
width = makeAttr "width"

wmode :: String -> Attrs
wmode = makeAttr "wmode"

clipPath :: String -> Attrs
clipPath = makeAttr "clipPath"

cx :: String -> Attrs
cx = makeAttr "cx"

cy :: String -> Attrs
cy = makeAttr "cy"

d :: String -> Attrs
d = makeAttr "d"

dx :: String -> Attrs
dx = makeAttr "dx"

dy :: String -> Attrs
dy = makeAttr "dy"

fill :: String -> Attrs
fill = makeAttr "fill"

fillOpacity :: String -> Attrs
fillOpacity = makeAttr "fillOpacity"

fontFamily :: String -> Attrs
fontFamily = makeAttr "fontFamily"

fontSize :: String -> Attrs
fontSize = makeAttr "fontSize"

fx :: String -> Attrs
fx = makeAttr "effects"

fy :: String -> Attrs
fy = makeAttr "fy"

gradientTransform :: String -> Attrs
gradientTransform = makeAttr "gradientTransform"

gradientUnits :: String -> Attrs
gradientUnits = makeAttr "gradientUnits"

markerEnd :: String -> Attrs
markerEnd = makeAttr "markerEnd"

markerMid :: String -> Attrs
markerMid = makeAttr "markerMid"

markerStart :: String -> Attrs
markerStart = makeAttr "markerStart"

offset :: String -> Attrs
offset = makeAttr "offset"

opacity :: String -> Attrs
opacity = makeAttr "opacity"

patternContentUnits :: String -> Attrs
patternContentUnits = makeAttr "patternContentUnits"

patternUnits :: String -> Attrs
patternUnits = makeAttr "patternUnits"

points :: String -> Attrs
points = makeAttr "points"

preserveAspectRatio :: String -> Attrs
preserveAspectRatio = makeAttr "preserveAspectRatio"

r :: String -> Attrs
r = makeAttr "r"

rx :: String -> Attrs
rx = makeAttr "rx"

ry :: String -> Attrs
ry = makeAttr "ry"

spreadMethod :: String -> Attrs
spreadMethod = makeAttr "spreadMethod"

stopColor :: String -> Attrs
stopColor = makeAttr "stopColor"

stopOpacity :: String -> Attrs
stopOpacity = makeAttr "stopOpacity"

stroke :: String -> Attrs
stroke = makeAttr "stroke"

strokeDasharray :: String -> Attrs
strokeDasharray = makeAttr "strokeDasharray"

strokeLinecap :: String -> Attrs
strokeLinecap = makeAttr "strokeLinecap"

strokeOpacity :: String -> Attrs
strokeOpacity = makeAttr "strokeOpacity"

strokeWidth :: String -> Attrs
strokeWidth = makeAttr "strokeWidth"

textAnchor :: String -> Attrs
textAnchor = makeAttr "textAnchor"

transform :: String -> Attrs
transform = makeAttr "transform"

version :: String -> Attrs
version = makeAttr "version"

viewBox :: String -> Attrs
viewBox = makeAttr "viewBox"

x1 :: String -> Attrs
x1 = makeAttr "x1"

x2 :: String -> Attrs
x2 = makeAttr "x2"

x :: String -> Attrs
x = makeAttr "x"

xlinkActuate :: String -> Attrs
xlinkActuate = makeAttr "xlinkActuate"

xlinkArcrole :: String -> Attrs
xlinkArcrole = makeAttr "xlinkArcrole"

xlinkHref :: String -> Attrs
xlinkHref = makeAttr "xlinkHref"

xlinkRole :: String -> Attrs
xlinkRole = makeAttr "xlinkRole"

xlinkShow :: String -> Attrs
xlinkShow = makeAttr "xlinkShow"

xlinkTitle :: String -> Attrs
xlinkTitle = makeAttr "xlinkTitle"

xlinkType :: String -> Attrs
xlinkType = makeAttr "xlinkType"

xmlBase :: String -> Attrs
xmlBase = makeAttr "xmlBase"

xmlLang :: String -> Attrs
xmlLang = makeAttr "xmlLang"

xmlSpace :: String -> Attrs
xmlSpace = makeAttr "xmlSpace"

y1 :: String -> Attrs
y1 = makeAttr "y1"

y2 :: String -> Attrs
y2 = makeAttr "y2"

y :: String -> Attrs
y = makeAttr "y"
