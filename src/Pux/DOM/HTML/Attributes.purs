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

import Data.List
import DOM (DOM())
import Prelude
import Pux.DOM
import Pux.React
import Pux.React.Types
import Signal.Channel (Chan())

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

onCopy :: forall action eff. Handler Event action (dom :: DOM, chan :: Chan | eff) -> Attrs
onCopy (Handler actions fx) = makeHandler "onCopy" fx $ \ev -> actions

onCut :: forall action eff. Handler Event action (dom :: DOM, chan :: Chan | eff) -> Attrs
onCut (Handler actions fx) = makeHandler "onCut" fx $ \ev -> actions

onPaste :: forall action eff. Handler Event action (dom :: DOM, chan :: Chan | eff) -> Attrs
onPaste (Handler actions fx) = makeHandler "onPaste" fx $ \ev -> actions

onKeyDown :: forall action eff. Handler KeyboardEvent (KeyboardEvent -> action) (dom :: DOM, chan :: Chan | eff) -> Attrs
onKeyDown (Handler actions fx) = makeHandler "onKeyDown" fx $ \ev -> map (\a -> a ev) actions

onKeyPress :: forall action eff. Handler KeyboardEvent (KeyboardEvent -> action) (dom :: DOM, chan :: Chan | eff) -> Attrs
onKeyPress (Handler actions fx) = makeHandler "onKeyPress" fx $ \ev -> map (\a -> a ev) actions

onKeyUp :: forall action eff. Handler KeyboardEvent (KeyboardEvent -> action) (dom :: DOM, chan :: Chan | eff) -> Attrs
onKeyUp (Handler actions fx) = makeHandler "onKeyUp" fx $ \ev -> map (\a -> a ev) actions

onFocus :: forall action eff. Handler KeyboardEvent (KeyboardEvent -> action) (dom :: DOM, chan :: Chan | eff) -> Attrs
onFocus (Handler actions fx) = makeHandler "onFocus" fx $ \ev -> actions

onBlur :: forall action eff. Handler KeyboardEvent (KeyboardEvent -> action) (dom :: DOM, chan :: Chan | eff) -> Attrs
onBlur (Handler actions fx) = makeHandler "onBlur" fx $ \ev -> actions

onChange :: forall action eff. Handler KeyboardEvent (KeyboardEvent -> action) (dom :: DOM, chan :: Chan | eff) -> Attrs
onChange (Handler actions fx) = makeHandler "onChange" fx $ \ev -> actions

onInput :: forall action eff. Handler KeyboardEvent (KeyboardEvent -> action) (dom :: DOM, chan :: Chan | eff) -> Attrs
onInput (Handler actions fx) = makeHandler "onInput" fx $ \ev -> actions

onSubmit :: forall action eff. Handler KeyboardEvent (KeyboardEvent -> action) (dom :: DOM, chan :: Chan | eff) -> Attrs
onSubmit (Handler actions fx) = makeHandler "onSubmit" fx $ \ev -> actions

onClick :: forall action eff. Handler Event action (dom :: DOM, chan :: Chan | eff) -> Attrs
onClick (Handler actions fx) = makeHandler "onClick" fx $ \ev -> actions

onDoubleClick :: forall action eff. Handler Event action (dom :: DOM, chan :: Chan | eff) -> Attrs
onDoubleClick (Handler actions fx) = makeHandler "onDoubleClick" fx $ \ev -> actions

onDrag :: forall action eff. Handler Event action (dom :: DOM, chan :: Chan | eff) -> Attrs
onDrag (Handler actions fx) = makeHandler "onDrag" fx $ \ev -> actions

onDragEnd :: forall action eff. Handler Event action (dom :: DOM, chan :: Chan | eff) -> Attrs
onDragEnd (Handler actions fx) = makeHandler "onDragEnd" fx $ \ev -> actions

onDragEnter :: forall action eff. Handler Event action (dom :: DOM, chan :: Chan | eff) -> Attrs
onDragEnter (Handler actions fx) = makeHandler "onDragEnter" fx $ \ev -> actions

onDragExit :: forall action eff. Handler Event action (dom :: DOM, chan :: Chan | eff) -> Attrs
onDragExit (Handler actions fx) = makeHandler "onDragExit" fx $ \ev -> actions

onDragLeave :: forall action eff. Handler Event action (dom :: DOM, chan :: Chan | eff) -> Attrs
onDragLeave (Handler actions fx) = makeHandler "onDragLeave" fx $ \ev -> actions

onDragOver :: forall action eff. Handler Event action (dom :: DOM, chan :: Chan | eff) -> Attrs
onDragOver (Handler actions fx) = makeHandler "onDragOver" fx $ \ev -> actions

onDragStart :: forall action eff. Handler Event action (dom :: DOM, chan :: Chan | eff) -> Attrs
onDragStart (Handler actions fx) = makeHandler "onDragStart" fx $ \ev -> actions

onDrop :: forall action eff. Handler Event action (dom :: DOM, chan :: Chan | eff) -> Attrs
onDrop (Handler actions fx) = makeHandler "onDrop" fx $ \ev -> actions

onMouseDown :: forall action eff. Handler MouseEvent (MouseEvent -> action) (dom :: DOM, chan :: Chan | eff) -> Attrs
onMouseDown (Handler actions fx) = makeHandler "onMouseDown" fx $ \ev -> map (\a -> a ev) actions

onMouseEnter :: forall action eff. Handler MouseEvent (MouseEvent -> action) (dom :: DOM, chan :: Chan | eff) -> Attrs
onMouseEnter (Handler actions fx) = makeHandler "onMouseEnter" fx $ \ev -> map (\a -> a ev) actions

onMouseLeave :: forall action eff. Handler MouseEvent (MouseEvent -> action) (dom :: DOM, chan :: Chan | eff) -> Attrs
onMouseLeave (Handler actions fx) = makeHandler "onMouseLeave" fx $ \ev -> map (\a -> a ev) actions

onMouseMove :: forall action eff. Handler MouseEvent (MouseEvent -> action) (dom :: DOM, chan :: Chan | eff) -> Attrs
onMouseMove (Handler actions fx) = makeHandler "onMouseMove" fx $ \ev -> map (\a -> a ev) actions

onMouseOut :: forall action eff. Handler MouseEvent (MouseEvent -> action) (dom :: DOM, chan :: Chan | eff) -> Attrs
onMouseOut (Handler actions fx) = makeHandler "onMouseOut" fx $ \ev -> map (\a -> a ev) actions

onMouseOver :: forall action eff. Handler MouseEvent (MouseEvent -> action) (dom :: DOM, chan :: Chan | eff) -> Attrs
onMouseOver (Handler actions fx) = makeHandler "onMouseOver" fx $ \ev -> map (\a -> a ev) actions

onMouseUp :: forall action eff. Handler MouseEvent (MouseEvent -> action) (dom :: DOM, chan :: Chan | eff) -> Attrs
onMouseUp (Handler actions fx) = makeHandler "onMouseUp" fx $ \ev -> map (\a -> a ev) actions

onTouchCancel :: forall action eff. Handler Event action (dom :: DOM, chan :: Chan | eff) -> Attrs
onTouchCancel (Handler actions fx) = makeHandler "onTouchCancel" fx $ \ev -> actions

onTouchEnd :: forall action eff. Handler Event action (dom :: DOM, chan :: Chan | eff) -> Attrs
onTouchEnd (Handler actions fx) = makeHandler "onTouchEnd" fx $ \ev -> actions

onTouchMove :: forall action eff. Handler Event action (dom :: DOM, chan :: Chan | eff) -> Attrs
onTouchMove (Handler actions fx) = makeHandler "onTouchMove" fx $ \ev -> actions

onTouchStart :: forall action eff. Handler Event action (dom :: DOM, chan :: Chan | eff) -> Attrs
onTouchStart (Handler actions fx) = makeHandler "onTouchStart" fx $ \ev -> actions

onScroll :: forall action eff. Handler Event action (dom :: DOM, chan :: Chan | eff) -> Attrs
onScroll (Handler actions fx) = makeHandler "onScroll" fx $ \ev -> actions

onWheel :: forall action eff. Handler Event action (dom :: DOM, chan :: Chan | eff) -> Attrs
onWheel (Handler actions fx) = makeHandler "onWheel" fx $ \ev -> actions

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
