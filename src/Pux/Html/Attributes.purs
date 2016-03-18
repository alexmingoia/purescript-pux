module Pux.Html.Attributes where

import Data.Function (Fn2, runFn2)
import Prelude ((++))
import Pux.Html (Attribute)

accept :: forall a. String -> Attribute a
accept = runFn2 attribute "accept"

acceptCharset :: forall a. String -> Attribute a
acceptCharset = runFn2 attribute "acceptCharset"

accessKey :: forall a. String -> Attribute a
accessKey = runFn2 attribute "accessKey"

action :: forall a. String -> Attribute a
action = runFn2 attribute "action"

allowFullScreen :: forall a. String -> Attribute a
allowFullScreen = runFn2 attribute "allowFullScreen"

allowTransparency :: forall a. String -> Attribute a
allowTransparency = runFn2 attribute "allowTransparency"

alt :: forall a. String -> Attribute a
alt = runFn2 attribute "alt"

aria :: forall a. String -> String -> Attribute a
aria attr = runFn2 attribute ("aria-" ++ attr)

async :: forall a. String -> Attribute a
async = runFn2 attribute "async"

autoComplete :: forall a. String -> Attribute a
autoComplete = runFn2 attribute "autoComplete"

autoFocus :: forall a. Boolean -> Attribute a
autoFocus true = runFn2 attribute "autoFocus" "autoFocus"
autoFocus false = runFn2 attribute "autoFocus" ""

autoPlay :: forall a. String -> Attribute a
autoPlay = runFn2 attribute "autoPlay"

cellPadding :: forall a. String -> Attribute a
cellPadding = runFn2 attribute "cellPadding"

cellSpacing :: forall a. String -> Attribute a
cellSpacing = runFn2 attribute "cellSpacing"

charSet :: forall a. String -> Attribute a
charSet = runFn2 attribute "charSet"

checked :: forall a. String -> Attribute a
checked = runFn2 attribute "checked"

classID :: forall a. String -> Attribute a
classID = runFn2 attribute "classID"

className :: forall a. String -> Attribute a
className = runFn2 attribute "className"

cols :: forall a. String -> Attribute a
cols = runFn2 attribute "cols"

colSpan :: forall a. String -> Attribute a
colSpan = runFn2 attribute "colSpan"

content :: forall a. String -> Attribute a
content = runFn2 attribute "content"

contentEditable :: forall a. String -> Attribute a
contentEditable = runFn2 attribute "contentEditable"

contextMenu :: forall a. String -> Attribute a
contextMenu = runFn2 attribute "contextMenu"

controls :: forall a. String -> Attribute a
controls = runFn2 attribute "controls"

coords :: forall a. String -> Attribute a
coords = runFn2 attribute "coords"

crossOrigin :: forall a. String -> Attribute a
crossOrigin = runFn2 attribute "crossOrigin"

dangerouslySetInnerHTML :: forall a. String -> Attribute a
dangerouslySetInnerHTML = runFn2 attribute "dangerouslySetInnerHTML"

data_ :: forall a. String -> String -> Attribute a
data_ attr = runFn2 attribute ("data-" ++ attr)

dateTime :: forall a. String -> Attribute a
dateTime = runFn2 attribute "dateTime"

defer :: forall a. String -> Attribute a
defer = runFn2 attribute "defer"

dir :: forall a. String -> Attribute a
dir = runFn2 attribute "dir"

disabled :: forall a. Boolean -> Attribute a
disabled true = runFn2 attribute "disabled" "disabled"
disabled false = runFn2 attribute "disabled" ""

download :: forall a. String -> Attribute a
download = runFn2 attribute "download"

draggable :: forall a. String -> Attribute a
draggable = runFn2 attribute "draggable"

encType :: forall a. String -> Attribute a
encType = runFn2 attribute "encType"

form :: forall a. String -> Attribute a
form = runFn2 attribute "form"

formAction :: forall a. String -> Attribute a
formAction = runFn2 attribute "formAction"

formEncType :: forall a. String -> Attribute a
formEncType = runFn2 attribute "formEncType"

formMethod :: forall a. String -> Attribute a
formMethod = runFn2 attribute "formMethod"

formNoValidate :: forall a. String -> Attribute a
formNoValidate = runFn2 attribute "formNoValidate"

formTarget :: forall a. String -> Attribute a
formTarget = runFn2 attribute "formTarget"

frameBorder :: forall a. String -> Attribute a
frameBorder = runFn2 attribute "frameBorder"

height :: forall a. String -> Attribute a
height = runFn2 attribute "height"

hidden :: forall a. String -> Attribute a
hidden = runFn2 attribute "hidden"

href :: forall a. String -> Attribute a
href = runFn2 attribute "href"

hrefLang :: forall a. String -> Attribute a
hrefLang = runFn2 attribute "hrefLang"

htmlFor :: forall a. String -> Attribute a
htmlFor = runFn2 attribute "htmlFor"

httpEquiv :: forall a. String -> Attribute a
httpEquiv = runFn2 attribute "httpEquiv"

icon :: forall a. String -> Attribute a
icon = runFn2 attribute "icon"

id_ :: forall a. String -> Attribute a
id_ = runFn2 attribute "id"

key :: forall a. String -> Attribute a
key = runFn2 attribute "key"

label :: forall a. String -> Attribute a
label = runFn2 attribute "label"

lang :: forall a. String -> Attribute a
lang = runFn2 attribute "lang"

list :: forall a. String -> Attribute a
list = runFn2 attribute "list"

loop :: forall a. String -> Attribute a
loop = runFn2 attribute "loop"

manifest :: forall a. String -> Attribute a
manifest = runFn2 attribute "manifest"

marginHeight :: forall a. String -> Attribute a
marginHeight = runFn2 attribute "marginHeight"

marginWidth :: forall a. String -> Attribute a
marginWidth = runFn2 attribute "marginWidth"

max :: forall a. String -> Attribute a
max = runFn2 attribute "max"

maxLength :: forall a. String -> Attribute a
maxLength = runFn2 attribute "maxLength"

media :: forall a. String -> Attribute a
media = runFn2 attribute "media"

mediaGroup :: forall a. String -> Attribute a
mediaGroup = runFn2 attribute "mediaGroup"

method :: forall a. String -> Attribute a
method = runFn2 attribute "method"

min :: forall a. String -> Attribute a
min = runFn2 attribute "min"

multiple :: forall a. String -> Attribute a
multiple = runFn2 attribute "multiple"

muted :: forall a. String -> Attribute a
muted = runFn2 attribute "muted"

name :: forall a. String -> Attribute a
name = runFn2 attribute "name"

noValidate :: forall a. String -> Attribute a
noValidate = runFn2 attribute "noValidate"

open :: forall a. String -> Attribute a
open = runFn2 attribute "open"

pattern :: forall a. String -> Attribute a
pattern = runFn2 attribute "pattern"

placeholder :: forall a. String -> Attribute a
placeholder = runFn2 attribute "placeholder"

poster :: forall a. String -> Attribute a
poster = runFn2 attribute "poster"

preload :: forall a. String -> Attribute a
preload = runFn2 attribute "preload"

radioGroup :: forall a. String -> Attribute a
radioGroup = runFn2 attribute "radioGroup"

readOnly :: forall a. String -> Attribute a
readOnly = runFn2 attribute "readOnly"

rel :: forall a. String -> Attribute a
rel = runFn2 attribute "rel"

required :: forall a. String -> Attribute a
required = runFn2 attribute "required"

role :: forall a. String -> Attribute a
role = runFn2 attribute "role"

rows :: forall a. String -> Attribute a
rows = runFn2 attribute "rows"

rowSpan :: forall a. String -> Attribute a
rowSpan = runFn2 attribute "rowSpan"

sandbox :: forall a. String -> Attribute a
sandbox = runFn2 attribute "sandbox"

scope :: forall a. String -> Attribute a
scope = runFn2 attribute "scope"

scrolling :: forall a. String -> Attribute a
scrolling = runFn2 attribute "scrolling"

seamless :: forall a. String -> Attribute a
seamless = runFn2 attribute "seamless"

selected :: forall a. String -> Attribute a
selected = runFn2 attribute "selected"

shape :: forall a. String -> Attribute a
shape = runFn2 attribute "shape"

size :: forall a. String -> Attribute a
size = runFn2 attribute "size"

sizes :: forall a. String -> Attribute a
sizes = runFn2 attribute "sizes"

span_ :: forall a. String -> Attribute a
span_ = runFn2 attribute "span"

spellCheck :: forall a. String -> Attribute a
spellCheck = runFn2 attribute "spellCheck"

src :: forall a. String -> Attribute a
src = runFn2 attribute "src"

srcDoc :: forall a. String -> Attribute a
srcDoc = runFn2 attribute "srcDoc"

srcSet :: forall a. String -> Attribute a
srcSet = runFn2 attribute "srcSet"

start :: forall a. String -> Attribute a
start = runFn2 attribute "start"

step :: forall a. String -> Attribute a
step = runFn2 attribute "step"

tabIndex :: forall a. String -> Attribute a
tabIndex = runFn2 attribute "tabIndex"

target :: forall a. String -> Attribute a
target = runFn2 attribute "target"

title :: forall a. String -> Attribute a
title = runFn2 attribute "title"

type_ :: forall a. String -> Attribute a
type_ = runFn2 attribute "type"

useMap :: forall a. String -> Attribute a
useMap = runFn2 attribute "useMap"

value :: forall a. String -> Attribute a
value = runFn2 attribute "value"

width :: forall a. String -> Attribute a
width = runFn2 attribute "width"

wmode :: forall a. String -> Attribute a
wmode = runFn2 attribute "wmode"

clipPath :: forall a. String -> Attribute a
clipPath = runFn2 attribute "clipPath"

cx :: forall a. String -> Attribute a
cx = runFn2 attribute "cx"

cy :: forall a. String -> Attribute a
cy = runFn2 attribute "cy"

d :: forall a. String -> Attribute a
d = runFn2 attribute "d"

dx :: forall a. String -> Attribute a
dx = runFn2 attribute "dx"

dy :: forall a. String -> Attribute a
dy = runFn2 attribute "dy"

fill :: forall a. String -> Attribute a
fill = runFn2 attribute "fill"

fillOpacity :: forall a. String -> Attribute a
fillOpacity = runFn2 attribute "fillOpacity"

fontFamily :: forall a. String -> Attribute a
fontFamily = runFn2 attribute "fontFamily"

fontSize :: forall a. String -> Attribute a
fontSize = runFn2 attribute "fontSize"

fx :: forall a. String -> Attribute a
fx = runFn2 attribute "effects"

fy :: forall a. String -> Attribute a
fy = runFn2 attribute "fy"

gradientTransform :: forall a. String -> Attribute a
gradientTransform = runFn2 attribute "gradientTransform"

gradientUnits :: forall a. String -> Attribute a
gradientUnits = runFn2 attribute "gradientUnits"

markerEnd :: forall a. String -> Attribute a
markerEnd = runFn2 attribute "markerEnd"

markerMid :: forall a. String -> Attribute a
markerMid = runFn2 attribute "markerMid"

markerStart :: forall a. String -> Attribute a
markerStart = runFn2 attribute "markerStart"

offset :: forall a. String -> Attribute a
offset = runFn2 attribute "offset"

opacity :: forall a. String -> Attribute a
opacity = runFn2 attribute "opacity"

patternContentUnits :: forall a. String -> Attribute a
patternContentUnits = runFn2 attribute "patternContentUnits"

patternUnits :: forall a. String -> Attribute a
patternUnits = runFn2 attribute "patternUnits"

points :: forall a. String -> Attribute a
points = runFn2 attribute "points"

preserveAspectRatio :: forall a. String -> Attribute a
preserveAspectRatio = runFn2 attribute "preserveAspectRatio"

r :: forall a. String -> Attribute a
r = runFn2 attribute "r"

rx :: forall a. String -> Attribute a
rx = runFn2 attribute "rx"

ry :: forall a. String -> Attribute a
ry = runFn2 attribute "ry"

spreadMethod :: forall a. String -> Attribute a
spreadMethod = runFn2 attribute "spreadMethod"

stopColor :: forall a. String -> Attribute a
stopColor = runFn2 attribute "stopColor"

stopOpacity :: forall a. String -> Attribute a
stopOpacity = runFn2 attribute "stopOpacity"

stroke :: forall a. String -> Attribute a
stroke = runFn2 attribute "stroke"

strokeDasharray :: forall a. String -> Attribute a
strokeDasharray = runFn2 attribute "strokeDasharray"

strokeLinecap :: forall a. String -> Attribute a
strokeLinecap = runFn2 attribute "strokeLinecap"

strokeOpacity :: forall a. String -> Attribute a
strokeOpacity = runFn2 attribute "strokeOpacity"

strokeWidth :: forall a. String -> Attribute a
strokeWidth = runFn2 attribute "strokeWidth"

style :: forall a. String -> Attribute a
style = runFn2 attribute "style"

textAnchor :: forall a. String -> Attribute a
textAnchor = runFn2 attribute "textAnchor"

transform :: forall a. String -> Attribute a
transform = runFn2 attribute "transform"

version :: forall a. String -> Attribute a
version = runFn2 attribute "version"

viewBox :: forall a. String -> Attribute a
viewBox = runFn2 attribute "viewBox"

x1 :: forall a. String -> Attribute a
x1 = runFn2 attribute "x1"

x2 :: forall a. String -> Attribute a
x2 = runFn2 attribute "x2"

x :: forall a. String -> Attribute a
x = runFn2 attribute "x"

xlinkActuate :: forall a. String -> Attribute a
xlinkActuate = runFn2 attribute "xlinkActuate"

xlinkArcrole :: forall a. String -> Attribute a
xlinkArcrole = runFn2 attribute "xlinkArcrole"

xlinkHref :: forall a. String -> Attribute a
xlinkHref = runFn2 attribute "xlinkHref"

xlinkRole :: forall a. String -> Attribute a
xlinkRole = runFn2 attribute "xlinkRole"

xlinkShow :: forall a. String -> Attribute a
xlinkShow = runFn2 attribute "xlinkShow"

xlinkTitle :: forall a. String -> Attribute a
xlinkTitle = runFn2 attribute "xlinkTitle"

xlinkType :: forall a. String -> Attribute a
xlinkType = runFn2 attribute "xlinkType"

xmlBase :: forall a. String -> Attribute a
xmlBase = runFn2 attribute "xmlBase"

xmlLang :: forall a. String -> Attribute a
xmlLang = runFn2 attribute "xmlLang"

xmlSpace :: forall a. String -> Attribute a
xmlSpace = runFn2 attribute "xmlSpace"

y1 :: forall a. String -> Attribute a
y1 = runFn2 attribute "y1"

y2 :: forall a. String -> Attribute a
y2 = runFn2 attribute "y2"

y :: forall a. String -> Attribute a
y = runFn2 attribute "y"

foreign import attribute :: forall a. Fn2 String String (Attribute a)
