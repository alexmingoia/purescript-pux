module Pux.Html.Attributes where

import Prelude ((++))
import Pux.Html (Attribute)

accept :: forall a. String -> Attribute a
accept = attr "accept"

acceptCharset :: forall a. String -> Attribute a
acceptCharset = attr "acceptCharset"

accessKey :: forall a. String -> Attribute a
accessKey = attr "accessKey"

action :: forall a. String -> Attribute a
action = attr "action"

allowFullScreen :: forall a. String -> Attribute a
allowFullScreen = attr "allowFullScreen"

allowTransparency :: forall a. String -> Attribute a
allowTransparency = attr "allowTransparency"

alt :: forall a. String -> Attribute a
alt = attr "alt"

aria :: forall a. String -> String -> Attribute a
aria attr = attr ("aria-" ++ attr)

async :: forall a. String -> Attribute a
async = attr "async"

autoComplete :: forall a. String -> Attribute a
autoComplete = attr "autoComplete"

autoFocus :: forall a. Boolean -> Attribute a
autoFocus true = attr "autoFocus" "autoFocus"
autoFocus false = attr "autoFocus" ""

autoPlay :: forall a. String -> Attribute a
autoPlay = attr "autoPlay"

cellPadding :: forall a. String -> Attribute a
cellPadding = attr "cellPadding"

cellSpacing :: forall a. String -> Attribute a
cellSpacing = attr "cellSpacing"

charSet :: forall a. String -> Attribute a
charSet = attr "charSet"

checked :: forall a. String -> Attribute a
checked = attr "checked"

classID :: forall a. String -> Attribute a
classID = attr "classID"

className :: forall a. String -> Attribute a
className = attr "className"

cols :: forall a. String -> Attribute a
cols = attr "cols"

colSpan :: forall a. String -> Attribute a
colSpan = attr "colSpan"

content :: forall a. String -> Attribute a
content = attr "content"

contentEditable :: forall a. String -> Attribute a
contentEditable = attr "contentEditable"

contextMenu :: forall a. String -> Attribute a
contextMenu = attr "contextMenu"

controls :: forall a. String -> Attribute a
controls = attr "controls"

coords :: forall a. String -> Attribute a
coords = attr "coords"

crossOrigin :: forall a. String -> Attribute a
crossOrigin = attr "crossOrigin"

dangerouslySetInnerHTML :: forall a. String -> Attribute a
dangerouslySetInnerHTML = attr "dangerouslySetInnerHTML"

data_ :: forall a. String -> String -> Attribute a
data_ attr = attr ("data-" ++ attr)

dateTime :: forall a. String -> Attribute a
dateTime = attr "dateTime"

defer :: forall a. String -> Attribute a
defer = attr "defer"

dir :: forall a. String -> Attribute a
dir = attr "dir"

disabled :: forall a. Boolean -> Attribute a
disabled true = attr "disabled" "disabled"
disabled false = attr "disabled" ""

download :: forall a. String -> Attribute a
download = attr "download"

draggable :: forall a. String -> Attribute a
draggable = attr "draggable"

encType :: forall a. String -> Attribute a
encType = attr "encType"

form :: forall a. String -> Attribute a
form = attr "form"

formAction :: forall a. String -> Attribute a
formAction = attr "formAction"

formEncType :: forall a. String -> Attribute a
formEncType = attr "formEncType"

formMethod :: forall a. String -> Attribute a
formMethod = attr "formMethod"

formNoValidate :: forall a. String -> Attribute a
formNoValidate = attr "formNoValidate"

formTarget :: forall a. String -> Attribute a
formTarget = attr "formTarget"

frameBorder :: forall a. String -> Attribute a
frameBorder = attr "frameBorder"

height :: forall a. String -> Attribute a
height = attr "height"

hidden :: forall a. String -> Attribute a
hidden = attr "hidden"

href :: forall a. String -> Attribute a
href = attr "href"

hrefLang :: forall a. String -> Attribute a
hrefLang = attr "hrefLang"

htmlFor :: forall a. String -> Attribute a
htmlFor = attr "htmlFor"

httpEquiv :: forall a. String -> Attribute a
httpEquiv = attr "httpEquiv"

icon :: forall a. String -> Attribute a
icon = attr "icon"

id_ :: forall a. String -> Attribute a
id_ = attr "id"

key :: forall a. String -> Attribute a
key = attr "key"

label :: forall a. String -> Attribute a
label = attr "label"

lang :: forall a. String -> Attribute a
lang = attr "lang"

list :: forall a. String -> Attribute a
list = attr "list"

loop :: forall a. String -> Attribute a
loop = attr "loop"

manifest :: forall a. String -> Attribute a
manifest = attr "manifest"

marginHeight :: forall a. String -> Attribute a
marginHeight = attr "marginHeight"

marginWidth :: forall a. String -> Attribute a
marginWidth = attr "marginWidth"

max :: forall a. String -> Attribute a
max = attr "max"

maxLength :: forall a. String -> Attribute a
maxLength = attr "maxLength"

media :: forall a. String -> Attribute a
media = attr "media"

mediaGroup :: forall a. String -> Attribute a
mediaGroup = attr "mediaGroup"

method :: forall a. String -> Attribute a
method = attr "method"

min :: forall a. String -> Attribute a
min = attr "min"

multiple :: forall a. String -> Attribute a
multiple = attr "multiple"

muted :: forall a. String -> Attribute a
muted = attr "muted"

name :: forall a. String -> Attribute a
name = attr "name"

noValidate :: forall a. String -> Attribute a
noValidate = attr "noValidate"

open :: forall a. String -> Attribute a
open = attr "open"

pattern :: forall a. String -> Attribute a
pattern = attr "pattern"

placeholder :: forall a. String -> Attribute a
placeholder = attr "placeholder"

poster :: forall a. String -> Attribute a
poster = attr "poster"

preload :: forall a. String -> Attribute a
preload = attr "preload"

radioGroup :: forall a. String -> Attribute a
radioGroup = attr "radioGroup"

readOnly :: forall a. String -> Attribute a
readOnly = attr "readOnly"

rel :: forall a. String -> Attribute a
rel = attr "rel"

required :: forall a. String -> Attribute a
required = attr "required"

role :: forall a. String -> Attribute a
role = attr "role"

rows :: forall a. String -> Attribute a
rows = attr "rows"

rowSpan :: forall a. String -> Attribute a
rowSpan = attr "rowSpan"

sandbox :: forall a. String -> Attribute a
sandbox = attr "sandbox"

scope :: forall a. String -> Attribute a
scope = attr "scope"

scrolling :: forall a. String -> Attribute a
scrolling = attr "scrolling"

seamless :: forall a. String -> Attribute a
seamless = attr "seamless"

selected :: forall a. String -> Attribute a
selected = attr "selected"

shape :: forall a. String -> Attribute a
shape = attr "shape"

size :: forall a. String -> Attribute a
size = attr "size"

sizes :: forall a. String -> Attribute a
sizes = attr "sizes"

span_ :: forall a. String -> Attribute a
span_ = attr "span"

spellCheck :: forall a. String -> Attribute a
spellCheck = attr "spellCheck"

src :: forall a. String -> Attribute a
src = attr "src"

srcDoc :: forall a. String -> Attribute a
srcDoc = attr "srcDoc"

srcSet :: forall a. String -> Attribute a
srcSet = attr "srcSet"

start :: forall a. String -> Attribute a
start = attr "start"

step :: forall a. String -> Attribute a
step = attr "step"

tabIndex :: forall a. String -> Attribute a
tabIndex = attr "tabIndex"

target :: forall a. String -> Attribute a
target = attr "target"

title :: forall a. String -> Attribute a
title = attr "title"

type_ :: forall a. String -> Attribute a
type_ = attr "type"

useMap :: forall a. String -> Attribute a
useMap = attr "useMap"

value :: forall a. String -> Attribute a
value = attr "value"

width :: forall a. String -> Attribute a
width = attr "width"

wmode :: forall a. String -> Attribute a
wmode = attr "wmode"

clipPath :: forall a. String -> Attribute a
clipPath = attr "clipPath"

cx :: forall a. String -> Attribute a
cx = attr "cx"

cy :: forall a. String -> Attribute a
cy = attr "cy"

d :: forall a. String -> Attribute a
d = attr "d"

dx :: forall a. String -> Attribute a
dx = attr "dx"

dy :: forall a. String -> Attribute a
dy = attr "dy"

fill :: forall a. String -> Attribute a
fill = attr "fill"

fillOpacity :: forall a. String -> Attribute a
fillOpacity = attr "fillOpacity"

fontFamily :: forall a. String -> Attribute a
fontFamily = attr "fontFamily"

fontSize :: forall a. String -> Attribute a
fontSize = attr "fontSize"

fx :: forall a. String -> Attribute a
fx = attr "effects"

fy :: forall a. String -> Attribute a
fy = attr "fy"

gradientTransform :: forall a. String -> Attribute a
gradientTransform = attr "gradientTransform"

gradientUnits :: forall a. String -> Attribute a
gradientUnits = attr "gradientUnits"

markerEnd :: forall a. String -> Attribute a
markerEnd = attr "markerEnd"

markerMid :: forall a. String -> Attribute a
markerMid = attr "markerMid"

markerStart :: forall a. String -> Attribute a
markerStart = attr "markerStart"

offset :: forall a. String -> Attribute a
offset = attr "offset"

opacity :: forall a. String -> Attribute a
opacity = attr "opacity"

patternContentUnits :: forall a. String -> Attribute a
patternContentUnits = attr "patternContentUnits"

patternUnits :: forall a. String -> Attribute a
patternUnits = attr "patternUnits"

points :: forall a. String -> Attribute a
points = attr "points"

preserveAspectRatio :: forall a. String -> Attribute a
preserveAspectRatio = attr "preserveAspectRatio"

r :: forall a. String -> Attribute a
r = attr "r"

rx :: forall a. String -> Attribute a
rx = attr "rx"

ry :: forall a. String -> Attribute a
ry = attr "ry"

spreadMethod :: forall a. String -> Attribute a
spreadMethod = attr "spreadMethod"

stopColor :: forall a. String -> Attribute a
stopColor = attr "stopColor"

stopOpacity :: forall a. String -> Attribute a
stopOpacity = attr "stopOpacity"

stroke :: forall a. String -> Attribute a
stroke = attr "stroke"

strokeDasharray :: forall a. String -> Attribute a
strokeDasharray = attr "strokeDasharray"

strokeLinecap :: forall a. String -> Attribute a
strokeLinecap = attr "strokeLinecap"

strokeOpacity :: forall a. String -> Attribute a
strokeOpacity = attr "strokeOpacity"

strokeWidth :: forall a. String -> Attribute a
strokeWidth = attr "strokeWidth"

style :: forall a v. { | v } -> Attribute a
style = attr "style"

textAnchor :: forall a. String -> Attribute a
textAnchor = attr "textAnchor"

transform :: forall a. String -> Attribute a
transform = attr "transform"

version :: forall a. String -> Attribute a
version = attr "version"

viewBox :: forall a. String -> Attribute a
viewBox = attr "viewBox"

x1 :: forall a. String -> Attribute a
x1 = attr "x1"

x2 :: forall a. String -> Attribute a
x2 = attr "x2"

x :: forall a. String -> Attribute a
x = attr "x"

xlinkActuate :: forall a. String -> Attribute a
xlinkActuate = attr "xlinkActuate"

xlinkArcrole :: forall a. String -> Attribute a
xlinkArcrole = attr "xlinkArcrole"

xlinkHref :: forall a. String -> Attribute a
xlinkHref = attr "xlinkHref"

xlinkRole :: forall a. String -> Attribute a
xlinkRole = attr "xlinkRole"

xlinkShow :: forall a. String -> Attribute a
xlinkShow = attr "xlinkShow"

xlinkTitle :: forall a. String -> Attribute a
xlinkTitle = attr "xlinkTitle"

xlinkType :: forall a. String -> Attribute a
xlinkType = attr "xlinkType"

xmlBase :: forall a. String -> Attribute a
xmlBase = attr "xmlBase"

xmlLang :: forall a. String -> Attribute a
xmlLang = attr "xmlLang"

xmlSpace :: forall a. String -> Attribute a
xmlSpace = attr "xmlSpace"

y1 :: forall a. String -> Attribute a
y1 = attr "y1"

y2 :: forall a. String -> Attribute a
y2 = attr "y2"

y :: forall a. String -> Attribute a
y = attr "y"

foreign import attr :: forall a v. String -> v -> Attribute a
