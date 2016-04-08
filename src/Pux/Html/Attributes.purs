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

allowFullScreen :: forall a. Boolean -> Attribute a
allowFullScreen = attr "allowFullScreen"

allowTransparency :: forall a. String -> Attribute a
allowTransparency = attr "allowTransparency"

alt :: forall a. String -> Attribute a
alt = attr "alt"

aria :: forall a. String -> String -> Attribute a
aria ariaAttr = attr ("aria-" ++ ariaAttr)

async :: forall a. Boolean -> Attribute a
async = attr "async"

autoCapitalize :: forall a. String -> Attribute a
autoCapitalize = attr "autoCapitalize"

autoCorrect :: forall a. String -> Attribute a
autoCorrect = attr "autoCorrect"

autoSave :: forall a. String -> Attribute a
autoSave = attr "autoSave"

autoComplete :: forall a. String -> Attribute a
autoComplete = attr "autoComplete"

autoFocus :: forall a. Boolean -> Attribute a
autoFocus = attr "autoFocus"

autoPlay :: forall a. String -> Attribute a
autoPlay = attr "autoPlay"

capture :: forall a. Boolean -> Attribute a
capture = attr "capture"

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

color :: forall a. String -> Attribute a
color = attr "color"

cols :: forall a. Int -> Attribute a
cols = attr "cols"

colSpan :: forall a. String -> Attribute a
colSpan = attr "colSpan"

content :: forall a. String -> Attribute a
content = attr "content"

contentEditable :: forall a. String -> Attribute a
contentEditable = attr "contentEditable"

contextMenu :: forall a. String -> Attribute a
contextMenu = attr "contextMenu"

controls :: forall a. Boolean -> Attribute a
controls = attr "controls"

coords :: forall a. String -> Attribute a
coords = attr "coords"

crossOrigin :: forall a. String -> Attribute a
crossOrigin = attr "crossOrigin"

dangerouslySetInnerHTML :: forall a. String -> Attribute a
dangerouslySetInnerHTML = attr "dangerouslySetInnerHTML"

data_ :: forall a. String -> String -> Attribute a
data_ dataAttr = attr ("data-" ++ dataAttr)

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

formNoValidate :: forall a. Boolean -> Attribute a
formNoValidate = attr "formNoValidate"

formTarget :: forall a. String -> Attribute a
formTarget = attr "formTarget"

frameBorder :: forall a. String -> Attribute a
frameBorder = attr "frameBorder"

height :: forall a. String -> Attribute a
height = attr "height"

hidden :: forall a. Boolean -> Attribute a
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

itemProp :: forall a. String -> Attribute a
itemProp = attr "itemProp"

itemScope :: forall a. Boolean -> Attribute a
itemScope = attr "itemScope"

itemType :: forall a. String -> Attribute a
itemType = attr "itemType"

itemID :: forall a. String -> Attribute a
itemID = attr "itemID"

itemRef :: forall a. String -> Attribute a
itemRef = attr "itemRef"

key :: forall a. String -> Attribute a
key = attr "key"

label :: forall a. String -> Attribute a
label = attr "label"

lang :: forall a. String -> Attribute a
lang = attr "lang"

list :: forall a. String -> Attribute a
list = attr "list"

loop :: forall a. Boolean -> Attribute a
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

multiple :: forall a. Boolean -> Attribute a
multiple = attr "multiple"

muted :: forall a. Boolean -> Attribute a
muted = attr "muted"

name :: forall a. String -> Attribute a
name = attr "name"

noValidate :: forall a. Boolean -> Attribute a
noValidate = attr "noValidate"

open :: forall a. Boolean -> Attribute a
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

readOnly :: forall a. Boolean -> Attribute a
readOnly = attr "readOnly"

rel :: forall a. String -> Attribute a
rel = attr "rel"

results :: forall a. String -> Attribute a
results = attr "results"

required :: forall a. Boolean -> Attribute a
required = attr "required"

role :: forall a. String -> Attribute a
role = attr "role"

rows :: forall a. Int -> Attribute a
rows = attr "rows"

rowSpan :: forall a. Int -> Attribute a
rowSpan = attr "rowSpan"

sandbox :: forall a. String -> Attribute a
sandbox = attr "sandbox"

scope :: forall a. String -> Attribute a
scope = attr "scope"

scoped :: forall a. Boolean -> Attribute a
scoped = attr "scope"

scrolling :: forall a. String -> Attribute a
scrolling = attr "scrolling"

seamless :: forall a. Boolean -> Attribute a
seamless = attr "seamless"

selected :: forall a. Boolean -> Attribute a
selected = attr "selected"

security :: forall a. String -> Attribute a
security = attr "security"

shape :: forall a. String -> Attribute a
shape = attr "shape"

size :: forall a. Int -> Attribute a
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

start :: forall a. Boolean -> Attribute a
start = attr "start"

step :: forall a. String -> Attribute a
step = attr "step"

style :: forall a v. { | v } -> Attribute a
style = attr "style"

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

unselectable :: forall a. String -> Attribute a
unselectable = attr "unselectable"

value :: forall a. String -> Attribute a
value = attr "value"

width :: forall a. String -> Attribute a
width = attr "width"

wmode :: forall a. String -> Attribute a
wmode = attr "wmode"

-- | ## SVG Attributes

accentHeight :: forall a. String -> Attribute a
accentHeight = attr "accentHeight"

accumulate :: forall a. String -> Attribute a
accumulate = attr "accumulate"

additive :: forall a. String -> Attribute a
additive = attr "additive"

alignmentBaseline :: forall a. String -> Attribute a
alignmentBaseline = attr "alignmentBaseline"

allowReorder :: forall a. String -> Attribute a
allowReorder = attr "allowReorder"

alphabetic :: forall a. String -> Attribute a
alphabetic = attr "alphabetic"

amplitude :: forall a. String -> Attribute a
amplitude = attr "amplitude"

arabicForm :: forall a. String -> Attribute a
arabicForm = attr "arabicForm"

ascent :: forall a. String -> Attribute a
ascent = attr "ascent"

attributeName :: forall a. String -> Attribute a
attributeName = attr "attributeName"

attributeType :: forall a. String -> Attribute a
attributeType = attr "attributeType"

autoReverse :: forall a. String -> Attribute a
autoReverse = attr "autoReverse"

azimuth :: forall a. String -> Attribute a
azimuth = attr "azimuth"

baseFrequency :: forall a. String -> Attribute a
baseFrequency = attr "baseFrequency"

baseProfile :: forall a. String -> Attribute a
baseProfile = attr "baseProfile"

baselineShift :: forall a. String -> Attribute a
baselineShift = attr "baselineShift"

bbox :: forall a. String -> Attribute a
bbox = attr "bbox"

begin :: forall a. String -> Attribute a
begin = attr "begin"

bias :: forall a. String -> Attribute a
bias = attr "bias"

by :: forall a. String -> Attribute a
by = attr "by"

calcMode :: forall a. String -> Attribute a
calcMode = attr "calcMode"

capHeight :: forall a. String -> Attribute a
capHeight = attr "capHeight"

clip :: forall a. String -> Attribute a
clip = attr "clip"

clipPath :: forall a. String -> Attribute a
clipPath = attr "clipPath"

clipPathUnits :: forall a. String -> Attribute a
clipPathUnits = attr "clipPathUnits"

clipRule :: forall a. String -> Attribute a
clipRule = attr "clipRule"

colorInterpolation :: forall a. String -> Attribute a
colorInterpolation = attr "colorInterpolation"

colorInterpolationFilters :: forall a. String -> Attribute a
colorInterpolationFilters = attr "colorInterpolationFilters"

colorProfile :: forall a. String -> Attribute a
colorProfile = attr "colorProfile"

colorRendering :: forall a. String -> Attribute a
colorRendering = attr "colorRendering"

contentScriptType :: forall a. String -> Attribute a
contentScriptType = attr "contentScriptType"

contentStyleType :: forall a. String -> Attribute a
contentStyleType = attr "contentStyleType"

cursor :: forall a. String -> Attribute a
cursor = attr "cursor"

cx :: forall a. String -> Attribute a
cx = attr "cx"

cy :: forall a. String -> Attribute a
cy = attr "cy"

d :: forall a. String -> Attribute a
d = attr "d"

decelerate :: forall a. String -> Attribute a
decelerate = attr "decelerate"

descent :: forall a. String -> Attribute a
descent = attr "descent"

diffuseConstant :: forall a. String -> Attribute a
diffuseConstant = attr "diffuseConstant"

direction :: forall a. String -> Attribute a
direction = attr "direction"

display :: forall a. String -> Attribute a
display = attr "display"

divisor :: forall a. String -> Attribute a
divisor = attr "divisor"

dominantBaseline :: forall a. String -> Attribute a
dominantBaseline = attr "dominantBaseline"

dur :: forall a. String -> Attribute a
dur = attr "dur"

dx :: forall a. String -> Attribute a
dx = attr "dx"

dy :: forall a. String -> Attribute a
dy = attr "dy"

edgeMode :: forall a. String -> Attribute a
edgeMode = attr "edgeMode"

elevation :: forall a. String -> Attribute a
elevation = attr "elevation"

enableBackground :: forall a. String -> Attribute a
enableBackground = attr "enableBackground"

end :: forall a. String -> Attribute a
end = attr "end"

exponent :: forall a. String -> Attribute a
exponent = attr "exponent"

externalResourcesRequired :: forall a. String -> Attribute a
externalResourcesRequired = attr "externalResourcesRequired"

fill :: forall a. String -> Attribute a
fill = attr "fill"

fillOpacity :: forall a. String -> Attribute a
fillOpacity = attr "fillOpacity"

fillRule :: forall a. String -> Attribute a
fillRule = attr "fillRule"

filter :: forall a. String -> Attribute a
filter = attr "filter"

filterRes :: forall a. String -> Attribute a
filterRes = attr "filterRes"

filterUnits :: forall a. String -> Attribute a
filterUnits = attr "filterUnits"

floodColor :: forall a. String -> Attribute a
floodColor = attr "floodColor"

floodOpacity :: forall a. String -> Attribute a
floodOpacity = attr "floodOpacity"

focusable :: forall a. String -> Attribute a
focusable = attr "focusable"

fontFamily :: forall a. String -> Attribute a
fontFamily = attr "fontFamily"

fontSize :: forall a. String -> Attribute a
fontSize = attr "fontSize"

fontSizeAdjust :: forall a. String -> Attribute a
fontSizeAdjust = attr "fontSizeAdjust"

fontStretch :: forall a. String -> Attribute a
fontStretch = attr "fontStretch"

fontStyle :: forall a. String -> Attribute a
fontStyle = attr "fontStyle"

fontVariant :: forall a. String -> Attribute a
fontVariant = attr "fontVariant"

fontWeight :: forall a. String -> Attribute a
fontWeight = attr "fontWeight"

format :: forall a. String -> Attribute a
format = attr "format"

from :: forall a. String -> Attribute a
from = attr "from"

fx :: forall a. String -> Attribute a
fx = attr "effects"

fy :: forall a. String -> Attribute a
fy = attr "fy"

g1 :: forall a. String -> Attribute a
g1 = attr "g1"

g2 :: forall a. String -> Attribute a
g2 = attr "g2"

glyphName :: forall a. String -> Attribute a
glyphName = attr "glyphName"

glyphOrientationHorizontal :: forall a. String -> Attribute a
glyphOrientationHorizontal = attr "glyphOrientationHorizontal"

glyphOrientationVertical :: forall a. String -> Attribute a
glyphOrientationVertical = attr "glyphOrientationVertical"

glyphRef :: forall a. String -> Attribute a
glyphRef = attr "glyphRef"

gradientTransform :: forall a. String -> Attribute a
gradientTransform = attr "gradientTransform"

gradientUnits :: forall a. String -> Attribute a
gradientUnits = attr "gradientUnits"

hanging :: forall a. String -> Attribute a
hanging = attr "hanging"

horizAdvX :: forall a. String -> Attribute a
horizAdvX = attr "horizAdvX"

horizOriginX :: forall a. String -> Attribute a
horizOriginX = attr "horizOriginX"

ideographic :: forall a. String -> Attribute a
ideographic = attr "ideographic"

imageRendering :: forall a. String -> Attribute a
imageRendering = attr "imageRendering"

in2 :: forall a. String -> Attribute a
in2 = attr "in2"

in_ :: forall a. String -> Attribute a
in_ = attr "in"

intercept :: forall a. String -> Attribute a
intercept = attr "intercept"

k :: forall a. String -> Attribute a
k = attr "k"

k1 :: forall a. String -> Attribute a
k1 = attr "k1"

k2 :: forall a. String -> Attribute a
k2 = attr "k2"

k3 :: forall a. String -> Attribute a
k3 = attr "k3"

k4 :: forall a. String -> Attribute a
k4 = attr "k4"

kernelMatrix :: forall a. String -> Attribute a
kernelMatrix = attr "kernelMatrix"

kernelUnitLength :: forall a. String -> Attribute a
kernelUnitLength = attr "kernelUnitLength"

kerning :: forall a. String -> Attribute a
kerning = attr "kerning"

keyPoints :: forall a. String -> Attribute a
keyPoints = attr "keyPoints"

keySplines :: forall a. String -> Attribute a
keySplines = attr "keySplines"

keyTimes :: forall a. String -> Attribute a
keyTimes = attr "keyTimes"

lengthAdjust :: forall a. String -> Attribute a
lengthAdjust = attr "lengthAdjust"

letterSpacing :: forall a. String -> Attribute a
letterSpacing = attr "letterSpacing"

lightingColor :: forall a. String -> Attribute a
lightingColor = attr "lightingColor"

limitingConeAngle :: forall a. String -> Attribute a
limitingConeAngle = attr "limitingConeAngle"

local :: forall a. String -> Attribute a
local = attr "local"

markerEnd :: forall a. String -> Attribute a
markerEnd = attr "markerEnd"

markerHeight :: forall a. String -> Attribute a
markerHeight = attr "markerHeight"

markerMid :: forall a. String -> Attribute a
markerMid = attr "markerMid"

markerStart :: forall a. String -> Attribute a
markerStart = attr "markerStart"

markerUnits :: forall a. String -> Attribute a
markerUnits = attr "markerUnits"

markerWidth :: forall a. String -> Attribute a
markerWidth = attr "markerWidth"

mask :: forall a. String -> Attribute a
mask = attr "mask"

maskContentUnits :: forall a. String -> Attribute a
maskContentUnits = attr "maskContentUnits"

maskUnits :: forall a. String -> Attribute a
maskUnits = attr "maskUnits"

mathematical :: forall a. String -> Attribute a
mathematical = attr "mathematical"

mode :: forall a. String -> Attribute a
mode = attr "mode"

numOctaves :: forall a. String -> Attribute a
numOctaves = attr "numOctaves"

offset :: forall a. String -> Attribute a
offset = attr "offset"

opacity :: forall a. String -> Attribute a
opacity = attr "opacity"

operator :: forall a. String -> Attribute a
operator = attr "operator"

order :: forall a. String -> Attribute a
order = attr "order"

orient :: forall a. String -> Attribute a
orient = attr "orient"

orientation :: forall a. String -> Attribute a
orientation = attr "orientation"

origin :: forall a. String -> Attribute a
origin = attr "origin"

overflow :: forall a. String -> Attribute a
overflow = attr "overflow"

overlinePosition :: forall a. String -> Attribute a
overlinePosition = attr "overlinePosition"

overlineThickness :: forall a. String -> Attribute a
overlineThickness = attr "overlineThickness"

paintOrder :: forall a. String -> Attribute a
paintOrder = attr "paintOrder"

panose1 :: forall a. String -> Attribute a
panose1 = attr "panose1"

pathLength :: forall a. String -> Attribute a
pathLength = attr "pathLength"

patternContentUnits :: forall a. String -> Attribute a
patternContentUnits = attr "patternContentUnits"

patternTransform :: forall a. String -> Attribute a
patternTransform = attr "patternTransform"

patternUnits :: forall a. String -> Attribute a
patternUnits = attr "patternUnits"

pointerEvents :: forall a. String -> Attribute a
pointerEvents = attr "pointerEvents"

points :: forall a. String -> Attribute a
points = attr "points"

pointsAtX :: forall a. String -> Attribute a
pointsAtX = attr "pointsAtX"

pointsAtY :: forall a. String -> Attribute a
pointsAtY = attr "pointsAtY"

pointsAtZ :: forall a. String -> Attribute a
pointsAtZ = attr "pointsAtZ"

preserveAlpha :: forall a. String -> Attribute a
preserveAlpha = attr "preserveAlpha"

preserveAspectRatio :: forall a. String -> Attribute a
preserveAspectRatio = attr "preserveAspectRatio"

primitiveUnits :: forall a. String -> Attribute a
primitiveUnits = attr "primitiveUnits"

r :: forall a. String -> Attribute a
r = attr "r"

radius :: forall a. String -> Attribute a
radius = attr "radius"

refX :: forall a. String -> Attribute a
refX = attr "refX"

refY :: forall a. String -> Attribute a
refY = attr "refY"

renderingIntent :: forall a. String -> Attribute a
renderingIntent = attr "renderingIntent"

repeatCount :: forall a. String -> Attribute a
repeatCount = attr "repeatCount"

repeatDur :: forall a. String -> Attribute a
repeatDur = attr "repeatDur"

requiredExtensions :: forall a. String -> Attribute a
requiredExtensions = attr "requiredExtensions"

requiredFeatures :: forall a. String -> Attribute a
requiredFeatures = attr "requiredFeatures"

restart :: forall a. String -> Attribute a
restart = attr "restart"

result :: forall a. String -> Attribute a
result = attr "result"

rotate :: forall a. String -> Attribute a
rotate = attr "rotate"

rx :: forall a. String -> Attribute a
rx = attr "rx"

ry :: forall a. String -> Attribute a
ry = attr "ry"

scale :: forall a. String -> Attribute a
scale = attr "scale"

seed :: forall a. String -> Attribute a
seed = attr "seed"

shapeRendering :: forall a. String -> Attribute a
shapeRendering = attr "shapeRendering"

slope :: forall a. String -> Attribute a
slope = attr "slope"

spacing :: forall a. String -> Attribute a
spacing = attr "spacing"

specularConstant :: forall a. String -> Attribute a
specularConstant = attr "specularConstant"

specularExponent :: forall a. String -> Attribute a
specularExponent = attr "specularExponent"

speed :: forall a. String -> Attribute a
speed = attr "speed"

spreadMethod :: forall a. String -> Attribute a
spreadMethod = attr "spreadMethod"

startOffset :: forall a. String -> Attribute a
startOffset = attr "startOffset"

stdDeviation :: forall a. String -> Attribute a
stdDeviation = attr "stdDeviation"

stemh :: forall a. String -> Attribute a
stemh = attr "stemh"

stemv :: forall a. String -> Attribute a
stemv = attr "stemv"

stitchTiles :: forall a. String -> Attribute a
stitchTiles = attr "stitchTiles"

stopColor :: forall a. String -> Attribute a
stopColor = attr "stopColor"

stopOpacity :: forall a. String -> Attribute a
stopOpacity = attr "stopOpacity"

strikethroughPosition :: forall a. String -> Attribute a
strikethroughPosition = attr "strikethroughPosition"

strikethroughThickness :: forall a. String -> Attribute a
strikethroughThickness = attr "strikethroughThickness"

string :: forall a. String -> Attribute a
string = attr "string"

stroke :: forall a. String -> Attribute a
stroke = attr "stroke"

strokeDasharray :: forall a. String -> Attribute a
strokeDasharray = attr "strokeDasharray"

strokeDashoffset :: forall a. String -> Attribute a
strokeDashoffset = attr "strokeDashoffset"

strokeLinecap :: forall a. String -> Attribute a
strokeLinecap = attr "strokeLinecap"

strokeLinejoin :: forall a. String -> Attribute a
strokeLinejoin = attr "strokeLinejoin"

strokeMiterlimit :: forall a. String -> Attribute a
strokeMiterlimit = attr "strokeMiterlimit"

strokeOpacity :: forall a. String -> Attribute a
strokeOpacity = attr "strokeOpacity"

strokeWidth :: forall a. String -> Attribute a
strokeWidth = attr "strokeWidth"

surfaceScale :: forall a. String -> Attribute a
surfaceScale = attr "surfaceScale"

systemLanguage :: forall a. String -> Attribute a
systemLanguage = attr "systemLanguage"

tableValues :: forall a. String -> Attribute a
tableValues = attr "tableValues"

targetX :: forall a. String -> Attribute a
targetX = attr "targetX"

targetY :: forall a. String -> Attribute a
targetY = attr "targetY"

textAnchor :: forall a. String -> Attribute a
textAnchor = attr "textAnchor"

textDecoration :: forall a. String -> Attribute a
textDecoration = attr "textDecoration"

textLength :: forall a. String -> Attribute a
textLength = attr "textLength"

textRendering :: forall a. String -> Attribute a
textRendering = attr "textRendering"

to :: forall a. String -> Attribute a
to = attr "to"

transform :: forall a. String -> Attribute a
transform = attr "transform"

u1 :: forall a. String -> Attribute a
u1 = attr "u1"

u2 :: forall a. String -> Attribute a
u2 = attr "u2"

underlinePosition :: forall a. String -> Attribute a
underlinePosition = attr "underlinePosition"

underlineThickness :: forall a. String -> Attribute a
underlineThickness = attr "underlineThickness"

unicode :: forall a. String -> Attribute a
unicode = attr "unicode"

unicodeBidi :: forall a. String -> Attribute a
unicodeBidi = attr "unicodeBidi"

unicodeRange :: forall a. String -> Attribute a
unicodeRange = attr "unicodeRange"

unitsPerEm :: forall a. String -> Attribute a
unitsPerEm = attr "unitsPerEm"

vAlphabetic :: forall a. String -> Attribute a
vAlphabetic = attr "vAlphabetic"

vHanging :: forall a. String -> Attribute a
vHanging = attr "vHanging"

vIdeographic :: forall a. String -> Attribute a
vIdeographic = attr "vIdeographic"

vMathematical :: forall a. String -> Attribute a
vMathematical = attr "vMathematical"

values :: forall a. String -> Attribute a
values = attr "values"

vectorEffect :: forall a. String -> Attribute a
vectorEffect = attr "vectorEffect"

version :: forall a. String -> Attribute a
version = attr "version"

vertAdvY :: forall a. String -> Attribute a
vertAdvY = attr "vertAdvY"

vertOriginX :: forall a. String -> Attribute a
vertOriginX = attr "vertOriginX"

vertOriginY :: forall a. String -> Attribute a
vertOriginY = attr "vertOriginY"

viewBox :: forall a. String -> Attribute a
viewBox = attr "viewBox"

viewTarget :: forall a. String -> Attribute a
viewTarget = attr "viewTarget"

visibility :: forall a. String -> Attribute a
visibility = attr "visibility"

widths :: forall a. String -> Attribute a
widths = attr "widths"

wordSpacing :: forall a. String -> Attribute a
wordSpacing = attr "wordSpacing"

writingMode :: forall a. String -> Attribute a
writingMode = attr "writingMode"

x :: forall a. String -> Attribute a
x = attr "x"

x1 :: forall a. String -> Attribute a
x1 = attr "x1"

x2 :: forall a. String -> Attribute a
x2 = attr "x2"

xChannelSelector :: forall a. String -> Attribute a
xChannelSelector = attr "xChannelSelector"

xHeight :: forall a. String -> Attribute a
xHeight = attr "xHeight"

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

y :: forall a. String -> Attribute a
y = attr "y"

y1 :: forall a. String -> Attribute a
y1 = attr "y1"

y2 :: forall a. String -> Attribute a
y2 = attr "y2"

yChannelSelector :: forall a. String -> Attribute a
yChannelSelector = attr "yChannelSelector"

z :: forall a. String -> Attribute a
z = attr "z"

zoomAndPan :: forall a. String -> Attribute a
zoomAndPan = attr "zoomAndPan"

foreign import attr :: forall a v. String -> v -> Attribute a
