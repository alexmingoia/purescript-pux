## Module Pux.Html

#### `bind`

``` purescript
bind :: forall a. Html a -> (Unit -> Html a) -> Html a
```

This version of bind is for appending `Html` using a `do` block.

```purescript
import Pux.Html (Html, (#), (!), bind, div, span, button, text)
import Pux.Html.Events (onClick)

view :: State -> Html Action
view state =
  div # do
    button ! onClick (const Increment) # text "Increment"
    span # text (show count)
    button ! onClick (const Decrement) # text "Decrement"
```

#### `forwardTo`

``` purescript
forwardTo :: forall a b. (a -> b) -> Html a -> Html b
```

Forward child `Html` actions to their parent action. `forwardTo` maps
over `Html` that sends actions of type `a` and returns `Html` that sends
actions of type `b`.

```purescript
view :: State -> Html Action
view state =
  div # do
    forwardTo Top $ Counter.view state.topCount
    forwardTo Bottom $ Counter.view state.bottomCount
    button ! onClick (const Reset) # text "Reset"
```

#### `withAttr`

``` purescript
withAttr :: forall a. (Array (Attribute a) -> Array (Html a) -> Html a) -> Attribute a -> Array (Attribute a) -> Array (Html a) -> Html a
```

Combine elements with attributes.

```purescript
button ! className "primary" ! onClick (const Increment) # text "Increment"
```

#### `(!)`

``` purescript
infixl 4 withAttr as !
```

_left-associative / precedence 4_

#### `withChild`

``` purescript
withChild :: forall a. (Array (Attribute a) -> Array (Html a) -> Html a) -> Html a -> Html a
```

Append child to parent element.

```purescript
div # do
  button ! onClick (const Increment) # text "Increment"
  span # text ("Counter: " ++ show count)
  button ! onClick (const Decrement) # text "Decrement"
```

#### `(#)`

``` purescript
infixl 4 withChild as #
```

_left-associative / precedence 4_

#### `withChildren`

``` purescript
withChildren :: forall a. (Array (Attribute a) -> Array (Html a) -> Html a) -> Array (Html a) -> Html a
```

#### `(##)`

``` purescript
infixl 4 withChildren as ##
```

_left-associative / precedence 4_


### Re-exported from Pux.Html.Elements:

#### `Attribute`

``` purescript
data Attribute :: * -> *
```

#### `Html`

``` purescript
data Html :: * -> *
```

#### `a`

``` purescript
a :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `abbr`

``` purescript
abbr :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `address`

``` purescript
address :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `altGlyph`

``` purescript
altGlyph :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

## SVG Elements

#### `altGlyphDef`

``` purescript
altGlyphDef :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `altGlyphItem`

``` purescript
altGlyphItem :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `animate`

``` purescript
animate :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `animateColor`

``` purescript
animateColor :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `animateMotion`

``` purescript
animateMotion :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `animateTransform`

``` purescript
animateTransform :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `area`

``` purescript
area :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `article`

``` purescript
article :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `aside`

``` purescript
aside :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `audio`

``` purescript
audio :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `b`

``` purescript
b :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `base`

``` purescript
base :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `bdi`

``` purescript
bdi :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `bdo`

``` purescript
bdo :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `big`

``` purescript
big :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `blockquote`

``` purescript
blockquote :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `body`

``` purescript
body :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `br`

``` purescript
br :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `button`

``` purescript
button :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `canvas`

``` purescript
canvas :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `caption`

``` purescript
caption :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `circle`

``` purescript
circle :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `cite`

``` purescript
cite :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `clipPath`

``` purescript
clipPath :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `code`

``` purescript
code :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `col`

``` purescript
col :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `colgroup`

``` purescript
colgroup :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `colorProfile`

``` purescript
colorProfile :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `cursor`

``` purescript
cursor :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `data_`

``` purescript
data_ :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `datalist`

``` purescript
datalist :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `dd`

``` purescript
dd :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `defs`

``` purescript
defs :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `del`

``` purescript
del :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `desc`

``` purescript
desc :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `details`

``` purescript
details :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `dfn`

``` purescript
dfn :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `dialog`

``` purescript
dialog :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `div`

``` purescript
div :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `dl`

``` purescript
dl :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `dt`

``` purescript
dt :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `element`

``` purescript
element :: forall a. Fn3 String (Array (Attribute a)) (Array (Html a)) (Html a)
```

#### `ellipse`

``` purescript
ellipse :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `em`

``` purescript
em :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `embed`

``` purescript
embed :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `feBlend`

``` purescript
feBlend :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `feColorMatrix`

``` purescript
feColorMatrix :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `feComponentTransfer`

``` purescript
feComponentTransfer :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `feComposite`

``` purescript
feComposite :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `feConvolveMatrix`

``` purescript
feConvolveMatrix :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `feDiffuseLighting`

``` purescript
feDiffuseLighting :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `feDisplacementMap`

``` purescript
feDisplacementMap :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `feDistantLight`

``` purescript
feDistantLight :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `feFlood`

``` purescript
feFlood :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `feFuncA`

``` purescript
feFuncA :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `feFuncB`

``` purescript
feFuncB :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `feFuncG`

``` purescript
feFuncG :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `feFuncR`

``` purescript
feFuncR :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `feGaussianBlur`

``` purescript
feGaussianBlur :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `feImage`

``` purescript
feImage :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `feMerge`

``` purescript
feMerge :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `feMergeNode`

``` purescript
feMergeNode :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `feMorphology`

``` purescript
feMorphology :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `feOffset`

``` purescript
feOffset :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `fePointLight`

``` purescript
fePointLight :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `feSpecularLighting`

``` purescript
feSpecularLighting :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `feSpotLight`

``` purescript
feSpotLight :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `feTile`

``` purescript
feTile :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `feTurbulence`

``` purescript
feTurbulence :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `fieldset`

``` purescript
fieldset :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `figcaption`

``` purescript
figcaption :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `figure`

``` purescript
figure :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `filter`

``` purescript
filter :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `font`

``` purescript
font :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `fontFace`

``` purescript
fontFace :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `fontFaceFormat`

``` purescript
fontFaceFormat :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `fontFaceName`

``` purescript
fontFaceName :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `fontFaceSrc`

``` purescript
fontFaceSrc :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `fontFaceUri`

``` purescript
fontFaceUri :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `footer`

``` purescript
footer :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `foreignObject`

``` purescript
foreignObject :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `form`

``` purescript
form :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `g`

``` purescript
g :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `glyph`

``` purescript
glyph :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `glyphRef`

``` purescript
glyphRef :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `h1`

``` purescript
h1 :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `h2`

``` purescript
h2 :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `h3`

``` purescript
h3 :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `h4`

``` purescript
h4 :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `h5`

``` purescript
h5 :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `h6`

``` purescript
h6 :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `head`

``` purescript
head :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `header`

``` purescript
header :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `hkern`

``` purescript
hkern :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `hr`

``` purescript
hr :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `html`

``` purescript
html :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `i`

``` purescript
i :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `iframe`

``` purescript
iframe :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `image`

``` purescript
image :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `img`

``` purescript
img :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `input`

``` purescript
input :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `ins`

``` purescript
ins :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `kbd`

``` purescript
kbd :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `keygen`

``` purescript
keygen :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `label`

``` purescript
label :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `legend`

``` purescript
legend :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `li`

``` purescript
li :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `line`

``` purescript
line :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `linearGradient`

``` purescript
linearGradient :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `link`

``` purescript
link :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `main`

``` purescript
main :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `map`

``` purescript
map :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `mark`

``` purescript
mark :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `marker`

``` purescript
marker :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `mask`

``` purescript
mask :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `menu`

``` purescript
menu :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `menuitem`

``` purescript
menuitem :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `meta`

``` purescript
meta :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `metadata`

``` purescript
metadata :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `meter`

``` purescript
meter :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `missingGlyph`

``` purescript
missingGlyph :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `mpath`

``` purescript
mpath :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `nav`

``` purescript
nav :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `noscript`

``` purescript
noscript :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `object`

``` purescript
object :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `ol`

``` purescript
ol :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `optgroup`

``` purescript
optgroup :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `option`

``` purescript
option :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `output`

``` purescript
output :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `p`

``` purescript
p :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `param`

``` purescript
param :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `path`

``` purescript
path :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `pattern`

``` purescript
pattern :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `picture`

``` purescript
picture :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `polygon`

``` purescript
polygon :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `polyline`

``` purescript
polyline :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `pre`

``` purescript
pre :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `progress`

``` purescript
progress :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `q`

``` purescript
q :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `radialGradient`

``` purescript
radialGradient :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `rect`

``` purescript
rect :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `rp`

``` purescript
rp :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `rt`

``` purescript
rt :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `ruby`

``` purescript
ruby :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `s`

``` purescript
s :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `samp`

``` purescript
samp :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `script`

``` purescript
script :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `section`

``` purescript
section :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `select`

``` purescript
select :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `set`

``` purescript
set :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `small`

``` purescript
small :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `source`

``` purescript
source :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `span`

``` purescript
span :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `stop`

``` purescript
stop :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `strong`

``` purescript
strong :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `style`

``` purescript
style :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `sub`

``` purescript
sub :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `summary`

``` purescript
summary :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `sup`

``` purescript
sup :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `svg`

``` purescript
svg :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `switch`

``` purescript
switch :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `symbol`

``` purescript
symbol :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `table`

``` purescript
table :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `tbody`

``` purescript
tbody :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `td`

``` purescript
td :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `text`

``` purescript
text :: forall a. String -> Html a
```

#### `textPath`

``` purescript
textPath :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `textarea`

``` purescript
textarea :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `tfoot`

``` purescript
tfoot :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `th`

``` purescript
th :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `thead`

``` purescript
thead :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `time`

``` purescript
time :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `title`

``` purescript
title :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `tr`

``` purescript
tr :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `track`

``` purescript
track :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `tref`

``` purescript
tref :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `tspan`

``` purescript
tspan :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `u`

``` purescript
u :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `ul`

``` purescript
ul :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `use`

``` purescript
use :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `var`

``` purescript
var :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `video`

``` purescript
video :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `view`

``` purescript
view :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `vkern`

``` purescript
vkern :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

#### `wbr`

``` purescript
wbr :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
```

