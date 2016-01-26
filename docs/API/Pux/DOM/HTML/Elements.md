## Module Pux.DOM.HTML.Elements

Example:

```purescript
import Pux
import Pux.DOM.HTML.Elements (div, p, button, text)
import Pux.DOM.HTML.Attributes (onClick, send)

view :: View State
view state children = div $ do
  p $ text ("Counter: " ++ show state.counter)
  p $ do
    button ! onClick (send Increment) $ text "Increment"
    button ! onClick (send Decrement) $ text "Decrement"
```

#### `parent`

``` purescript
parent :: String -> VDom -> VDom
```

#### `leaf`

``` purescript
leaf :: String -> VDom
```

#### `text`

``` purescript
text :: String -> VDom
```

#### `a`

``` purescript
a :: VDom -> VDom
```

#### `abbr`

``` purescript
abbr :: VDom -> VDom
```

#### `address`

``` purescript
address :: VDom -> VDom
```

#### `area`

``` purescript
area :: VDom
```

#### `article`

``` purescript
article :: VDom -> VDom
```

#### `aside`

``` purescript
aside :: VDom -> VDom
```

#### `audio`

``` purescript
audio :: VDom -> VDom
```

#### `b`

``` purescript
b :: VDom -> VDom
```

#### `base`

``` purescript
base :: VDom
```

#### `bdi`

``` purescript
bdi :: VDom -> VDom
```

#### `bdo`

``` purescript
bdo :: VDom -> VDom
```

#### `big`

``` purescript
big :: VDom -> VDom
```

#### `blockquote`

``` purescript
blockquote :: VDom -> VDom
```

#### `body`

``` purescript
body :: VDom -> VDom
```

#### `br`

``` purescript
br :: VDom
```

#### `button`

``` purescript
button :: VDom -> VDom
```

#### `canvas`

``` purescript
canvas :: VDom -> VDom
```

#### `caption`

``` purescript
caption :: VDom -> VDom
```

#### `cite`

``` purescript
cite :: VDom -> VDom
```

#### `code`

``` purescript
code :: VDom -> VDom
```

#### `col`

``` purescript
col :: VDom
```

#### `colgroup`

``` purescript
colgroup :: VDom -> VDom
```

#### `data_`

``` purescript
data_ :: VDom -> VDom
```

#### `datalist`

``` purescript
datalist :: VDom -> VDom
```

#### `dd`

``` purescript
dd :: VDom -> VDom
```

#### `del`

``` purescript
del :: VDom -> VDom
```

#### `details`

``` purescript
details :: VDom -> VDom
```

#### `dfn`

``` purescript
dfn :: VDom -> VDom
```

#### `dialog`

``` purescript
dialog :: VDom -> VDom
```

#### `div`

``` purescript
div :: VDom -> VDom
```

#### `dl`

``` purescript
dl :: VDom -> VDom
```

#### `dt`

``` purescript
dt :: VDom -> VDom
```

#### `em`

``` purescript
em :: VDom -> VDom
```

#### `embed`

``` purescript
embed :: VDom
```

#### `fieldset`

``` purescript
fieldset :: VDom -> VDom
```

#### `figcaption`

``` purescript
figcaption :: VDom -> VDom
```

#### `figure`

``` purescript
figure :: VDom -> VDom
```

#### `footer`

``` purescript
footer :: VDom -> VDom
```

#### `form`

``` purescript
form :: VDom -> VDom
```

#### `h1`

``` purescript
h1 :: VDom -> VDom
```

#### `h2`

``` purescript
h2 :: VDom -> VDom
```

#### `h3`

``` purescript
h3 :: VDom -> VDom
```

#### `h4`

``` purescript
h4 :: VDom -> VDom
```

#### `h5`

``` purescript
h5 :: VDom -> VDom
```

#### `h6`

``` purescript
h6 :: VDom -> VDom
```

#### `head`

``` purescript
head :: VDom -> VDom
```

#### `header`

``` purescript
header :: VDom -> VDom
```

#### `hr`

``` purescript
hr :: VDom
```

#### `html`

``` purescript
html :: VDom -> VDom
```

#### `i`

``` purescript
i :: VDom -> VDom
```

#### `iframe`

``` purescript
iframe :: VDom -> VDom
```

#### `img`

``` purescript
img :: VDom
```

#### `input`

``` purescript
input :: VDom
```

#### `ins`

``` purescript
ins :: VDom -> VDom
```

#### `kbd`

``` purescript
kbd :: VDom -> VDom
```

#### `keygen`

``` purescript
keygen :: VDom
```

#### `label`

``` purescript
label :: VDom -> VDom
```

#### `legend`

``` purescript
legend :: VDom -> VDom
```

#### `li`

``` purescript
li :: VDom -> VDom
```

#### `link`

``` purescript
link :: VDom
```

#### `main`

``` purescript
main :: VDom -> VDom
```

#### `map`

``` purescript
map :: VDom -> VDom
```

#### `mark`

``` purescript
mark :: VDom -> VDom
```

#### `menu`

``` purescript
menu :: VDom -> VDom
```

#### `menuitem`

``` purescript
menuitem :: VDom
```

#### `meta`

``` purescript
meta :: VDom
```

#### `meter`

``` purescript
meter :: VDom -> VDom
```

#### `nav`

``` purescript
nav :: VDom -> VDom
```

#### `noscript`

``` purescript
noscript :: VDom -> VDom
```

#### `object`

``` purescript
object :: VDom -> VDom
```

#### `ol`

``` purescript
ol :: VDom -> VDom
```

#### `optgroup`

``` purescript
optgroup :: VDom -> VDom
```

#### `option`

``` purescript
option :: VDom -> VDom
```

#### `output`

``` purescript
output :: VDom -> VDom
```

#### `p`

``` purescript
p :: VDom -> VDom
```

#### `param`

``` purescript
param :: VDom
```

#### `picture`

``` purescript
picture :: VDom -> VDom
```

#### `pre`

``` purescript
pre :: VDom -> VDom
```

#### `progress`

``` purescript
progress :: VDom -> VDom
```

#### `q`

``` purescript
q :: VDom -> VDom
```

#### `rp`

``` purescript
rp :: VDom -> VDom
```

#### `rt`

``` purescript
rt :: VDom -> VDom
```

#### `ruby`

``` purescript
ruby :: VDom -> VDom
```

#### `s`

``` purescript
s :: VDom -> VDom
```

#### `samp`

``` purescript
samp :: VDom -> VDom
```

#### `script`

``` purescript
script :: VDom -> VDom
```

#### `section`

``` purescript
section :: VDom -> VDom
```

#### `select`

``` purescript
select :: VDom -> VDom
```

#### `small`

``` purescript
small :: VDom -> VDom
```

#### `source`

``` purescript
source :: VDom
```

#### `span`

``` purescript
span :: VDom -> VDom
```

#### `strong`

``` purescript
strong :: VDom -> VDom
```

#### `style`

``` purescript
style :: VDom -> VDom
```

#### `sub`

``` purescript
sub :: VDom -> VDom
```

#### `summary`

``` purescript
summary :: VDom -> VDom
```

#### `sup`

``` purescript
sup :: VDom -> VDom
```

#### `table`

``` purescript
table :: VDom -> VDom
```

#### `tbody`

``` purescript
tbody :: VDom -> VDom
```

#### `td`

``` purescript
td :: VDom -> VDom
```

#### `textarea`

``` purescript
textarea :: VDom -> VDom
```

#### `tfoot`

``` purescript
tfoot :: VDom -> VDom
```

#### `th`

``` purescript
th :: VDom -> VDom
```

#### `thead`

``` purescript
thead :: VDom -> VDom
```

#### `time`

``` purescript
time :: VDom -> VDom
```

#### `title`

``` purescript
title :: VDom -> VDom
```

#### `tr`

``` purescript
tr :: VDom -> VDom
```

#### `track`

``` purescript
track :: VDom
```

#### `u`

``` purescript
u :: VDom -> VDom
```

#### `ul`

``` purescript
ul :: VDom -> VDom
```

#### `var`

``` purescript
var :: VDom -> VDom
```

#### `video`

``` purescript
video :: VDom -> VDom
```

#### `wbr`

``` purescript
wbr :: VDom
```


