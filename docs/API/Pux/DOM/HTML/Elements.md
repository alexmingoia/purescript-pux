## Module Pux.DOM.HTML.Elements

Example:

```purescript
import Pux
import Pux.DOM.HTML.Elements (div, p, button, text)
import Pux.DOM.HTML.Attributes (onClick, send)

view :: State -> VirtualDOM
view state = div $ do
  p $ text ("Counter: " ++ show state.counter)
  p $ do
    button ! onClick (send Increment) $ text "Increment"
    button ! onClick (send Decrement) $ text "Decrement"
```

#### `parent`

``` purescript
parent :: String -> VirtualDOM -> VirtualDOM
```

#### `leaf`

``` purescript
leaf :: String -> VirtualDOM
```

#### `text`

``` purescript
text :: String -> VirtualDOM
```

#### `a`

``` purescript
a :: VirtualDOM -> VirtualDOM
```

#### `abbr`

``` purescript
abbr :: VirtualDOM -> VirtualDOM
```

#### `address`

``` purescript
address :: VirtualDOM -> VirtualDOM
```

#### `area`

``` purescript
area :: VirtualDOM
```

#### `article`

``` purescript
article :: VirtualDOM -> VirtualDOM
```

#### `aside`

``` purescript
aside :: VirtualDOM -> VirtualDOM
```

#### `audio`

``` purescript
audio :: VirtualDOM -> VirtualDOM
```

#### `b`

``` purescript
b :: VirtualDOM -> VirtualDOM
```

#### `base`

``` purescript
base :: VirtualDOM
```

#### `bdi`

``` purescript
bdi :: VirtualDOM -> VirtualDOM
```

#### `bdo`

``` purescript
bdo :: VirtualDOM -> VirtualDOM
```

#### `big`

``` purescript
big :: VirtualDOM -> VirtualDOM
```

#### `blockquote`

``` purescript
blockquote :: VirtualDOM -> VirtualDOM
```

#### `body`

``` purescript
body :: VirtualDOM -> VirtualDOM
```

#### `br`

``` purescript
br :: VirtualDOM
```

#### `button`

``` purescript
button :: VirtualDOM -> VirtualDOM
```

#### `canvas`

``` purescript
canvas :: VirtualDOM -> VirtualDOM
```

#### `caption`

``` purescript
caption :: VirtualDOM -> VirtualDOM
```

#### `cite`

``` purescript
cite :: VirtualDOM -> VirtualDOM
```

#### `code`

``` purescript
code :: VirtualDOM -> VirtualDOM
```

#### `col`

``` purescript
col :: VirtualDOM
```

#### `colgroup`

``` purescript
colgroup :: VirtualDOM -> VirtualDOM
```

#### `data_`

``` purescript
data_ :: VirtualDOM -> VirtualDOM
```

#### `datalist`

``` purescript
datalist :: VirtualDOM -> VirtualDOM
```

#### `dd`

``` purescript
dd :: VirtualDOM -> VirtualDOM
```

#### `del`

``` purescript
del :: VirtualDOM -> VirtualDOM
```

#### `details`

``` purescript
details :: VirtualDOM -> VirtualDOM
```

#### `dfn`

``` purescript
dfn :: VirtualDOM -> VirtualDOM
```

#### `dialog`

``` purescript
dialog :: VirtualDOM -> VirtualDOM
```

#### `div`

``` purescript
div :: VirtualDOM -> VirtualDOM
```

#### `dl`

``` purescript
dl :: VirtualDOM -> VirtualDOM
```

#### `dt`

``` purescript
dt :: VirtualDOM -> VirtualDOM
```

#### `em`

``` purescript
em :: VirtualDOM -> VirtualDOM
```

#### `embed`

``` purescript
embed :: VirtualDOM
```

#### `fieldset`

``` purescript
fieldset :: VirtualDOM -> VirtualDOM
```

#### `figcaption`

``` purescript
figcaption :: VirtualDOM -> VirtualDOM
```

#### `figure`

``` purescript
figure :: VirtualDOM -> VirtualDOM
```

#### `footer`

``` purescript
footer :: VirtualDOM -> VirtualDOM
```

#### `form`

``` purescript
form :: VirtualDOM -> VirtualDOM
```

#### `h1`

``` purescript
h1 :: VirtualDOM -> VirtualDOM
```

#### `h2`

``` purescript
h2 :: VirtualDOM -> VirtualDOM
```

#### `h3`

``` purescript
h3 :: VirtualDOM -> VirtualDOM
```

#### `h4`

``` purescript
h4 :: VirtualDOM -> VirtualDOM
```

#### `h5`

``` purescript
h5 :: VirtualDOM -> VirtualDOM
```

#### `h6`

``` purescript
h6 :: VirtualDOM -> VirtualDOM
```

#### `head`

``` purescript
head :: VirtualDOM -> VirtualDOM
```

#### `header`

``` purescript
header :: VirtualDOM -> VirtualDOM
```

#### `hr`

``` purescript
hr :: VirtualDOM
```

#### `html`

``` purescript
html :: VirtualDOM -> VirtualDOM
```

#### `i`

``` purescript
i :: VirtualDOM -> VirtualDOM
```

#### `iframe`

``` purescript
iframe :: VirtualDOM -> VirtualDOM
```

#### `img`

``` purescript
img :: VirtualDOM
```

#### `input`

``` purescript
input :: VirtualDOM
```

#### `ins`

``` purescript
ins :: VirtualDOM -> VirtualDOM
```

#### `kbd`

``` purescript
kbd :: VirtualDOM -> VirtualDOM
```

#### `keygen`

``` purescript
keygen :: VirtualDOM
```

#### `label`

``` purescript
label :: VirtualDOM -> VirtualDOM
```

#### `legend`

``` purescript
legend :: VirtualDOM -> VirtualDOM
```

#### `li`

``` purescript
li :: VirtualDOM -> VirtualDOM
```

#### `link`

``` purescript
link :: VirtualDOM
```

#### `main`

``` purescript
main :: VirtualDOM -> VirtualDOM
```

#### `map`

``` purescript
map :: VirtualDOM -> VirtualDOM
```

#### `mark`

``` purescript
mark :: VirtualDOM -> VirtualDOM
```

#### `menu`

``` purescript
menu :: VirtualDOM -> VirtualDOM
```

#### `menuitem`

``` purescript
menuitem :: VirtualDOM
```

#### `meta`

``` purescript
meta :: VirtualDOM
```

#### `meter`

``` purescript
meter :: VirtualDOM -> VirtualDOM
```

#### `nav`

``` purescript
nav :: VirtualDOM -> VirtualDOM
```

#### `noscript`

``` purescript
noscript :: VirtualDOM -> VirtualDOM
```

#### `object`

``` purescript
object :: VirtualDOM -> VirtualDOM
```

#### `ol`

``` purescript
ol :: VirtualDOM -> VirtualDOM
```

#### `optgroup`

``` purescript
optgroup :: VirtualDOM -> VirtualDOM
```

#### `option`

``` purescript
option :: VirtualDOM -> VirtualDOM
```

#### `output`

``` purescript
output :: VirtualDOM -> VirtualDOM
```

#### `p`

``` purescript
p :: VirtualDOM -> VirtualDOM
```

#### `param`

``` purescript
param :: VirtualDOM
```

#### `picture`

``` purescript
picture :: VirtualDOM -> VirtualDOM
```

#### `pre`

``` purescript
pre :: VirtualDOM -> VirtualDOM
```

#### `progress`

``` purescript
progress :: VirtualDOM -> VirtualDOM
```

#### `q`

``` purescript
q :: VirtualDOM -> VirtualDOM
```

#### `rp`

``` purescript
rp :: VirtualDOM -> VirtualDOM
```

#### `rt`

``` purescript
rt :: VirtualDOM -> VirtualDOM
```

#### `ruby`

``` purescript
ruby :: VirtualDOM -> VirtualDOM
```

#### `s`

``` purescript
s :: VirtualDOM -> VirtualDOM
```

#### `samp`

``` purescript
samp :: VirtualDOM -> VirtualDOM
```

#### `script`

``` purescript
script :: VirtualDOM -> VirtualDOM
```

#### `section`

``` purescript
section :: VirtualDOM -> VirtualDOM
```

#### `select`

``` purescript
select :: VirtualDOM -> VirtualDOM
```

#### `small`

``` purescript
small :: VirtualDOM -> VirtualDOM
```

#### `source`

``` purescript
source :: VirtualDOM
```

#### `span`

``` purescript
span :: VirtualDOM -> VirtualDOM
```

#### `strong`

``` purescript
strong :: VirtualDOM -> VirtualDOM
```

#### `style`

``` purescript
style :: VirtualDOM -> VirtualDOM
```

#### `sub`

``` purescript
sub :: VirtualDOM -> VirtualDOM
```

#### `summary`

``` purescript
summary :: VirtualDOM -> VirtualDOM
```

#### `sup`

``` purescript
sup :: VirtualDOM -> VirtualDOM
```

#### `table`

``` purescript
table :: VirtualDOM -> VirtualDOM
```

#### `tbody`

``` purescript
tbody :: VirtualDOM -> VirtualDOM
```

#### `td`

``` purescript
td :: VirtualDOM -> VirtualDOM
```

#### `textarea`

``` purescript
textarea :: VirtualDOM -> VirtualDOM
```

#### `tfoot`

``` purescript
tfoot :: VirtualDOM -> VirtualDOM
```

#### `th`

``` purescript
th :: VirtualDOM -> VirtualDOM
```

#### `thead`

``` purescript
thead :: VirtualDOM -> VirtualDOM
```

#### `time`

``` purescript
time :: VirtualDOM -> VirtualDOM
```

#### `title`

``` purescript
title :: VirtualDOM -> VirtualDOM
```

#### `tr`

``` purescript
tr :: VirtualDOM -> VirtualDOM
```

#### `track`

``` purescript
track :: VirtualDOM
```

#### `u`

``` purescript
u :: VirtualDOM -> VirtualDOM
```

#### `ul`

``` purescript
ul :: VirtualDOM -> VirtualDOM
```

#### `var`

``` purescript
var :: VirtualDOM -> VirtualDOM
```

#### `video`

``` purescript
video :: VirtualDOM -> VirtualDOM
```

#### `wbr`

``` purescript
wbr :: VirtualDOM
```


