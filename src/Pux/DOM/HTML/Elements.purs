-- | Example:
-- |
-- | ```purescript
-- | import Pux
-- | import Pux.DOM.HTML.Elements (div, p, button, text)
-- | import Pux.DOM.HTML.Attributes (onClick, send)
-- |
-- | view :: View State
-- | view state children = div $ do
-- |   p $ text ("Counter: " ++ show state.counter)
-- |   p $ do
-- |     button ! onClick (send Increment) $ text "Increment"
-- |     button ! onClick (send Decrement) $ text "Decrement"
-- | ```

module Pux.DOM.HTML.Elements where

import Data.Maybe
import Prelude (unit)
import Pux.View

parent :: String -> VDom -> VDom
parent el children = Node el (Just children) [] [] (Return unit)

leaf :: String -> VDom
leaf el = Node el Nothing [] [] (Return unit)

text :: String -> VDom
text str = Content str (Return unit)

a :: VDom -> VDom
a = parent "a"

abbr :: VDom -> VDom
abbr = parent "abbr"

address :: VDom -> VDom
address = parent "address"

area :: VDom
area = leaf "area"

article :: VDom -> VDom
article = parent "article"

aside :: VDom -> VDom
aside = parent "aside"

audio :: VDom -> VDom
audio = parent "audio"

b :: VDom -> VDom
b = parent "b"

base :: VDom
base = leaf "base"

bdi :: VDom -> VDom
bdi = parent "bdi"

bdo :: VDom -> VDom
bdo = parent "bdo"

big :: VDom -> VDom
big = parent "big"

blockquote :: VDom -> VDom
blockquote = parent "blockquote"

body :: VDom -> VDom
body = parent "body"

br :: VDom
br = leaf "br"

button :: VDom -> VDom
button = parent "button"

canvas :: VDom -> VDom
canvas = parent "canvas"

caption :: VDom -> VDom
caption = parent "caption"

cite :: VDom -> VDom
cite = parent "cite"

code :: VDom -> VDom
code = parent "code"

col :: VDom
col = leaf "col"

colgroup :: VDom -> VDom
colgroup = parent "colgroup"

data_ :: VDom -> VDom
data_ = parent "data"

datalist :: VDom -> VDom
datalist = parent "datalist"

dd :: VDom -> VDom
dd = parent "dd"

del :: VDom -> VDom
del = parent "del"

details :: VDom -> VDom
details = parent "details"

dfn :: VDom -> VDom
dfn = parent "dfn"

dialog :: VDom -> VDom
dialog = parent "dialog"

div :: VDom -> VDom
div = parent "div"

dl :: VDom -> VDom
dl = parent "dl"

dt :: VDom -> VDom
dt = parent "dt"

em :: VDom -> VDom
em = parent "em"

embed :: VDom
embed = leaf "embed"

fieldset :: VDom -> VDom
fieldset = parent "fieldset"

figcaption :: VDom -> VDom
figcaption = parent "figcaption"

figure :: VDom -> VDom
figure = parent "figure"

footer :: VDom -> VDom
footer = parent "footer"

form :: VDom -> VDom
form = parent "form"

h1 :: VDom -> VDom
h1 = parent "h1"

h2 :: VDom -> VDom
h2 = parent "h2"

h3 :: VDom -> VDom
h3 = parent "h3"

h4 :: VDom -> VDom
h4 = parent "h4"

h5 :: VDom -> VDom
h5 = parent "h5"

h6 :: VDom -> VDom
h6 = parent "h6"

head :: VDom -> VDom
head = parent "head"

header :: VDom -> VDom
header = parent "header"

hr :: VDom
hr = leaf "hr"

html :: VDom -> VDom
html = parent "html"

i :: VDom -> VDom
i = parent "i"

iframe :: VDom -> VDom
iframe = parent "iframe"

img :: VDom
img = leaf "img"

input :: VDom
input = leaf "input"

ins :: VDom -> VDom
ins = parent "ins"

kbd :: VDom -> VDom
kbd = parent "kbd"

keygen :: VDom
keygen = leaf "keygen"

label :: VDom -> VDom
label = parent "label"

legend :: VDom -> VDom
legend = parent "legend"

li :: VDom -> VDom
li = parent "li"

link :: VDom
link = leaf "link"

main :: VDom -> VDom
main = parent "main"

map :: VDom -> VDom
map = parent "map"

mark :: VDom -> VDom
mark = parent "mark"

menu :: VDom -> VDom
menu = parent "menu"

menuitem :: VDom
menuitem = leaf "menuitem"

meta :: VDom
meta = leaf "meta"

meter :: VDom -> VDom
meter = parent "meter"

nav :: VDom -> VDom
nav = parent "nav"

noscript :: VDom -> VDom
noscript = parent "noscript"

object :: VDom -> VDom
object = parent "object"

ol :: VDom -> VDom
ol = parent "ol"

optgroup :: VDom -> VDom
optgroup = parent "optgroup"

option :: VDom -> VDom
option = parent "option"

output :: VDom -> VDom
output = parent "output"

p :: VDom -> VDom
p = parent "p"

param :: VDom
param = leaf "param"

picture :: VDom -> VDom
picture = parent "picture"

pre :: VDom -> VDom
pre = parent "pre"

progress :: VDom -> VDom
progress = parent "progress"

q :: VDom -> VDom
q = parent "q"

rp :: VDom -> VDom
rp = parent "rp"

rt :: VDom -> VDom
rt = parent "rt"

ruby :: VDom -> VDom
ruby = parent "ruby"

s :: VDom -> VDom
s = parent "s"

samp :: VDom -> VDom
samp = parent "samp"

script :: VDom -> VDom
script = parent "script"

section :: VDom -> VDom
section = parent "section"

select :: VDom -> VDom
select = parent "select"

small :: VDom -> VDom
small = parent "small"

source :: VDom
source = leaf "source"

span :: VDom -> VDom
span = parent "span"

strong :: VDom -> VDom
strong = parent "strong"

style :: VDom -> VDom
style = parent "style"

sub :: VDom -> VDom
sub = parent "sub"

summary :: VDom -> VDom
summary = parent "summary"

sup :: VDom -> VDom
sup = parent "sup"

table :: VDom -> VDom
table = parent "table"

tbody :: VDom -> VDom
tbody = parent "tbody"

td :: VDom -> VDom
td = parent "td"

textarea :: VDom -> VDom
textarea = parent "textarea"

tfoot :: VDom -> VDom
tfoot = parent "tfoot"

th :: VDom -> VDom
th = parent "th"

thead :: VDom -> VDom
thead = parent "thead"

time :: VDom -> VDom
time = parent "time"

title :: VDom -> VDom
title = parent "title"

tr :: VDom -> VDom
tr = parent "tr"

track :: VDom
track = leaf "track"

u :: VDom -> VDom
u = parent "u"

ul :: VDom -> VDom
ul = parent "ul"

var :: VDom -> VDom
var = parent "var"

video :: VDom -> VDom
video = parent "video"

wbr :: VDom
wbr = leaf "body"
