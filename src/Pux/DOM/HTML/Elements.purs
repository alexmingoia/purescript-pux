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

module Pux.DOM.HTML.Elements where

import Data.Maybe (Maybe(Nothing, Just))
import Prelude (unit)
import Pux.DOM (VirtualDOM, VDomM(Return, Content, Node))

parent :: String -> VirtualDOM -> VirtualDOM
parent el children = Node el (Just children) [] [] (Return unit)

leaf :: String -> VirtualDOM
leaf el = Node el Nothing [] [] (Return unit)

text :: String -> VirtualDOM
text str = Content str (Return unit)

a :: VirtualDOM -> VirtualDOM
a = parent "a"

abbr :: VirtualDOM -> VirtualDOM
abbr = parent "abbr"

address :: VirtualDOM -> VirtualDOM
address = parent "address"

area :: VirtualDOM
area = leaf "area"

article :: VirtualDOM -> VirtualDOM
article = parent "article"

aside :: VirtualDOM -> VirtualDOM
aside = parent "aside"

audio :: VirtualDOM -> VirtualDOM
audio = parent "audio"

b :: VirtualDOM -> VirtualDOM
b = parent "b"

base :: VirtualDOM
base = leaf "base"

bdi :: VirtualDOM -> VirtualDOM
bdi = parent "bdi"

bdo :: VirtualDOM -> VirtualDOM
bdo = parent "bdo"

big :: VirtualDOM -> VirtualDOM
big = parent "big"

blockquote :: VirtualDOM -> VirtualDOM
blockquote = parent "blockquote"

body :: VirtualDOM -> VirtualDOM
body = parent "body"

br :: VirtualDOM
br = leaf "br"

button :: VirtualDOM -> VirtualDOM
button = parent "button"

canvas :: VirtualDOM -> VirtualDOM
canvas = parent "canvas"

caption :: VirtualDOM -> VirtualDOM
caption = parent "caption"

cite :: VirtualDOM -> VirtualDOM
cite = parent "cite"

code :: VirtualDOM -> VirtualDOM
code = parent "code"

col :: VirtualDOM
col = leaf "col"

colgroup :: VirtualDOM -> VirtualDOM
colgroup = parent "colgroup"

data_ :: VirtualDOM -> VirtualDOM
data_ = parent "data"

datalist :: VirtualDOM -> VirtualDOM
datalist = parent "datalist"

dd :: VirtualDOM -> VirtualDOM
dd = parent "dd"

del :: VirtualDOM -> VirtualDOM
del = parent "del"

details :: VirtualDOM -> VirtualDOM
details = parent "details"

dfn :: VirtualDOM -> VirtualDOM
dfn = parent "dfn"

dialog :: VirtualDOM -> VirtualDOM
dialog = parent "dialog"

div :: VirtualDOM -> VirtualDOM
div = parent "div"

dl :: VirtualDOM -> VirtualDOM
dl = parent "dl"

dt :: VirtualDOM -> VirtualDOM
dt = parent "dt"

em :: VirtualDOM -> VirtualDOM
em = parent "em"

embed :: VirtualDOM
embed = leaf "embed"

fieldset :: VirtualDOM -> VirtualDOM
fieldset = parent "fieldset"

figcaption :: VirtualDOM -> VirtualDOM
figcaption = parent "figcaption"

figure :: VirtualDOM -> VirtualDOM
figure = parent "figure"

footer :: VirtualDOM -> VirtualDOM
footer = parent "footer"

form :: VirtualDOM -> VirtualDOM
form = parent "form"

h1 :: VirtualDOM -> VirtualDOM
h1 = parent "h1"

h2 :: VirtualDOM -> VirtualDOM
h2 = parent "h2"

h3 :: VirtualDOM -> VirtualDOM
h3 = parent "h3"

h4 :: VirtualDOM -> VirtualDOM
h4 = parent "h4"

h5 :: VirtualDOM -> VirtualDOM
h5 = parent "h5"

h6 :: VirtualDOM -> VirtualDOM
h6 = parent "h6"

head :: VirtualDOM -> VirtualDOM
head = parent "head"

header :: VirtualDOM -> VirtualDOM
header = parent "header"

hr :: VirtualDOM
hr = leaf "hr"

html :: VirtualDOM -> VirtualDOM
html = parent "html"

i :: VirtualDOM -> VirtualDOM
i = parent "i"

iframe :: VirtualDOM -> VirtualDOM
iframe = parent "iframe"

img :: VirtualDOM
img = leaf "img"

input :: VirtualDOM
input = leaf "input"

ins :: VirtualDOM -> VirtualDOM
ins = parent "ins"

kbd :: VirtualDOM -> VirtualDOM
kbd = parent "kbd"

keygen :: VirtualDOM
keygen = leaf "keygen"

label :: VirtualDOM -> VirtualDOM
label = parent "label"

legend :: VirtualDOM -> VirtualDOM
legend = parent "legend"

li :: VirtualDOM -> VirtualDOM
li = parent "li"

link :: VirtualDOM
link = leaf "link"

main :: VirtualDOM -> VirtualDOM
main = parent "main"

map :: VirtualDOM -> VirtualDOM
map = parent "map"

mark :: VirtualDOM -> VirtualDOM
mark = parent "mark"

menu :: VirtualDOM -> VirtualDOM
menu = parent "menu"

menuitem :: VirtualDOM
menuitem = leaf "menuitem"

meta :: VirtualDOM
meta = leaf "meta"

meter :: VirtualDOM -> VirtualDOM
meter = parent "meter"

nav :: VirtualDOM -> VirtualDOM
nav = parent "nav"

noscript :: VirtualDOM -> VirtualDOM
noscript = parent "noscript"

object :: VirtualDOM -> VirtualDOM
object = parent "object"

ol :: VirtualDOM -> VirtualDOM
ol = parent "ol"

optgroup :: VirtualDOM -> VirtualDOM
optgroup = parent "optgroup"

option :: VirtualDOM -> VirtualDOM
option = parent "option"

output :: VirtualDOM -> VirtualDOM
output = parent "output"

p :: VirtualDOM -> VirtualDOM
p = parent "p"

param :: VirtualDOM
param = leaf "param"

picture :: VirtualDOM -> VirtualDOM
picture = parent "picture"

pre :: VirtualDOM -> VirtualDOM
pre = parent "pre"

progress :: VirtualDOM -> VirtualDOM
progress = parent "progress"

q :: VirtualDOM -> VirtualDOM
q = parent "q"

rp :: VirtualDOM -> VirtualDOM
rp = parent "rp"

rt :: VirtualDOM -> VirtualDOM
rt = parent "rt"

ruby :: VirtualDOM -> VirtualDOM
ruby = parent "ruby"

s :: VirtualDOM -> VirtualDOM
s = parent "s"

samp :: VirtualDOM -> VirtualDOM
samp = parent "samp"

script :: VirtualDOM -> VirtualDOM
script = parent "script"

section :: VirtualDOM -> VirtualDOM
section = parent "section"

select :: VirtualDOM -> VirtualDOM
select = parent "select"

small :: VirtualDOM -> VirtualDOM
small = parent "small"

source :: VirtualDOM
source = leaf "source"

span :: VirtualDOM -> VirtualDOM
span = parent "span"

strong :: VirtualDOM -> VirtualDOM
strong = parent "strong"

style :: VirtualDOM -> VirtualDOM
style = parent "style"

sub :: VirtualDOM -> VirtualDOM
sub = parent "sub"

summary :: VirtualDOM -> VirtualDOM
summary = parent "summary"

sup :: VirtualDOM -> VirtualDOM
sup = parent "sup"

table :: VirtualDOM -> VirtualDOM
table = parent "table"

tbody :: VirtualDOM -> VirtualDOM
tbody = parent "tbody"

td :: VirtualDOM -> VirtualDOM
td = parent "td"

textarea :: VirtualDOM -> VirtualDOM
textarea = parent "textarea"

tfoot :: VirtualDOM -> VirtualDOM
tfoot = parent "tfoot"

th :: VirtualDOM -> VirtualDOM
th = parent "th"

thead :: VirtualDOM -> VirtualDOM
thead = parent "thead"

time :: VirtualDOM -> VirtualDOM
time = parent "time"

title :: VirtualDOM -> VirtualDOM
title = parent "title"

tr :: VirtualDOM -> VirtualDOM
tr = parent "tr"

track :: VirtualDOM
track = leaf "track"

u :: VirtualDOM -> VirtualDOM
u = parent "u"

ul :: VirtualDOM -> VirtualDOM
ul = parent "ul"

var :: VirtualDOM -> VirtualDOM
var = parent "var"

video :: VirtualDOM -> VirtualDOM
video = parent "video"

wbr :: VirtualDOM
wbr = leaf "body"

circle :: VirtualDOM
circle = leaf "circle"

clipPath :: VirtualDOM -> VirtualDOM
clipPath = parent "clipPath"

defs :: VirtualDOM -> VirtualDOM
defs = parent "defs"

ellipse :: VirtualDOM
ellipse = leaf "ellipse"

g :: VirtualDOM -> VirtualDOM
g = parent "g"

image :: VirtualDOM
image = leaf "image"

line :: VirtualDOM
line = leaf "line"

linearGradient :: VirtualDOM -> VirtualDOM
linearGradient = parent "linearGradient"

mask :: VirtualDOM -> VirtualDOM
mask = parent "mask"

path :: VirtualDOM
path = leaf "path"

pattern :: VirtualDOM -> VirtualDOM
pattern = parent "pattern"

polygon :: VirtualDOM
polygon = leaf "polygon"

polyline :: VirtualDOM
polyline = leaf "polyline"

radialGradient :: VirtualDOM -> VirtualDOM
radialGradient = parent "radialGradient"

rect :: VirtualDOM
rect = leaf "rect"

stop :: VirtualDOM
stop = leaf "stop"

svg :: VirtualDOM -> VirtualDOM
svg = parent "svg"

text' :: VirtualDOM -> VirtualDOM
text' = parent "text"

tspan :: VirtualDOM -> VirtualDOM
tspan = parent "tspan"
