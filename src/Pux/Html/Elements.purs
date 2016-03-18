module Pux.Html.Elements where

import Data.Function (Fn3, runFn3)

foreign import data Attribute :: * -> *
foreign import data Html :: * -> *

foreign import text :: forall a. String -> Html a

a :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
a = parent "a"

abbr :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
abbr = parent "abbr"

address :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
address = parent "address"

area :: forall a. Array (Attribute a) -> Html a
area = leaf "area"

article :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
article = parent "article"

aside :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
aside = parent "aside"

audio :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
audio = parent "audio"

b :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
b = parent "b"

base :: forall a. Array (Attribute a) -> Html a
base = leaf "base"

bdi :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
bdi = parent "bdi"

bdo :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
bdo = parent "bdo"

big :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
big = parent "big"

blockquote :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
blockquote = parent "blockquote"

body :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
body = parent "body"

br :: forall a. Array (Attribute a) -> Html a
br = leaf "br"

button :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
button = parent "button"

canvas :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
canvas = parent "canvas"

caption :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
caption = parent "caption"

cite :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
cite = parent "cite"

code :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
code = parent "code"

col :: forall a. Array (Attribute a) -> Html a
col = leaf "col"

colgroup :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
colgroup = parent "colgroup"

data_ :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
data_ = parent "data"

datalist :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
datalist = parent "datalist"

dd :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
dd = parent "dd"

del :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
del = parent "del"

details :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
details = parent "details"

dfn :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
dfn = parent "dfn"

dialog :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
dialog = parent "dialog"

div :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
div = parent "div"

dl :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
dl = parent "dl"

dt :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
dt = parent "dt"

em :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
em = parent "em"

embed :: forall a. Array (Attribute a) -> Html a
embed = leaf "embed"

fieldset :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
fieldset = parent "fieldset"

figcaption :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
figcaption = parent "figcaption"

figure :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
figure = parent "figure"

footer :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
footer = parent "footer"

form :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
form = parent "form"

h1 :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
h1 = parent "h1"

h2 :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
h2 = parent "h2"

h3 :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
h3 = parent "h3"

h4 :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
h4 = parent "h4"

h5 :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
h5 = parent "h5"

h6 :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
h6 = parent "h6"

head :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
head = parent "head"

header :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
header = parent "header"

hr :: forall a. Array (Attribute a) -> Html a
hr = leaf "hr"

html :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
html = parent "html"

i :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
i = parent "i"

iframe :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
iframe = parent "iframe"

img :: forall a. Array (Attribute a) -> Html a
img = leaf "img"

input :: forall a. Array (Attribute a) -> Html a
input = leaf "input"

ins :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
ins = parent "ins"

kbd :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
kbd = parent "kbd"

keygen :: forall a. Array (Attribute a) -> Html a
keygen = leaf "keygen"

label :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
label = parent "label"

legend :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
legend = parent "legend"

li :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
li = parent "li"

link :: forall a. Array (Attribute a) -> Html a
link = leaf "link"

main :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
main = parent "main"

map :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
map = parent "map"

mark :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
mark = parent "mark"

menu :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
menu = parent "menu"

menuitem :: forall a. Array (Attribute a) -> Html a
menuitem = leaf "menuitem"

meta :: forall a. Array (Attribute a) -> Html a
meta = leaf "meta"

meter :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
meter = parent "meter"

nav :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
nav = parent "nav"

noscript :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
noscript = parent "noscript"

object :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
object = parent "object"

ol :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
ol = parent "ol"

optgroup :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
optgroup = parent "optgroup"

option :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
option = parent "option"

output :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
output = parent "output"

p :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
p = parent "p"

param :: forall a. Array (Attribute a) -> Html a
param = leaf "param"

picture :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
picture = parent "picture"

pre :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
pre = parent "pre"

progress :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
progress = parent "progress"

q :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
q = parent "q"

rp :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
rp = parent "rp"

rt :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
rt = parent "rt"

ruby :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
ruby = parent "ruby"

s :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
s = parent "s"

samp :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
samp = parent "samp"

script :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
script = parent "script"

section :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
section = parent "section"

select :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
select = parent "select"

small :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
small = parent "small"

source :: forall a. Array (Attribute a) -> Html a
source = leaf "source"

span :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
span = parent "span"

strong :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
strong = parent "strong"

style :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
style = parent "style"

sub :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
sub = parent "sub"

summary :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
summary = parent "summary"

sup :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
sup = parent "sup"

table :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
table = parent "table"

tbody :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
tbody = parent "tbody"

td :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
td = parent "td"

textarea :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
textarea = parent "textarea"

tfoot :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
tfoot = parent "tfoot"

th :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
th = parent "th"

thead :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
thead = parent "thead"

time :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
time = parent "time"

title :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
title = parent "title"

tr :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
tr = parent "tr"

track :: forall a. Array (Attribute a) -> Html a
track = leaf "track"

u :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
u = parent "u"

ul :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
ul = parent "ul"

var :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
var = parent "var"

video :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
video = parent "video"

wbr :: forall a. Array (Attribute a) -> Html a
wbr = leaf "body"

clipPath :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
clipPath = parent "clipPath"

defs :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
defs = parent "defs"

ellipse :: forall a. Array (Attribute a) -> Html a
ellipse = leaf "ellipse"

g :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
g = parent "g"

image :: forall a. Array (Attribute a) -> Html a
image = leaf "image"

line :: forall a. Array (Attribute a) -> Html a
line = leaf "line"

linearGradient :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
linearGradient = parent "linearGradient"

mask :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
mask = parent "mask"

path :: forall a. Array (Attribute a) -> Html a
path = leaf "path"

pattern :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
pattern = parent "pattern"

polygon :: forall a. Array (Attribute a) -> Html a
polygon = leaf "polygon"

polyline :: forall a. Array (Attribute a) -> Html a
polyline = leaf "polyline"

radialGradient :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
radialGradient = parent "radialGradient"

rect :: forall a. Array (Attribute a) -> Html a
rect = leaf "rect"

stop :: forall a. Array (Attribute a) -> Html a
stop = leaf "stop"

svg :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
svg = parent "svg"

text' :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
text' = parent "text"

tspan :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
tspan = parent "tspan"

parent :: forall a.
          String -> Array (Attribute a) -> Array (Html a) -> Html a
parent key attrs children = runFn3 element key attrs children

leaf :: forall a. String -> Array (Attribute a) -> Html a
leaf key attrs = runFn3 element key attrs []

foreign import element :: forall a.
                          Fn3 String (Array (Attribute a)) (Array (Html a)) (Html a)
