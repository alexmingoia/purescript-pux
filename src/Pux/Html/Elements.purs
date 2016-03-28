module Pux.Html.Elements where

import Data.Function (Fn3, runFn3)

foreign import data Attribute :: * -> *
foreign import data Html :: * -> *

foreign import text :: forall a. String -> Html a

a :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
a = runFn3 element "a"

abbr :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
abbr = runFn3 element "abbr"

address :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
address = runFn3 element "address"

area :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
area = runFn3 element "area"

article :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
article = runFn3 element "article"

aside :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
aside = runFn3 element "aside"

audio :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
audio = runFn3 element "audio"

b :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
b = runFn3 element "b"

base :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
base = runFn3 element "base"

bdi :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
bdi = runFn3 element "bdi"

bdo :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
bdo = runFn3 element "bdo"

big :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
big = runFn3 element "big"

blockquote :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
blockquote = runFn3 element "blockquote"

body :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
body = runFn3 element "body"

br :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
br = runFn3 element "br"

button :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
button = runFn3 element "button"

canvas :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
canvas = runFn3 element "canvas"

caption :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
caption = runFn3 element "caption"

cite :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
cite = runFn3 element "cite"

code :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
code = runFn3 element "code"

col :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
col = runFn3 element "col"

colgroup :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
colgroup = runFn3 element "colgroup"

data_ :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
data_ = runFn3 element "data"

datalist :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
datalist = runFn3 element "datalist"

dd :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
dd = runFn3 element "dd"

del :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
del = runFn3 element "del"

details :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
details = runFn3 element "details"

dfn :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
dfn = runFn3 element "dfn"

dialog :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
dialog = runFn3 element "dialog"

div :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
div = runFn3 element "div"

dl :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
dl = runFn3 element "dl"

dt :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
dt = runFn3 element "dt"

em :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
em = runFn3 element "em"

embed :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
embed = runFn3 element "embed"

fieldset :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
fieldset = runFn3 element "fieldset"

figcaption :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
figcaption = runFn3 element "figcaption"

figure :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
figure = runFn3 element "figure"

footer :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
footer = runFn3 element "footer"

form :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
form = runFn3 element "form"

h1 :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
h1 = runFn3 element "h1"

h2 :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
h2 = runFn3 element "h2"

h3 :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
h3 = runFn3 element "h3"

h4 :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
h4 = runFn3 element "h4"

h5 :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
h5 = runFn3 element "h5"

h6 :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
h6 = runFn3 element "h6"

head :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
head = runFn3 element "head"

header :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
header = runFn3 element "header"

hr :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
hr = runFn3 element "hr"

html :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
html = runFn3 element "html"

i :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
i = runFn3 element "i"

iframe :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
iframe = runFn3 element "iframe"

img :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
img = runFn3 element "img"

input :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
input = runFn3 element "input"

ins :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
ins = runFn3 element "ins"

kbd :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
kbd = runFn3 element "kbd"

keygen :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
keygen = runFn3 element "keygen"

label :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
label = runFn3 element "label"

legend :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
legend = runFn3 element "legend"

li :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
li = runFn3 element "li"

link :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
link = runFn3 element "link"

main :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
main = runFn3 element "main"

map :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
map = runFn3 element "map"

mark :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
mark = runFn3 element "mark"

menu :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
menu = runFn3 element "menu"

menuitem :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
menuitem = runFn3 element "menuitem"

meta :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
meta = runFn3 element "meta"

meter :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
meter = runFn3 element "meter"

nav :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
nav = runFn3 element "nav"

noscript :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
noscript = runFn3 element "noscript"

object :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
object = runFn3 element "object"

ol :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
ol = runFn3 element "ol"

optgroup :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
optgroup = runFn3 element "optgroup"

option :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
option = runFn3 element "option"

output :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
output = runFn3 element "output"

p :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
p = runFn3 element "p"

param :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
param = runFn3 element "param"

picture :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
picture = runFn3 element "picture"

pre :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
pre = runFn3 element "pre"

progress :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
progress = runFn3 element "progress"

q :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
q = runFn3 element "q"

rp :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
rp = runFn3 element "rp"

rt :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
rt = runFn3 element "rt"

ruby :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
ruby = runFn3 element "ruby"

s :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
s = runFn3 element "s"

samp :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
samp = runFn3 element "samp"

script :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
script = runFn3 element "script"

section :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
section = runFn3 element "section"

select :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
select = runFn3 element "select"

small :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
small = runFn3 element "small"

source :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
source = runFn3 element "source"

span :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
span = runFn3 element "span"

strong :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
strong = runFn3 element "strong"

style :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
style = runFn3 element "style"

sub :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
sub = runFn3 element "sub"

summary :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
summary = runFn3 element "summary"

sup :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
sup = runFn3 element "sup"

table :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
table = runFn3 element "table"

tbody :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
tbody = runFn3 element "tbody"

td :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
td = runFn3 element "td"

textarea :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
textarea = runFn3 element "textarea"

tfoot :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
tfoot = runFn3 element "tfoot"

th :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
th = runFn3 element "th"

thead :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
thead = runFn3 element "thead"

time :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
time = runFn3 element "time"

title :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
title = runFn3 element "title"

tr :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
tr = runFn3 element "tr"

track :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
track = runFn3 element "track"

u :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
u = runFn3 element "u"

ul :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
ul = runFn3 element "ul"

var :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
var = runFn3 element "var"

video :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
video = runFn3 element "video"

wbr :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
wbr = runFn3 element "body"

clipPath :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
clipPath = runFn3 element "clipPath"

defs :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
defs = runFn3 element "defs"

ellipse :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
ellipse = runFn3 element "ellipse"

g :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
g = runFn3 element "g"

image :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
image = runFn3 element "image"

line :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
line = runFn3 element "line"

linearGradient :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
linearGradient = runFn3 element "linearGradient"

mask :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
mask = runFn3 element "mask"

path :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
path = runFn3 element "path"

pattern :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
pattern = runFn3 element "pattern"

polygon :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
polygon = runFn3 element "polygon"

polyline :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
polyline = runFn3 element "polyline"

radialGradient :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
radialGradient = runFn3 element "radialGradient"

rect :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
rect = runFn3 element "rect"

stop :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
stop = runFn3 element "stop"

svg :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
svg = runFn3 element "svg"

text' :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
text' = runFn3 element "text"

tspan :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
tspan = runFn3 element "tspan"

foreign import element :: forall a.
                          Fn3 String (Array (Attribute a)) (Array (Html a)) (Html a)
