## Module Pux.DOM.HTML.Attributes

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

#### `MouseEvent`

``` purescript
type MouseEvent = { pageX :: Number, pageY :: Number }
```

#### `KeyboardEvent`

``` purescript
type KeyboardEvent = { altKey :: Boolean, ctrlKey :: Boolean, charCode :: Int, key :: String, keyCode :: Int, locale :: String, location :: Int, metaKey :: Boolean, repeat :: Boolean, shiftKey :: Boolean, which :: Int }
```

#### `send`

``` purescript
send :: forall ev action eff. action -> Handler ev action eff
```

```purescript
button ! onClick (send Increment)
```

#### `preventDefault`

``` purescript
preventDefault :: forall ev action eff. Handler ev action (dom :: DOM | eff)
```

```purescript
form ! onSubmit preventDefault
```

#### `stopPropagation`

``` purescript
stopPropagation :: forall ev action eff. Handler ev action (dom :: DOM | eff)
```

```purescript
button ! onClick (send Increment <> stopPropagation)
```

#### `onCopy`

``` purescript
onCopy :: forall action eff. Handler Event action (dom :: DOM, chan :: Chan | eff) -> Attrs
```

#### `onCut`

``` purescript
onCut :: forall action eff. Handler Event action (dom :: DOM, chan :: Chan | eff) -> Attrs
```

#### `onPaste`

``` purescript
onPaste :: forall action eff. Handler Event action (dom :: DOM, chan :: Chan | eff) -> Attrs
```

#### `onKeyDown`

``` purescript
onKeyDown :: forall action eff. Handler KeyboardEvent (KeyboardEvent -> action) (dom :: DOM, chan :: Chan | eff) -> Attrs
```

#### `onKeyPress`

``` purescript
onKeyPress :: forall action eff. Handler KeyboardEvent (KeyboardEvent -> action) (dom :: DOM, chan :: Chan | eff) -> Attrs
```

#### `onKeyUp`

``` purescript
onKeyUp :: forall action eff. Handler KeyboardEvent (KeyboardEvent -> action) (dom :: DOM, chan :: Chan | eff) -> Attrs
```

#### `onFocus`

``` purescript
onFocus :: forall action eff. Handler KeyboardEvent (KeyboardEvent -> action) (dom :: DOM, chan :: Chan | eff) -> Attrs
```

#### `onBlur`

``` purescript
onBlur :: forall action eff. Handler KeyboardEvent (KeyboardEvent -> action) (dom :: DOM, chan :: Chan | eff) -> Attrs
```

#### `onChange`

``` purescript
onChange :: forall action eff. Handler KeyboardEvent (KeyboardEvent -> action) (dom :: DOM, chan :: Chan | eff) -> Attrs
```

#### `onInput`

``` purescript
onInput :: forall action eff. Handler KeyboardEvent (KeyboardEvent -> action) (dom :: DOM, chan :: Chan | eff) -> Attrs
```

#### `onSubmit`

``` purescript
onSubmit :: forall action eff. Handler KeyboardEvent (KeyboardEvent -> action) (dom :: DOM, chan :: Chan | eff) -> Attrs
```

#### `onClick`

``` purescript
onClick :: forall action eff. Handler Event action (dom :: DOM, chan :: Chan | eff) -> Attrs
```

#### `onDoubleClick`

``` purescript
onDoubleClick :: forall action eff. Handler Event action (dom :: DOM, chan :: Chan | eff) -> Attrs
```

#### `onDrag`

``` purescript
onDrag :: forall action eff. Handler Event action (dom :: DOM, chan :: Chan | eff) -> Attrs
```

#### `onDragEnd`

``` purescript
onDragEnd :: forall action eff. Handler Event action (dom :: DOM, chan :: Chan | eff) -> Attrs
```

#### `onDragEnter`

``` purescript
onDragEnter :: forall action eff. Handler Event action (dom :: DOM, chan :: Chan | eff) -> Attrs
```

#### `onDragExit`

``` purescript
onDragExit :: forall action eff. Handler Event action (dom :: DOM, chan :: Chan | eff) -> Attrs
```

#### `onDragLeave`

``` purescript
onDragLeave :: forall action eff. Handler Event action (dom :: DOM, chan :: Chan | eff) -> Attrs
```

#### `onDragOver`

``` purescript
onDragOver :: forall action eff. Handler Event action (dom :: DOM, chan :: Chan | eff) -> Attrs
```

#### `onDragStart`

``` purescript
onDragStart :: forall action eff. Handler Event action (dom :: DOM, chan :: Chan | eff) -> Attrs
```

#### `onDrop`

``` purescript
onDrop :: forall action eff. Handler Event action (dom :: DOM, chan :: Chan | eff) -> Attrs
```

#### `onMouseDown`

``` purescript
onMouseDown :: forall action eff. Handler MouseEvent (MouseEvent -> action) (dom :: DOM, chan :: Chan | eff) -> Attrs
```

#### `onMouseEnter`

``` purescript
onMouseEnter :: forall action eff. Handler MouseEvent (MouseEvent -> action) (dom :: DOM, chan :: Chan | eff) -> Attrs
```

#### `onMouseLeave`

``` purescript
onMouseLeave :: forall action eff. Handler MouseEvent (MouseEvent -> action) (dom :: DOM, chan :: Chan | eff) -> Attrs
```

#### `onMouseMove`

``` purescript
onMouseMove :: forall action eff. Handler MouseEvent (MouseEvent -> action) (dom :: DOM, chan :: Chan | eff) -> Attrs
```

#### `onMouseOut`

``` purescript
onMouseOut :: forall action eff. Handler MouseEvent (MouseEvent -> action) (dom :: DOM, chan :: Chan | eff) -> Attrs
```

#### `onMouseOver`

``` purescript
onMouseOver :: forall action eff. Handler MouseEvent (MouseEvent -> action) (dom :: DOM, chan :: Chan | eff) -> Attrs
```

#### `onMouseUp`

``` purescript
onMouseUp :: forall action eff. Handler MouseEvent (MouseEvent -> action) (dom :: DOM, chan :: Chan | eff) -> Attrs
```

#### `onTouchCancel`

``` purescript
onTouchCancel :: forall action eff. Handler Event action (dom :: DOM, chan :: Chan | eff) -> Attrs
```

#### `onTouchEnd`

``` purescript
onTouchEnd :: forall action eff. Handler Event action (dom :: DOM, chan :: Chan | eff) -> Attrs
```

#### `onTouchMove`

``` purescript
onTouchMove :: forall action eff. Handler Event action (dom :: DOM, chan :: Chan | eff) -> Attrs
```

#### `onTouchStart`

``` purescript
onTouchStart :: forall action eff. Handler Event action (dom :: DOM, chan :: Chan | eff) -> Attrs
```

#### `onScroll`

``` purescript
onScroll :: forall action eff. Handler Event action (dom :: DOM, chan :: Chan | eff) -> Attrs
```

#### `onWheel`

``` purescript
onWheel :: forall action eff. Handler Event action (dom :: DOM, chan :: Chan | eff) -> Attrs
```

#### `aria`

``` purescript
aria :: forall ariaAttrs. {  | ariaAttrs } -> Attrs
```

#### `data_`

``` purescript
data_ :: forall dataAttrs. {  | dataAttrs } -> Attrs
```

#### `style`

``` purescript
style :: forall style. {  | style } -> Attrs
```

#### `dangerouslySetInnerHTML`

``` purescript
dangerouslySetInnerHTML :: { __html :: String } -> Attrs
```

#### `accept`

``` purescript
accept :: String -> Attrs
```

#### `acceptCharset`

``` purescript
acceptCharset :: String -> Attrs
```

#### `accessKey`

``` purescript
accessKey :: String -> Attrs
```

#### `action`

``` purescript
action :: String -> Attrs
```

#### `allowFullScreen`

``` purescript
allowFullScreen :: String -> Attrs
```

#### `allowTransparency`

``` purescript
allowTransparency :: String -> Attrs
```

#### `alt`

``` purescript
alt :: String -> Attrs
```

#### `async`

``` purescript
async :: String -> Attrs
```

#### `autoComplete`

``` purescript
autoComplete :: String -> Attrs
```

#### `autoFocus`

``` purescript
autoFocus :: Boolean -> Attrs
```

#### `autoPlay`

``` purescript
autoPlay :: String -> Attrs
```

#### `cellPadding`

``` purescript
cellPadding :: String -> Attrs
```

#### `cellSpacing`

``` purescript
cellSpacing :: String -> Attrs
```

#### `charSet`

``` purescript
charSet :: String -> Attrs
```

#### `checked`

``` purescript
checked :: String -> Attrs
```

#### `classID`

``` purescript
classID :: String -> Attrs
```

#### `className`

``` purescript
className :: String -> Attrs
```

#### `cols`

``` purescript
cols :: String -> Attrs
```

#### `colSpan`

``` purescript
colSpan :: String -> Attrs
```

#### `content`

``` purescript
content :: String -> Attrs
```

#### `contentEditable`

``` purescript
contentEditable :: String -> Attrs
```

#### `contextMenu`

``` purescript
contextMenu :: String -> Attrs
```

#### `controls`

``` purescript
controls :: String -> Attrs
```

#### `coords`

``` purescript
coords :: String -> Attrs
```

#### `crossOrigin`

``` purescript
crossOrigin :: String -> Attrs
```

#### `dateTime`

``` purescript
dateTime :: String -> Attrs
```

#### `defer`

``` purescript
defer :: String -> Attrs
```

#### `dir`

``` purescript
dir :: String -> Attrs
```

#### `disabled`

``` purescript
disabled :: Boolean -> Attrs
```

#### `download`

``` purescript
download :: String -> Attrs
```

#### `draggable`

``` purescript
draggable :: String -> Attrs
```

#### `encType`

``` purescript
encType :: String -> Attrs
```

#### `form`

``` purescript
form :: String -> Attrs
```

#### `formAction`

``` purescript
formAction :: String -> Attrs
```

#### `formEncType`

``` purescript
formEncType :: String -> Attrs
```

#### `formMethod`

``` purescript
formMethod :: String -> Attrs
```

#### `formNoValidate`

``` purescript
formNoValidate :: String -> Attrs
```

#### `formTarget`

``` purescript
formTarget :: String -> Attrs
```

#### `frameBorder`

``` purescript
frameBorder :: String -> Attrs
```

#### `height`

``` purescript
height :: String -> Attrs
```

#### `hidden`

``` purescript
hidden :: String -> Attrs
```

#### `href`

``` purescript
href :: String -> Attrs
```

#### `hrefLang`

``` purescript
hrefLang :: String -> Attrs
```

#### `htmlFor`

``` purescript
htmlFor :: String -> Attrs
```

#### `httpEquiv`

``` purescript
httpEquiv :: String -> Attrs
```

#### `icon`

``` purescript
icon :: String -> Attrs
```

#### `id_`

``` purescript
id_ :: String -> Attrs
```

#### `key`

``` purescript
key :: String -> Attrs
```

#### `label`

``` purescript
label :: String -> Attrs
```

#### `lang`

``` purescript
lang :: String -> Attrs
```

#### `list`

``` purescript
list :: String -> Attrs
```

#### `loop`

``` purescript
loop :: String -> Attrs
```

#### `manifest`

``` purescript
manifest :: String -> Attrs
```

#### `marginHeight`

``` purescript
marginHeight :: String -> Attrs
```

#### `marginWidth`

``` purescript
marginWidth :: String -> Attrs
```

#### `max`

``` purescript
max :: String -> Attrs
```

#### `maxLength`

``` purescript
maxLength :: String -> Attrs
```

#### `media`

``` purescript
media :: String -> Attrs
```

#### `mediaGroup`

``` purescript
mediaGroup :: String -> Attrs
```

#### `method`

``` purescript
method :: String -> Attrs
```

#### `min`

``` purescript
min :: String -> Attrs
```

#### `multiple`

``` purescript
multiple :: String -> Attrs
```

#### `muted`

``` purescript
muted :: String -> Attrs
```

#### `name`

``` purescript
name :: String -> Attrs
```

#### `noValidate`

``` purescript
noValidate :: String -> Attrs
```

#### `open`

``` purescript
open :: String -> Attrs
```

#### `pattern`

``` purescript
pattern :: String -> Attrs
```

#### `placeholder`

``` purescript
placeholder :: String -> Attrs
```

#### `poster`

``` purescript
poster :: String -> Attrs
```

#### `preload`

``` purescript
preload :: String -> Attrs
```

#### `radioGroup`

``` purescript
radioGroup :: String -> Attrs
```

#### `readOnly`

``` purescript
readOnly :: String -> Attrs
```

#### `rel`

``` purescript
rel :: String -> Attrs
```

#### `required`

``` purescript
required :: String -> Attrs
```

#### `role`

``` purescript
role :: String -> Attrs
```

#### `rows`

``` purescript
rows :: String -> Attrs
```

#### `rowSpan`

``` purescript
rowSpan :: String -> Attrs
```

#### `sandbox`

``` purescript
sandbox :: String -> Attrs
```

#### `scope`

``` purescript
scope :: String -> Attrs
```

#### `scrolling`

``` purescript
scrolling :: String -> Attrs
```

#### `seamless`

``` purescript
seamless :: String -> Attrs
```

#### `selected`

``` purescript
selected :: String -> Attrs
```

#### `shape`

``` purescript
shape :: String -> Attrs
```

#### `size`

``` purescript
size :: String -> Attrs
```

#### `sizes`

``` purescript
sizes :: String -> Attrs
```

#### `span_`

``` purescript
span_ :: String -> Attrs
```

#### `spellCheck`

``` purescript
spellCheck :: String -> Attrs
```

#### `src`

``` purescript
src :: String -> Attrs
```

#### `srcDoc`

``` purescript
srcDoc :: String -> Attrs
```

#### `srcSet`

``` purescript
srcSet :: String -> Attrs
```

#### `start`

``` purescript
start :: String -> Attrs
```

#### `step`

``` purescript
step :: String -> Attrs
```

#### `tabIndex`

``` purescript
tabIndex :: String -> Attrs
```

#### `target`

``` purescript
target :: String -> Attrs
```

#### `title`

``` purescript
title :: String -> Attrs
```

#### `type_`

``` purescript
type_ :: String -> Attrs
```

#### `useMap`

``` purescript
useMap :: String -> Attrs
```

#### `value`

``` purescript
value :: String -> Attrs
```

#### `width`

``` purescript
width :: String -> Attrs
```

#### `wmode`

``` purescript
wmode :: String -> Attrs
```


