## Module Pux.Html.Events

#### `Target`

``` purescript
type Target = { value :: String, checked :: Boolean }
```

#### `ClipboardEvent`

``` purescript
type ClipboardEvent = { target :: Target, currentTarget :: Target }
```

#### `CompositionEvent`

``` purescript
type CompositionEvent = { target :: Target, currentTarget :: Target, data :: String }
```

#### `KeyboardEvent`

``` purescript
type KeyboardEvent = { target :: Target, currentTarget :: Target, altKey :: Boolean, ctrlKey :: Boolean, charCode :: Int, key :: String, keyCode :: Int, locale :: String, location :: Int, metaKey :: Boolean, repeat :: Boolean, shiftKey :: Boolean, which :: Int }
```

#### `FocusEvent`

``` purescript
type FocusEvent = { target :: Target, currentTarget :: Target, relatedTarget :: Target }
```

#### `FormEvent`

``` purescript
type FormEvent = { target :: Target, currentTarget :: Target }
```

#### `MouseEvent`

``` purescript
type MouseEvent = { target :: Target, currentTarget :: Target, altKey :: Boolean, button :: Number, buttons :: Number, clientX :: Number, clientY :: Number, ctrlKey :: Boolean, metaKey :: Boolean, pageX :: Number, pageY :: Number, screenX :: Number, screenY :: Number, shiftKey :: Boolean }
```

#### `SelectionEvent`

``` purescript
type SelectionEvent = { target :: Target, currentTarget :: Target }
```

#### `TouchEvent`

``` purescript
type TouchEvent = { target :: Target, currentTarget :: Target, altKey :: Boolean, ctrlKey :: Boolean, metaKey :: Boolean, shiftKey :: Boolean }
```

#### `UIEvent`

``` purescript
type UIEvent = { target :: Target, currentTarget :: Target, detail :: Number }
```

#### `WheelEvent`

``` purescript
type WheelEvent = { target :: Target, currentTarget :: Target, deltaMode :: Number, deltaX :: Number, deltaY :: Number, deltaZ :: Number }
```

#### `MediaEvent`

``` purescript
type MediaEvent = { target :: Target, currentTarget :: Target }
```

#### `onCopy`

``` purescript
onCopy :: forall action. (ClipboardEvent -> action) -> Attribute action
```

#### `onCut`

``` purescript
onCut :: forall action. (ClipboardEvent -> action) -> Attribute action
```

#### `onPaste`

``` purescript
onPaste :: forall action. (ClipboardEvent -> action) -> Attribute action
```

#### `onCompositionEnd`

``` purescript
onCompositionEnd :: forall action. (CompositionEvent -> action) -> Attribute action
```

#### `onCompositionStart`

``` purescript
onCompositionStart :: forall action. (CompositionEvent -> action) -> Attribute action
```

#### `onCompositionUpdate`

``` purescript
onCompositionUpdate :: forall action. (CompositionEvent -> action) -> Attribute action
```

#### `onKeyDown`

``` purescript
onKeyDown :: forall action. (KeyboardEvent -> action) -> Attribute action
```

#### `onKeyPress`

``` purescript
onKeyPress :: forall action. (KeyboardEvent -> action) -> Attribute action
```

#### `onKeyUp`

``` purescript
onKeyUp :: forall action. (KeyboardEvent -> action) -> Attribute action
```

#### `onKey`

``` purescript
onKey :: forall action. String -> (KeyboardEvent -> action) -> Attribute action
```

Send action only if specified key is pressed (on key up)

#### `onFocus`

``` purescript
onFocus :: forall action. (FocusEvent -> action) -> Attribute action
```

#### `onBlur`

``` purescript
onBlur :: forall action. (FocusEvent -> action) -> Attribute action
```

#### `onChange`

``` purescript
onChange :: forall action. (FormEvent -> action) -> Attribute action
```

#### `onInput`

``` purescript
onInput :: forall action. (FormEvent -> action) -> Attribute action
```

#### `onSubmit`

``` purescript
onSubmit :: forall action. (FormEvent -> action) -> Attribute action
```

#### `onClick`

``` purescript
onClick :: forall action. (MouseEvent -> action) -> Attribute action
```

#### `onContextMenu`

``` purescript
onContextMenu :: forall action. (MouseEvent -> action) -> Attribute action
```

#### `onDoubleClick`

``` purescript
onDoubleClick :: forall action. (MouseEvent -> action) -> Attribute action
```

#### `onDrag`

``` purescript
onDrag :: forall action. (MouseEvent -> action) -> Attribute action
```

#### `onDragEnd`

``` purescript
onDragEnd :: forall action. (MouseEvent -> action) -> Attribute action
```

#### `onDragEnter`

``` purescript
onDragEnter :: forall action. (MouseEvent -> action) -> Attribute action
```

#### `onDragExit`

``` purescript
onDragExit :: forall action. (MouseEvent -> action) -> Attribute action
```

#### `onDragLeave`

``` purescript
onDragLeave :: forall action. (MouseEvent -> action) -> Attribute action
```

#### `onDragOver`

``` purescript
onDragOver :: forall action. (MouseEvent -> action) -> Attribute action
```

#### `onDragStart`

``` purescript
onDragStart :: forall action. (MouseEvent -> action) -> Attribute action
```

#### `onDrop`

``` purescript
onDrop :: forall action. (MouseEvent -> action) -> Attribute action
```

#### `onMouseDown`

``` purescript
onMouseDown :: forall action. (MouseEvent -> action) -> Attribute action
```

#### `onMouseEnter`

``` purescript
onMouseEnter :: forall action. (MouseEvent -> action) -> Attribute action
```

#### `onMouseLeave`

``` purescript
onMouseLeave :: forall action. (MouseEvent -> action) -> Attribute action
```

#### `onMouseMove`

``` purescript
onMouseMove :: forall action. (MouseEvent -> action) -> Attribute action
```

#### `onMouseOut`

``` purescript
onMouseOut :: forall action. (MouseEvent -> action) -> Attribute action
```

#### `onMouseOver`

``` purescript
onMouseOver :: forall action. (MouseEvent -> action) -> Attribute action
```

#### `onMouseUp`

``` purescript
onMouseUp :: forall action. (MouseEvent -> action) -> Attribute action
```

#### `onSelect`

``` purescript
onSelect :: forall action. (SelectionEvent -> action) -> Attribute action
```

#### `onTouchCancel`

``` purescript
onTouchCancel :: forall action. (TouchEvent -> action) -> Attribute action
```

#### `onTouchEnd`

``` purescript
onTouchEnd :: forall action. (TouchEvent -> action) -> Attribute action
```

#### `onTouchMove`

``` purescript
onTouchMove :: forall action. (TouchEvent -> action) -> Attribute action
```

#### `onTouchStart`

``` purescript
onTouchStart :: forall action. (TouchEvent -> action) -> Attribute action
```

#### `onScroll`

``` purescript
onScroll :: forall action. (UIEvent -> action) -> Attribute action
```

#### `onWheel`

``` purescript
onWheel :: forall action. (WheelEvent -> action) -> Attribute action
```

#### `onAbort`

``` purescript
onAbort :: forall action. (MediaEvent -> action) -> Attribute action
```

#### `onCanPlay`

``` purescript
onCanPlay :: forall action. (MediaEvent -> action) -> Attribute action
```

#### `onCanPlayThrough`

``` purescript
onCanPlayThrough :: forall action. (MediaEvent -> action) -> Attribute action
```

#### `onDurationChange`

``` purescript
onDurationChange :: forall action. (MediaEvent -> action) -> Attribute action
```

#### `onEmptied`

``` purescript
onEmptied :: forall action. (MediaEvent -> action) -> Attribute action
```

#### `onEncrypted`

``` purescript
onEncrypted :: forall action. (MediaEvent -> action) -> Attribute action
```

#### `onEnded`

``` purescript
onEnded :: forall action. (MediaEvent -> action) -> Attribute action
```

#### `onError`

``` purescript
onError :: forall action. (MediaEvent -> action) -> Attribute action
```

#### `onLoad`

``` purescript
onLoad :: forall action. (MediaEvent -> action) -> Attribute action
```

#### `onLoadedData`

``` purescript
onLoadedData :: forall action. (MediaEvent -> action) -> Attribute action
```

#### `onLoadedMetadata`

``` purescript
onLoadedMetadata :: forall action. (MediaEvent -> action) -> Attribute action
```

#### `onLoadStart`

``` purescript
onLoadStart :: forall action. (MediaEvent -> action) -> Attribute action
```

#### `onPause`

``` purescript
onPause :: forall action. (MediaEvent -> action) -> Attribute action
```

#### `onPlay`

``` purescript
onPlay :: forall action. (MediaEvent -> action) -> Attribute action
```

#### `onPlaying`

``` purescript
onPlaying :: forall action. (MediaEvent -> action) -> Attribute action
```

#### `onProgress`

``` purescript
onProgress :: forall action. (MediaEvent -> action) -> Attribute action
```

#### `onRateChange`

``` purescript
onRateChange :: forall action. (MediaEvent -> action) -> Attribute action
```

#### `onSeeked`

``` purescript
onSeeked :: forall action. (MediaEvent -> action) -> Attribute action
```

#### `onSeeking`

``` purescript
onSeeking :: forall action. (MediaEvent -> action) -> Attribute action
```

#### `onStalled`

``` purescript
onStalled :: forall action. (MediaEvent -> action) -> Attribute action
```

#### `onSuspend`

``` purescript
onSuspend :: forall action. (MediaEvent -> action) -> Attribute action
```

#### `onTimeUpdate`

``` purescript
onTimeUpdate :: forall action. (MediaEvent -> action) -> Attribute action
```

#### `onVolumeChange`

``` purescript
onVolumeChange :: forall action. (MediaEvent -> action) -> Attribute action
```

#### `onWaiting`

``` purescript
onWaiting :: forall action. (MediaEvent -> action) -> Attribute action
```

#### `handler`

``` purescript
handler :: forall ev a. Fn2 String (ev -> a) (Attribute a)
```

#### `onKeyHandler`

``` purescript
onKeyHandler :: forall ev a. Fn2 String (ev -> a) (Attribute a)
```


