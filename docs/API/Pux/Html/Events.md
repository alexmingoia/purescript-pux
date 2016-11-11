## Module Pux.Html.Events

#### `ClipboardEvent`

``` purescript
type ClipboardEvent a b = { target :: a, currentTarget :: b }
```

#### `CompositionEvent`

``` purescript
type CompositionEvent a b = { target :: a, currentTarget :: b, data :: String }
```

#### `KeyboardEvent`

``` purescript
type KeyboardEvent a b = { target :: a, currentTarget :: b, altKey :: Boolean, ctrlKey :: Boolean, charCode :: Int, key :: String, keyCode :: Int, locale :: String, location :: Int, metaKey :: Boolean, repeat :: Boolean, shiftKey :: Boolean, which :: Int }
```

#### `FocusEvent`

``` purescript
type FocusEvent a b c = { target :: a, currentTarget :: b, relatedTarget :: c }
```

#### `FormEvent`

``` purescript
type FormEvent a b = { target :: a, currentTarget :: b }
```

#### `MouseEvent`

``` purescript
type MouseEvent a b = { target :: a, currentTarget :: b, altKey :: Boolean, button :: Number, buttons :: Number, clientX :: Number, clientY :: Number, ctrlKey :: Boolean, metaKey :: Boolean, pageX :: Number, pageY :: Number, screenX :: Number, screenY :: Number, shiftKey :: Boolean }
```

#### `SelectionEvent`

``` purescript
type SelectionEvent a b = { target :: a, currentTarget :: b }
```

#### `TouchEvent`

``` purescript
type TouchEvent a b = { target :: a, currentTarget :: b, altKey :: Boolean, ctrlKey :: Boolean, metaKey :: Boolean, shiftKey :: Boolean }
```

#### `UIEvent`

``` purescript
type UIEvent a b = { target :: a, currentTarget :: b, detail :: Number }
```

#### `WheelEvent`

``` purescript
type WheelEvent a b = { target :: a, currentTarget :: b, deltaMode :: Number, deltaX :: Number, deltaY :: Number, deltaZ :: Number }
```

#### `MediaEvent`

``` purescript
type MediaEvent a b = { target :: a, currentTarget :: b }
```

#### `onCopy`

``` purescript
onCopy :: forall action a b. (ClipboardEvent a b -> action) -> Attribute action
```

#### `onCut`

``` purescript
onCut :: forall action a b. (ClipboardEvent a b -> action) -> Attribute action
```

#### `onPaste`

``` purescript
onPaste :: forall action a b. (ClipboardEvent a b -> action) -> Attribute action
```

#### `onCompositionEnd`

``` purescript
onCompositionEnd :: forall action a b. (CompositionEvent a b -> action) -> Attribute action
```

#### `onCompositionStart`

``` purescript
onCompositionStart :: forall action a b. (CompositionEvent a b -> action) -> Attribute action
```

#### `onCompositionUpdate`

``` purescript
onCompositionUpdate :: forall action a b. (CompositionEvent a b -> action) -> Attribute action
```

#### `onKeyDown`

``` purescript
onKeyDown :: forall action a b. (KeyboardEvent a b -> action) -> Attribute action
```

#### `onKeyPress`

``` purescript
onKeyPress :: forall action a b. (KeyboardEvent a b -> action) -> Attribute action
```

#### `onKeyUp`

``` purescript
onKeyUp :: forall action a b. (KeyboardEvent a b -> action) -> Attribute action
```

#### `onKey`

``` purescript
onKey :: forall action a b. String -> (KeyboardEvent a b -> action) -> Attribute action
```

Send action only if specified key is pressed (on key up)

#### `onFocus`

``` purescript
onFocus :: forall action a b c. (FocusEvent a b c -> action) -> Attribute action
```

#### `onBlur`

``` purescript
onBlur :: forall action a b c. (FocusEvent a b c -> action) -> Attribute action
```

#### `onChange`

``` purescript
onChange :: forall action a b. (FormEvent a b -> action) -> Attribute action
```

#### `onInput`

``` purescript
onInput :: forall action a b. (FormEvent a b -> action) -> Attribute action
```

#### `onSubmit`

``` purescript
onSubmit :: forall action a b. (FormEvent a b -> action) -> Attribute action
```

#### `onClick`

``` purescript
onClick :: forall action a b. (MouseEvent a b -> action) -> Attribute action
```

#### `onContextMenu`

``` purescript
onContextMenu :: forall action a b. (MouseEvent a b -> action) -> Attribute action
```

#### `onDoubleClick`

``` purescript
onDoubleClick :: forall action a b. (MouseEvent a b -> action) -> Attribute action
```

#### `onDrag`

``` purescript
onDrag :: forall action a b. (MouseEvent a b -> action) -> Attribute action
```

#### `onDragEnd`

``` purescript
onDragEnd :: forall action a b. (MouseEvent a b -> action) -> Attribute action
```

#### `onDragEnter`

``` purescript
onDragEnter :: forall action a b. (MouseEvent a b -> action) -> Attribute action
```

#### `onDragExit`

``` purescript
onDragExit :: forall action a b. (MouseEvent a b -> action) -> Attribute action
```

#### `onDragLeave`

``` purescript
onDragLeave :: forall action a b. (MouseEvent a b -> action) -> Attribute action
```

#### `onDragOver`

``` purescript
onDragOver :: forall action a b. (MouseEvent a b -> action) -> Attribute action
```

#### `onDragStart`

``` purescript
onDragStart :: forall action a b. (MouseEvent a b -> action) -> Attribute action
```

#### `onDrop`

``` purescript
onDrop :: forall action a b. (MouseEvent a b -> action) -> Attribute action
```

#### `onMouseDown`

``` purescript
onMouseDown :: forall action a b. (MouseEvent a b -> action) -> Attribute action
```

#### `onMouseEnter`

``` purescript
onMouseEnter :: forall action a b. (MouseEvent a b -> action) -> Attribute action
```

#### `onMouseLeave`

``` purescript
onMouseLeave :: forall action a b. (MouseEvent a b -> action) -> Attribute action
```

#### `onMouseMove`

``` purescript
onMouseMove :: forall action a b. (MouseEvent a b -> action) -> Attribute action
```

#### `onMouseOut`

``` purescript
onMouseOut :: forall action a b. (MouseEvent a b -> action) -> Attribute action
```

#### `onMouseOver`

``` purescript
onMouseOver :: forall action a b. (MouseEvent a b -> action) -> Attribute action
```

#### `onMouseUp`

``` purescript
onMouseUp :: forall action a b. (MouseEvent a b -> action) -> Attribute action
```

#### `onSelect`

``` purescript
onSelect :: forall action a b. (SelectionEvent a b -> action) -> Attribute action
```

#### `onTouchCancel`

``` purescript
onTouchCancel :: forall action a b. (TouchEvent a b -> action) -> Attribute action
```

#### `onTouchEnd`

``` purescript
onTouchEnd :: forall action a b. (TouchEvent a b -> action) -> Attribute action
```

#### `onTouchMove`

``` purescript
onTouchMove :: forall action a b. (TouchEvent a b -> action) -> Attribute action
```

#### `onTouchStart`

``` purescript
onTouchStart :: forall action a b. (TouchEvent a b -> action) -> Attribute action
```

#### `onScroll`

``` purescript
onScroll :: forall action a b. (UIEvent a b -> action) -> Attribute action
```

#### `onWheel`

``` purescript
onWheel :: forall action a b. (WheelEvent a b -> action) -> Attribute action
```

#### `onAbort`

``` purescript
onAbort :: forall action a b. (MediaEvent a b -> action) -> Attribute action
```

#### `onCanPlay`

``` purescript
onCanPlay :: forall action a b. (MediaEvent a b -> action) -> Attribute action
```

#### `onCanPlayThrough`

``` purescript
onCanPlayThrough :: forall action a b. (MediaEvent a b -> action) -> Attribute action
```

#### `onDurationChange`

``` purescript
onDurationChange :: forall action a b. (MediaEvent a b -> action) -> Attribute action
```

#### `onEmptied`

``` purescript
onEmptied :: forall action a b. (MediaEvent a b -> action) -> Attribute action
```

#### `onEncrypted`

``` purescript
onEncrypted :: forall action a b. (MediaEvent a b -> action) -> Attribute action
```

#### `onEnded`

``` purescript
onEnded :: forall action a b. (MediaEvent a b -> action) -> Attribute action
```

#### `onError`

``` purescript
onError :: forall action a b. (MediaEvent a b -> action) -> Attribute action
```

#### `onLoad`

``` purescript
onLoad :: forall action a b. (MediaEvent a b -> action) -> Attribute action
```

#### `onLoadedData`

``` purescript
onLoadedData :: forall action a b. (MediaEvent a b -> action) -> Attribute action
```

#### `onLoadedMetadata`

``` purescript
onLoadedMetadata :: forall action a b. (MediaEvent a b -> action) -> Attribute action
```

#### `onLoadStart`

``` purescript
onLoadStart :: forall action a b. (MediaEvent a b -> action) -> Attribute action
```

#### `onPause`

``` purescript
onPause :: forall action a b. (MediaEvent a b -> action) -> Attribute action
```

#### `onPlay`

``` purescript
onPlay :: forall action a b. (MediaEvent a b -> action) -> Attribute action
```

#### `onPlaying`

``` purescript
onPlaying :: forall action a b. (MediaEvent a b -> action) -> Attribute action
```

#### `onProgress`

``` purescript
onProgress :: forall action a b. (MediaEvent a b -> action) -> Attribute action
```

#### `onRateChange`

``` purescript
onRateChange :: forall action a b. (MediaEvent a b -> action) -> Attribute action
```

#### `onSeeked`

``` purescript
onSeeked :: forall action a b. (MediaEvent a b -> action) -> Attribute action
```

#### `onSeeking`

``` purescript
onSeeking :: forall action a b. (MediaEvent a b -> action) -> Attribute action
```

#### `onStalled`

``` purescript
onStalled :: forall action a b. (MediaEvent a b -> action) -> Attribute action
```

#### `onSuspend`

``` purescript
onSuspend :: forall action a b. (MediaEvent a b -> action) -> Attribute action
```

#### `onTimeUpdate`

``` purescript
onTimeUpdate :: forall action a b. (MediaEvent a b -> action) -> Attribute action
```

#### `onVolumeChange`

``` purescript
onVolumeChange :: forall action a b. (MediaEvent a b -> action) -> Attribute action
```

#### `onWaiting`

``` purescript
onWaiting :: forall action a b. (MediaEvent a b -> action) -> Attribute action
```

#### `handler`

``` purescript
handler :: forall ev a. Fn2 String (ev -> a) (Attribute a)
```

#### `onKeyHandler`

``` purescript
onKeyHandler :: forall ev a. Fn2 String (ev -> a) (Attribute a)
```


