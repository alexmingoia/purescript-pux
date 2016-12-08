## Module Pux.Html.Events

#### `EventDecoder`

``` purescript
type EventDecoder fx ev = Event -> Eff fx ev
```

#### `on`

``` purescript
on :: forall fx ev a. Fn3 String (EventDecoder fx ev) (ev -> a) (Attribute a)
```

#### `defaultDecoder`

``` purescript
defaultDecoder :: forall fx ev. EventDecoder fx ev
```

#### `ChangeEvent`

``` purescript
type ChangeEvent = { currentTarget :: { value :: String } }
```

#### `InputEvent`

``` purescript
type InputEvent = ChangeEvent
```

#### `FormEvent`

``` purescript
type FormEvent = ChangeEvent
```

#### `CompositionEvent`

``` purescript
type CompositionEvent = { data :: String }
```

#### `KeyboardEvent`

``` purescript
type KeyboardEvent = { altKey :: Boolean, ctrlKey :: Boolean, charCode :: Int, key :: String, keyCode :: Int, locale :: String, location :: Int, metaKey :: Boolean, repeat :: Boolean, shiftKey :: Boolean, which :: Int }
```

#### `MouseEvent`

``` purescript
type MouseEvent = { altKey :: Boolean, button :: Number, buttons :: Number, clientX :: Number, clientY :: Number, ctrlKey :: Boolean, metaKey :: Boolean, pageX :: Number, pageY :: Number, screenX :: Number, screenY :: Number, shiftKey :: Boolean }
```

#### `TouchEvent`

``` purescript
type TouchEvent = { altKey :: Boolean, ctrlKey :: Boolean, metaKey :: Boolean, shiftKey :: Boolean }
```

#### `WheelEvent`

``` purescript
type WheelEvent = { deltaMode :: Number, deltaX :: Number, deltaY :: Number, deltaZ :: Number }
```

#### `onCopy`

``` purescript
onCopy :: forall action. (Event -> action) -> Attribute action
```

#### `onCut`

``` purescript
onCut :: forall action. (Event -> action) -> Attribute action
```

#### `onPaste`

``` purescript
onPaste :: forall action. (Event -> action) -> Attribute action
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

#### `onKeyHandler`

``` purescript
onKeyHandler :: forall fx ev a. Fn3 String (EventDecoder fx ev) (ev -> a) (Attribute a)
```

#### `onFocus`

``` purescript
onFocus :: forall action. (Event -> action) -> Attribute action
```

#### `onBlur`

``` purescript
onBlur :: forall action. (Event -> action) -> Attribute action
```

#### `onChange`

``` purescript
onChange :: forall action. (ChangeEvent -> action) -> Attribute action
```

#### `onInput`

``` purescript
onInput :: forall action. (InputEvent -> action) -> Attribute action
```

#### `onSubmit`

``` purescript
onSubmit :: forall action. (Event -> action) -> Attribute action
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
onSelect :: forall action. (Event -> action) -> Attribute action
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
onScroll :: forall action. (Event -> action) -> Attribute action
```

#### `onWheel`

``` purescript
onWheel :: forall action. (WheelEvent -> action) -> Attribute action
```

#### `onAbort`

``` purescript
onAbort :: forall action. (Event -> action) -> Attribute action
```

#### `onCanPlay`

``` purescript
onCanPlay :: forall action. (Event -> action) -> Attribute action
```

#### `onCanPlayThrough`

``` purescript
onCanPlayThrough :: forall action. (Event -> action) -> Attribute action
```

#### `onDurationChange`

``` purescript
onDurationChange :: forall action. (Event -> action) -> Attribute action
```

#### `onEmptied`

``` purescript
onEmptied :: forall action. (Event -> action) -> Attribute action
```

#### `onEncrypted`

``` purescript
onEncrypted :: forall action. (Event -> action) -> Attribute action
```

#### `onEnded`

``` purescript
onEnded :: forall action. (Event -> action) -> Attribute action
```

#### `onError`

``` purescript
onError :: forall action. (Event -> action) -> Attribute action
```

#### `onLoad`

``` purescript
onLoad :: forall action. (Event -> action) -> Attribute action
```

#### `onLoadedData`

``` purescript
onLoadedData :: forall action. (Event -> action) -> Attribute action
```

#### `onLoadedMetadata`

``` purescript
onLoadedMetadata :: forall action. (Event -> action) -> Attribute action
```

#### `onLoadStart`

``` purescript
onLoadStart :: forall action. (Event -> action) -> Attribute action
```

#### `onPause`

``` purescript
onPause :: forall action. (Event -> action) -> Attribute action
```

#### `onPlay`

``` purescript
onPlay :: forall action. (Event -> action) -> Attribute action
```

#### `onPlaying`

``` purescript
onPlaying :: forall action. (Event -> action) -> Attribute action
```

#### `onProgress`

``` purescript
onProgress :: forall action. (Event -> action) -> Attribute action
```

#### `onRateChange`

``` purescript
onRateChange :: forall action. (Event -> action) -> Attribute action
```

#### `onSeeked`

``` purescript
onSeeked :: forall action. (Event -> action) -> Attribute action
```

#### `onSeeking`

``` purescript
onSeeking :: forall action. (Event -> action) -> Attribute action
```

#### `onStalled`

``` purescript
onStalled :: forall action. (Event -> action) -> Attribute action
```

#### `onSuspend`

``` purescript
onSuspend :: forall action. (Event -> action) -> Attribute action
```

#### `onTimeUpdate`

``` purescript
onTimeUpdate :: forall action. (Event -> action) -> Attribute action
```

#### `onVolumeChange`

``` purescript
onVolumeChange :: forall action. (Event -> action) -> Attribute action
```

#### `onWaiting`

``` purescript
onWaiting :: forall action. (Event -> action) -> Attribute action
```


