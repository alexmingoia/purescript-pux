## Module Pux.DOM.Events

#### `DOMEvent`

``` purescript
type DOMEvent = Event
```

Synonym for
[DOM.Event.Types.Event](https://pursuit.purescript.org/packages/purescript-dom/4.3.1/docs/DOM.Event.Types#t:Event)
to distinguish from application events.

#### `targetValue`

``` purescript
targetValue :: DOMEvent -> String
```

Return `event.target.value` if it exists, or an empty string if not.

#### `mapEventHandler`

``` purescript
mapEventHandler :: forall a b. (a -> b) -> EventHandler (DOMEvent -> a) -> EventHandler (DOMEvent -> b)
```

Map event handler that returns event type `a` to event handler that returns
event type `b`.

#### `onCopy`

``` purescript
onCopy :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onCut`

``` purescript
onCut :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onPaste`

``` purescript
onPaste :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onCompositionEnd`

``` purescript
onCompositionEnd :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onCompositionStart`

``` purescript
onCompositionStart :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onCompositionUpdate`

``` purescript
onCompositionUpdate :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onKeyDown`

``` purescript
onKeyDown :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onKeyPress`

``` purescript
onKeyPress :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onKeyUp`

``` purescript
onKeyUp :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onFocus`

``` purescript
onFocus :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onBlur`

``` purescript
onBlur :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onChange`

``` purescript
onChange :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onInput`

``` purescript
onInput :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onSubmit`

``` purescript
onSubmit :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onClick`

``` purescript
onClick :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onContextMenu`

``` purescript
onContextMenu :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onDoubleClick`

``` purescript
onDoubleClick :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onDrag`

``` purescript
onDrag :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onDragEnd`

``` purescript
onDragEnd :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onDragEnter`

``` purescript
onDragEnter :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onDragExit`

``` purescript
onDragExit :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onDragLeave`

``` purescript
onDragLeave :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onDragOver`

``` purescript
onDragOver :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onDragStart`

``` purescript
onDragStart :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onDrop`

``` purescript
onDrop :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onMouseDown`

``` purescript
onMouseDown :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onMouseEnter`

``` purescript
onMouseEnter :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onMouseLeave`

``` purescript
onMouseLeave :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onMouseMove`

``` purescript
onMouseMove :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onMouseOut`

``` purescript
onMouseOut :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onMouseOver`

``` purescript
onMouseOver :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onMouseUp`

``` purescript
onMouseUp :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onSelect`

``` purescript
onSelect :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onTouchCancel`

``` purescript
onTouchCancel :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onTouchEnd`

``` purescript
onTouchEnd :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onTouchMove`

``` purescript
onTouchMove :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onTouchStart`

``` purescript
onTouchStart :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onScroll`

``` purescript
onScroll :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onWheel`

``` purescript
onWheel :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onAbort`

``` purescript
onAbort :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onCanPlay`

``` purescript
onCanPlay :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onCanPlayThrough`

``` purescript
onCanPlayThrough :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onDurationChange`

``` purescript
onDurationChange :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onEmptied`

``` purescript
onEmptied :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onEncrypted`

``` purescript
onEncrypted :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onEnded`

``` purescript
onEnded :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onError`

``` purescript
onError :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onLoad`

``` purescript
onLoad :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onLoadedData`

``` purescript
onLoadedData :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onLoadedMetadata`

``` purescript
onLoadedMetadata :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onLoadStart`

``` purescript
onLoadStart :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onPause`

``` purescript
onPause :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onPlay`

``` purescript
onPlay :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onPlaying`

``` purescript
onPlaying :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onProgress`

``` purescript
onProgress :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onRateChange`

``` purescript
onRateChange :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onSeeked`

``` purescript
onSeeked :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onSeeking`

``` purescript
onSeeking :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onStalled`

``` purescript
onStalled :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onSuspend`

``` purescript
onSuspend :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onTimeUpdate`

``` purescript
onTimeUpdate :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onVolumeChange`

``` purescript
onVolumeChange :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```

#### `onWaiting`

``` purescript
onWaiting :: forall ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
```


