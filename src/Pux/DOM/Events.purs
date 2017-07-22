module Pux.DOM.Events where

import DOM.Event.Types (Event)
import Text.Smolder.Markup (EventHandlers, on)

-- | Synonym for
-- | [DOM.Event.Types.Event](https://pursuit.purescript.org/packages/purescript-dom/4.3.1/docs/DOM.Event.Types#t:Event)
-- | to distinguish from application events.
type DOMEvent = Event

-- | Return `event.target.value` if it exists, or an empty string if not.
foreign import targetValue :: DOMEvent -> String

onCopy :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onCopy = on "onCopy"

onCut :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onCut = on "onCut"

onPaste :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onPaste = on "onPaste"

onCompositionEnd :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onCompositionEnd = on "onCompositionEnd"

onCompositionStart :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onCompositionStart = on "onCompositionStart"

onCompositionUpdate :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onCompositionUpdate = on "onCompositionUpdate"

onKeyDown :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onKeyDown = on "onKeyDown"

onKeyPress :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onKeyPress = on "onKeyPress"

onKeyUp :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onKeyUp = on "onKeyUp"

onFocus :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onFocus = on "onFocus"

onBlur :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onBlur = on "onBlur"

onChange :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onChange = on "onChange"

onInput :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onInput = on "onInput"

onSubmit :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onSubmit = on "onSubmit"

onClick :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onClick = on "onClick"

onContextMenu :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onContextMenu = on "onContextMenu"

onDoubleClick :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onDoubleClick = on "onDoubleClick"

onDrag :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onDrag = on "onDrag"

onDragEnd :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onDragEnd = on "onDragEnd"

onDragEnter :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onDragEnter = on "onDragEnter"

onDragExit :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onDragExit = on "onDragExit"

onDragLeave :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onDragLeave = on "onDragLeave"

onDragOver :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onDragOver = on "onDragOver"

onDragStart :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onDragStart = on "onDragStart"

onDrop :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onDrop = on "onDrop"

onMouseDown :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onMouseDown = on "onMouseDown"

onMouseEnter :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onMouseEnter = on "onMouseEnter"

onMouseLeave :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onMouseLeave = on "onMouseLeave"

onMouseMove :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onMouseMove = on "onMouseMove"

onMouseOut :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onMouseOut = on "onMouseOut"

onMouseOver :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onMouseOver = on "onMouseOver"

onMouseUp :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onMouseUp = on "onMouseUp"

onSelect :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onSelect = on "onSelect"

onTouchCancel :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onTouchCancel = on "onTouchCancel"

onTouchEnd :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onTouchEnd = on "onTouchEnd"

onTouchMove :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onTouchMove = on "onTouchMove"

onTouchStart :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onTouchStart = on "onTouchStart"

onScroll :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onScroll = on "onScroll"

onWheel :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onWheel = on "onWheel"

onAbort :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onAbort = on "onAbort"

onCanPlay :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onCanPlay = on "onCanPlay"

onCanPlayThrough :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onCanPlayThrough = on "onCanPlayThrough"

onDurationChange :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onDurationChange = on "onDurationChange"

onEmptied :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onEmptied = on "onEmptied"

onEncrypted :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onEncrypted = on "onEncrypted"

onEnded :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onEnded = on "onEnded"

onError :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onError = on "onError"

onLoad :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onLoad = on "onLoad"

onLoadedData :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onLoadedData = on "onLoadedData"

onLoadedMetadata :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onLoadedMetadata = on "onLoadedMetadata"

onLoadStart :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onLoadStart = on "onLoadStart"

onPause :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onPause = on "onPause"

onPlay :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onPlay = on "onPlay"

onPlaying :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onPlaying = on "onPlaying"

onProgress :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onProgress = on "onProgress"

onRateChange :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onRateChange = on "onRateChange"

onSeeked :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onSeeked = on "onSeeked"

onSeeking :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onSeeking = on "onSeeking"

onStalled :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onStalled = on "onStalled"

onSuspend :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onSuspend = on "onSuspend"

onTimeUpdate :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onTimeUpdate = on "onTimeUpdate"

onVolumeChange :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onVolumeChange = on "onVolumeChange"

onWaiting :: ∀ ev. (DOMEvent -> ev) -> EventHandlers (DOMEvent -> ev)
onWaiting = on "onWaiting"
