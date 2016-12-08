module Pux.Html.Events where

import Control.Monad.Eff (Eff)
import DOM.Event.Types (Event)
import Data.Function.Uncurried (Fn3, runFn3)
import Pux.Html (Attribute)

-- An `EventDecoder` is a function which maps a raw DOM event to the event
-- type consumed by an application. A default implementation is used by the
-- event attribute constructors such as `onClick`, `onInput`, etc.
--
-- The default implementation calls `.preventDefault()` on DOM events to prevent
-- UI changes that aren't described by the application.
type EventDecoder fx ev = Event -> Eff fx ev

-- Create an attribute that declares an event handler. This function can be used
-- to specify a custom event decoder when attaching event handlers.
foreign import on :: forall fx ev a. Fn3 String (EventDecoder fx ev) (ev -> a) (Attribute a)

-- Default event decoder used for event attribute constructors such as
-- `onClick`, `onInput`, etc.
foreign import defaultDecoder :: forall fx ev. EventDecoder fx ev

type ChangeEvent = { currentTarget :: { value :: String } }

type InputEvent = ChangeEvent
type FormEvent = ChangeEvent

type CompositionEvent = { data :: String }

type KeyboardEvent =
  { altKey   :: Boolean
  , ctrlKey  :: Boolean
  , charCode :: Int
  , key      :: String
  , keyCode  :: Int
  , locale   :: String
  , location :: Int
  , metaKey  :: Boolean
  , repeat   :: Boolean
  , shiftKey :: Boolean
  , which    :: Int
  }

type MouseEvent =
  { altKey :: Boolean
  , button :: Number
  , buttons :: Number
  , clientX :: Number
  , clientY :: Number
  , ctrlKey :: Boolean
  , metaKey :: Boolean
  , pageX :: Number
  , pageY :: Number
  , screenX :: Number
  , screenY :: Number
  , shiftKey :: Boolean
  }

type TouchEvent =
  { altKey :: Boolean
  , ctrlKey :: Boolean
  , metaKey :: Boolean
  , shiftKey :: Boolean
  }

type WheelEvent =
  { deltaMode :: Number
  , deltaX :: Number
  , deltaY :: Number
  , deltaZ :: Number
  }

onCopy :: forall action. (Event -> action) -> Attribute action
onCopy = runFn3 on "copy" (defaultDecoder)

onCut :: forall action. (Event -> action) -> Attribute action
onCut = runFn3 on "cut" (defaultDecoder)

onPaste :: forall action. (Event -> action) -> Attribute action
onPaste = runFn3 on "paste" (defaultDecoder)

onCompositionEnd :: forall action. (CompositionEvent -> action) -> Attribute action
onCompositionEnd = runFn3 on "compositionend" (defaultDecoder)

onCompositionStart :: forall action. (CompositionEvent -> action) -> Attribute action
onCompositionStart = runFn3 on "compositionstart" (defaultDecoder)

onCompositionUpdate :: forall action. (CompositionEvent -> action) -> Attribute action
onCompositionUpdate = runFn3 on "compositionupdate" (defaultDecoder)

onKeyDown :: forall action. (KeyboardEvent -> action) -> Attribute action
onKeyDown = runFn3 on "keydown" (defaultDecoder)

onKeyPress :: forall action. (KeyboardEvent -> action) -> Attribute action
onKeyPress = runFn3 on "keypress" (defaultDecoder)

onKeyUp :: forall action. (KeyboardEvent -> action) -> Attribute action
onKeyUp = runFn3 on "keyup" (defaultDecoder)

-- | Send action only if specified key is pressed (on key up)
onKey :: forall action. String -> (KeyboardEvent -> action) -> Attribute action
onKey keyName = runFn3 onKeyHandler keyName (defaultDecoder)

foreign import onKeyHandler :: forall fx ev a. Fn3 String (EventDecoder fx ev) (ev -> a) (Attribute a)

onFocus :: forall action. (Event -> action) -> Attribute action
onFocus = runFn3 on "focus" (defaultDecoder)

onBlur :: forall action. (Event -> action) -> Attribute action
onBlur = runFn3 on "blur" (defaultDecoder)

onChange :: forall action. (ChangeEvent -> action) -> Attribute action
onChange = runFn3 on "change" (defaultDecoder)

onInput :: forall action. (InputEvent -> action) -> Attribute action
onInput = runFn3 on "input" (defaultDecoder)

onSubmit :: forall action. (Event -> action) -> Attribute action
onSubmit = runFn3 on "submit" (defaultDecoder)

onClick :: forall action. (MouseEvent -> action) -> Attribute action
onClick = runFn3 on "click" (defaultDecoder)

onContextMenu :: forall action. (MouseEvent -> action) -> Attribute action
onContextMenu = runFn3 on "contextmenu" (defaultDecoder)

onDoubleClick :: forall action. (MouseEvent -> action) -> Attribute action
onDoubleClick = runFn3 on "doubleclick" (defaultDecoder)

onDrag :: forall action. (MouseEvent -> action) -> Attribute action
onDrag = runFn3 on "drag" (defaultDecoder)

onDragEnd :: forall action. (MouseEvent -> action) -> Attribute action
onDragEnd = runFn3 on "dragend" (defaultDecoder)

onDragEnter :: forall action. (MouseEvent -> action) -> Attribute action
onDragEnter = runFn3 on "dragenter" (defaultDecoder)

onDragExit :: forall action. (MouseEvent -> action) -> Attribute action
onDragExit = runFn3 on "dragexit" (defaultDecoder)

onDragLeave :: forall action. (MouseEvent -> action) -> Attribute action
onDragLeave = runFn3 on "dragleave" (defaultDecoder)

onDragOver :: forall action. (MouseEvent -> action) -> Attribute action
onDragOver = runFn3 on "dragover" (defaultDecoder)

onDragStart :: forall action. (MouseEvent -> action) -> Attribute action
onDragStart = runFn3 on "dragstart" (defaultDecoder)

onDrop :: forall action. (MouseEvent -> action) -> Attribute action
onDrop = runFn3 on "drop" (defaultDecoder)

onMouseDown :: forall action. (MouseEvent -> action) -> Attribute action
onMouseDown = runFn3 on "mousedown" (defaultDecoder)

onMouseEnter :: forall action. (MouseEvent -> action) -> Attribute action
onMouseEnter = runFn3 on "mouseenter" (defaultDecoder)

onMouseLeave :: forall action. (MouseEvent -> action) -> Attribute action
onMouseLeave = runFn3 on "mouseleave" (defaultDecoder)

onMouseMove :: forall action. (MouseEvent -> action) -> Attribute action
onMouseMove = runFn3 on "mousemove" (defaultDecoder)

onMouseOut :: forall action. (MouseEvent -> action) -> Attribute action
onMouseOut = runFn3 on "mouseout" (defaultDecoder)

onMouseOver :: forall action. (MouseEvent -> action) -> Attribute action
onMouseOver = runFn3 on "mouseover" (defaultDecoder)

onMouseUp :: forall action. (MouseEvent -> action) -> Attribute action
onMouseUp = runFn3 on "mouseup" (defaultDecoder)

onSelect :: forall action. (Event -> action) -> Attribute action
onSelect = runFn3 on "select" (defaultDecoder)

onTouchCancel :: forall action. (TouchEvent -> action) -> Attribute action
onTouchCancel = runFn3 on "touchcancel" (defaultDecoder)

onTouchEnd :: forall action. (TouchEvent -> action) -> Attribute action
onTouchEnd = runFn3 on "touchend" (defaultDecoder)

onTouchMove :: forall action. (TouchEvent -> action) -> Attribute action
onTouchMove = runFn3 on "touchmove" (defaultDecoder)

onTouchStart :: forall action. (TouchEvent -> action) -> Attribute action
onTouchStart = runFn3 on "touchstart" (defaultDecoder)

onScroll :: forall action. (Event -> action) -> Attribute action
onScroll = runFn3 on "scroll" (defaultDecoder)

onWheel :: forall action. (WheelEvent -> action) -> Attribute action
onWheel = runFn3 on "wheel" (defaultDecoder)

onAbort :: forall action. (Event -> action) -> Attribute action
onAbort = runFn3 on "abort" (defaultDecoder)

onCanPlay :: forall action. (Event -> action) -> Attribute action
onCanPlay = runFn3 on "canplay" (defaultDecoder)

onCanPlayThrough :: forall action. (Event -> action) -> Attribute action
onCanPlayThrough = runFn3 on "canplaythrough" (defaultDecoder)

onDurationChange :: forall action. (Event -> action) -> Attribute action
onDurationChange = runFn3 on "durationchange" (defaultDecoder)

onEmptied :: forall action. (Event -> action) -> Attribute action
onEmptied = runFn3 on "emptied" (defaultDecoder)

onEncrypted :: forall action. (Event -> action) -> Attribute action
onEncrypted = runFn3 on "encrypted" (defaultDecoder)

onEnded :: forall action. (Event -> action) -> Attribute action
onEnded = runFn3 on "ended" (defaultDecoder)

onError :: forall action. (Event -> action) -> Attribute action
onError = runFn3 on "error" (defaultDecoder)

onLoad :: forall action. (Event -> action) -> Attribute action
onLoad = runFn3 on "load" (defaultDecoder)

onLoadedData :: forall action. (Event -> action) -> Attribute action
onLoadedData = runFn3 on "loadeddata" (defaultDecoder)

onLoadedMetadata :: forall action. (Event -> action) -> Attribute action
onLoadedMetadata = runFn3 on "loadedmetadata" (defaultDecoder)

onLoadStart :: forall action. (Event -> action) -> Attribute action
onLoadStart = runFn3 on "loadstart" (defaultDecoder)

onPause :: forall action. (Event -> action) -> Attribute action
onPause = runFn3 on "pause" (defaultDecoder)

onPlay :: forall action. (Event -> action) -> Attribute action
onPlay = runFn3 on "play" (defaultDecoder)

onPlaying :: forall action. (Event -> action) -> Attribute action
onPlaying = runFn3 on "playing" (defaultDecoder)

onProgress :: forall action. (Event -> action) -> Attribute action
onProgress = runFn3 on "progress" (defaultDecoder)

onRateChange :: forall action. (Event -> action) -> Attribute action
onRateChange = runFn3 on "ratechange" (defaultDecoder)

onSeeked :: forall action. (Event -> action) -> Attribute action
onSeeked = runFn3 on "seeked" (defaultDecoder)

onSeeking :: forall action. (Event -> action) -> Attribute action
onSeeking = runFn3 on "seeking" (defaultDecoder)

onStalled :: forall action. (Event -> action) -> Attribute action
onStalled = runFn3 on "stalled" (defaultDecoder)

onSuspend :: forall action. (Event -> action) -> Attribute action
onSuspend = runFn3 on "suspend" (defaultDecoder)

onTimeUpdate :: forall action. (Event -> action) -> Attribute action
onTimeUpdate = runFn3 on "timeupdate" (defaultDecoder)

onVolumeChange :: forall action. (Event -> action) -> Attribute action
onVolumeChange = runFn3 on "volumechange" (defaultDecoder)

onWaiting :: forall action. (Event -> action) -> Attribute action
onWaiting = runFn3 on "waiting" (defaultDecoder)
