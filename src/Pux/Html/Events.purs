module Pux.Html.Events where

import Data.Function (Fn2, runFn2)
import Pux.Html (Attribute)

type Target = { value :: String }

type ClipboardEvent =
  { target :: Target
  , currentTarget :: Target
  }

type CompositionEvent =
  { target :: Target
  , currentTarget :: Target
  , data :: String
  }

type KeyboardEvent =
  { target :: Target
  , currentTarget :: Target
  , altKey   :: Boolean
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

type FocusEvent =
  { target :: Target
  , currentTarget :: Target
  , relatedTarget :: Target
  }

type FormEvent =
  { target :: Target
  , currentTarget :: Target
  }

type MouseEvent =
  { target :: Target
  , currentTarget :: Target
  , altKey :: Boolean
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

type SelectionEvent =
  { target :: Target
  , currentTarget :: Target
}

type TouchEvent =
  { target :: Target
  , currentTarget :: Target
  , altKey :: Boolean
  , ctrlKey :: Boolean
  , metaKey :: Boolean
  , shiftKey :: Boolean
  }

type UIEvent =
  { target :: Target
  , currentTarget :: Target
  , detail :: Number
  }

type WheelEvent =
  { target :: Target
  , currentTarget :: Target
  , deltaMode :: Number
  , deltaX :: Number
  , deltaY :: Number
  , deltaZ :: Number
  }

type MediaEvent =
  { target :: Target
  , currentTarget :: Target
  }

onCopy :: forall action. (ClipboardEvent -> action) -> Attribute action
onCopy = runFn2 handler "onCopy"

onCut :: forall action. (ClipboardEvent -> action) -> Attribute action
onCut = runFn2 handler "onCut"

onPaste :: forall action. (ClipboardEvent -> action) -> Attribute action
onPaste = runFn2 handler "onPaste"

onCompositionEnd :: forall action. (CompositionEvent -> action) -> Attribute action
onCompositionEnd = runFn2 handler "onCompositionEnd"

onCompositionStart :: forall action. (CompositionEvent -> action) -> Attribute action
onCompositionStart = runFn2 handler "onCompositionStart"

onCompositionUpdate :: forall action. (CompositionEvent -> action) -> Attribute action
onCompositionUpdate = runFn2 handler "onCompositionUpdate"

onKeyDown :: forall action. (KeyboardEvent -> action) -> Attribute action
onKeyDown = runFn2 handler "onKeyDown"

onKeyPress :: forall action. (KeyboardEvent -> action) -> Attribute action
onKeyPress = runFn2 handler "onKeyPress"

onKeyUp :: forall action. (KeyboardEvent -> action) -> Attribute action
onKeyUp = runFn2 handler "onKeyUp"

onFocus :: forall action. (FocusEvent -> action) -> Attribute action
onFocus = runFn2 handler "onFocus"

onBlur :: forall action. (FocusEvent -> action) -> Attribute action
onBlur = runFn2 handler "onBlur"

onChange :: forall action. (FormEvent -> action) -> Attribute action
onChange = runFn2 handler "onChange"

onInput :: forall action. (FormEvent -> action) -> Attribute action
onInput = runFn2 handler "onInput"

onSubmit :: forall action. (FormEvent -> action) -> Attribute action
onSubmit = runFn2 handler "onSubmit"

onClick :: forall action. (MouseEvent -> action) -> Attribute action
onClick = runFn2 handler "onClick"

onContextMenu :: forall action. (MouseEvent -> action) -> Attribute action
onContextMenu = runFn2 handler "onContextMenu"

onDoubleClick :: forall action. (MouseEvent -> action) -> Attribute action
onDoubleClick = runFn2 handler "onDoubleClick"

onDrag :: forall action. (MouseEvent -> action) -> Attribute action
onDrag = runFn2 handler "onDrag"

onDragEnd :: forall action. (MouseEvent -> action) -> Attribute action
onDragEnd = runFn2 handler "onDragEnd"

onDragEnter :: forall action. (MouseEvent -> action) -> Attribute action
onDragEnter = runFn2 handler "onDragEnter"

onDragExit :: forall action. (MouseEvent -> action) -> Attribute action
onDragExit = runFn2 handler "onDragExit"

onDragLeave :: forall action. (MouseEvent -> action) -> Attribute action
onDragLeave = runFn2 handler "onDragLeave"

onDragOver :: forall action. (MouseEvent -> action) -> Attribute action
onDragOver = runFn2 handler "onDragOver"

onDragStart :: forall action. (MouseEvent -> action) -> Attribute action
onDragStart = runFn2 handler "onDragStart"

onDrop :: forall action. (MouseEvent -> action) -> Attribute action
onDrop = runFn2 handler "onDrop"

onMouseDown :: forall action. (MouseEvent -> action) -> Attribute action
onMouseDown = runFn2 handler "onMouseDown"

onMouseEnter :: forall action. (MouseEvent -> action) -> Attribute action
onMouseEnter = runFn2 handler "onMouseEnter"

onMouseLeave :: forall action. (MouseEvent -> action) -> Attribute action
onMouseLeave = runFn2 handler "onMouseLeave"

onMouseMove :: forall action. (MouseEvent -> action) -> Attribute action
onMouseMove = runFn2 handler "onMouseMove"

onMouseOut :: forall action. (MouseEvent -> action) -> Attribute action
onMouseOut = runFn2 handler "onMouseOut"

onMouseOver :: forall action. (MouseEvent -> action) -> Attribute action
onMouseOver = runFn2 handler "onMouseOver"

onMouseUp :: forall action. (MouseEvent -> action) -> Attribute action
onMouseUp = runFn2 handler "onMouseUp"

onSelect :: forall action. (SelectionEvent -> action) -> Attribute action
onSelect = runFn2 handler "onSelect"

onTouchCancel :: forall action. (TouchEvent -> action) -> Attribute action
onTouchCancel = runFn2 handler "onTouchCancel"

onTouchEnd :: forall action. (TouchEvent -> action) -> Attribute action
onTouchEnd = runFn2 handler "onTouchEnd"

onTouchMove :: forall action. (TouchEvent -> action) -> Attribute action
onTouchMove = runFn2 handler "onTouchMove"

onTouchStart :: forall action. (TouchEvent -> action) -> Attribute action
onTouchStart = runFn2 handler "onTouchStart"

onScroll :: forall action. (UIEvent -> action) -> Attribute action
onScroll = runFn2 handler "onScroll"

onWheel :: forall action. (WheelEvent -> action) -> Attribute action
onWheel = runFn2 handler "onWheel"

onAbort :: forall action. (MediaEvent -> action) -> Attribute action
onAbort = runFn2 handler "onAbort"

onCanPlay :: forall action. (MediaEvent -> action) -> Attribute action
onCanPlay = runFn2 handler "onCanPlay"

onCanPlayThrough :: forall action. (MediaEvent -> action) -> Attribute action
onCanPlayThrough = runFn2 handler "onCanPlayThrough"

onDurationChange :: forall action. (MediaEvent -> action) -> Attribute action
onDurationChange = runFn2 handler "onDurationChange"

onEmptied :: forall action. (MediaEvent -> action) -> Attribute action
onEmptied = runFn2 handler "onEmptied"

onEncrypted :: forall action. (MediaEvent -> action) -> Attribute action
onEncrypted = runFn2 handler "onEncrypted"

onEnded :: forall action. (MediaEvent -> action) -> Attribute action
onEnded = runFn2 handler "onEnded"

onError :: forall action. (MediaEvent -> action) -> Attribute action
onError = runFn2 handler "onError"

onLoad :: forall action. (MediaEvent -> action) -> Attribute action
onLoad = runFn2 handler "onLoad"

onLoadedData :: forall action. (MediaEvent -> action) -> Attribute action
onLoadedData = runFn2 handler "onLoadedData"

onLoadedMetadata :: forall action. (MediaEvent -> action) -> Attribute action
onLoadedMetadata = runFn2 handler "onLoadedMetadata"

onLoadStart :: forall action. (MediaEvent -> action) -> Attribute action
onLoadStart = runFn2 handler "onLoadStart"

onPause :: forall action. (MediaEvent -> action) -> Attribute action
onPause = runFn2 handler "onPause"

onPlay :: forall action. (MediaEvent -> action) -> Attribute action
onPlay = runFn2 handler "onPlay"

onPlaying :: forall action. (MediaEvent -> action) -> Attribute action
onPlaying = runFn2 handler "onPlaying"

onProgress :: forall action. (MediaEvent -> action) -> Attribute action
onProgress = runFn2 handler "onProgress"

onRateChange :: forall action. (MediaEvent -> action) -> Attribute action
onRateChange = runFn2 handler "onRateChange"

onSeeked :: forall action. (MediaEvent -> action) -> Attribute action
onSeeked = runFn2 handler "onSeeked"

onSeeking :: forall action. (MediaEvent -> action) -> Attribute action
onSeeking = runFn2 handler "onSeeking"

onStalled :: forall action. (MediaEvent -> action) -> Attribute action
onStalled = runFn2 handler "onStalled"

onSuspend :: forall action. (MediaEvent -> action) -> Attribute action
onSuspend = runFn2 handler "onSuspend"

onTimeUpdate :: forall action. (MediaEvent -> action) -> Attribute action
onTimeUpdate = runFn2 handler "onTimeUpdate"

onVolumeChange :: forall action. (MediaEvent -> action) -> Attribute action
onVolumeChange = runFn2 handler "onVolumeChange"

onWaiting :: forall action. (MediaEvent -> action) -> Attribute action
onWaiting = runFn2 handler "onWaiting"

foreign import handler :: forall ev a. Fn2 String (ev -> a) (Attribute a)
