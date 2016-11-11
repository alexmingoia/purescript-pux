module Pux.Html.Events where

import Data.Function.Uncurried (Fn2, runFn2)
import Pux.Html (Attribute)

type ClipboardEvent a b =
  { target :: a
  , currentTarget :: b
  }

type CompositionEvent a b =
  { target :: a
  , currentTarget :: b
  , data :: String
  }

type KeyboardEvent a b =
  { target :: a
  , currentTarget :: b
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

type FocusEvent a b c =
  { target :: a
  , currentTarget :: b
  , relatedTarget :: c
  }

type FormEvent a b =
  { target :: a
  , currentTarget :: b
  }

type MouseEvent a b =
  { target :: a
  , currentTarget :: b
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

type SelectionEvent a b =
  { target :: a
  , currentTarget :: b
}

type TouchEvent a b =
  { target :: a
  , currentTarget :: b
  , altKey :: Boolean
  , ctrlKey :: Boolean
  , metaKey :: Boolean
  , shiftKey :: Boolean
  }

type UIEvent a b =
  { target :: a
  , currentTarget :: b
  , detail :: Number
  }

type WheelEvent a b =
  { target :: a
  , currentTarget :: b
  , deltaMode :: Number
  , deltaX :: Number
  , deltaY :: Number
  , deltaZ :: Number
  }

type MediaEvent a b =
  { target :: a
  , currentTarget :: b
  }

onCopy :: forall action a b. (ClipboardEvent a b -> action) -> Attribute action
onCopy = runFn2 handler "onCopy"

onCut :: forall action a b. (ClipboardEvent a b -> action) -> Attribute action
onCut = runFn2 handler "onCut"

onPaste :: forall action a b. (ClipboardEvent a b -> action) -> Attribute action
onPaste = runFn2 handler "onPaste"

onCompositionEnd :: forall action a b. (CompositionEvent a b -> action) -> Attribute action
onCompositionEnd = runFn2 handler "onCompositionEnd"

onCompositionStart :: forall action a b. (CompositionEvent a b -> action) -> Attribute action
onCompositionStart = runFn2 handler "onCompositionStart"

onCompositionUpdate :: forall action a b. (CompositionEvent a b -> action) -> Attribute action
onCompositionUpdate = runFn2 handler "onCompositionUpdate"

onKeyDown :: forall action a b. (KeyboardEvent a b -> action) -> Attribute action
onKeyDown = runFn2 handler "onKeyDown"

onKeyPress :: forall action a b. (KeyboardEvent a b -> action) -> Attribute action
onKeyPress = runFn2 handler "onKeyPress"

onKeyUp :: forall action a b. (KeyboardEvent a b -> action) -> Attribute action
onKeyUp = runFn2 handler "onKeyUp"

-- | Send action only if specified key is pressed (on key up)
onKey :: forall action a b. String -> (KeyboardEvent a b -> action) -> Attribute action
onKey = runFn2 onKeyHandler

onFocus :: forall action a b c. (FocusEvent a b c -> action) -> Attribute action
onFocus = runFn2 handler "onFocus"

onBlur :: forall action a b c. (FocusEvent a b c -> action) -> Attribute action
onBlur = runFn2 handler "onBlur"

onChange :: forall action a b. (FormEvent a b -> action) -> Attribute action
onChange = runFn2 handler "onChange"

onInput :: forall action a b. (FormEvent a b -> action) -> Attribute action
onInput = runFn2 handler "onInput"

onSubmit :: forall action a b. (FormEvent a b -> action) -> Attribute action
onSubmit = runFn2 handler "onSubmit"

onClick :: forall action a b. (MouseEvent a b -> action) -> Attribute action
onClick = runFn2 handler "onClick"

onContextMenu :: forall action a b. (MouseEvent a b -> action) -> Attribute action
onContextMenu = runFn2 handler "onContextMenu"

onDoubleClick :: forall action a b. (MouseEvent a b -> action) -> Attribute action
onDoubleClick = runFn2 handler "onDoubleClick"

onDrag :: forall action a b. (MouseEvent a b -> action) -> Attribute action
onDrag = runFn2 handler "onDrag"

onDragEnd :: forall action a b. (MouseEvent a b -> action) -> Attribute action
onDragEnd = runFn2 handler "onDragEnd"

onDragEnter :: forall action a b. (MouseEvent a b -> action) -> Attribute action
onDragEnter = runFn2 handler "onDragEnter"

onDragExit :: forall action a b. (MouseEvent a b -> action) -> Attribute action
onDragExit = runFn2 handler "onDragExit"

onDragLeave :: forall action a b. (MouseEvent a b -> action) -> Attribute action
onDragLeave = runFn2 handler "onDragLeave"

onDragOver :: forall action a b. (MouseEvent a b -> action) -> Attribute action
onDragOver = runFn2 handler "onDragOver"

onDragStart :: forall action a b. (MouseEvent a b -> action) -> Attribute action
onDragStart = runFn2 handler "onDragStart"

onDrop :: forall action a b. (MouseEvent a b -> action) -> Attribute action
onDrop = runFn2 handler "onDrop"

onMouseDown :: forall action a b. (MouseEvent a b -> action) -> Attribute action
onMouseDown = runFn2 handler "onMouseDown"

onMouseEnter :: forall action a b. (MouseEvent a b -> action) -> Attribute action
onMouseEnter = runFn2 handler "onMouseEnter"

onMouseLeave :: forall action a b. (MouseEvent a b -> action) -> Attribute action
onMouseLeave = runFn2 handler "onMouseLeave"

onMouseMove :: forall action a b. (MouseEvent a b -> action) -> Attribute action
onMouseMove = runFn2 handler "onMouseMove"

onMouseOut :: forall action a b. (MouseEvent a b -> action) -> Attribute action
onMouseOut = runFn2 handler "onMouseOut"

onMouseOver :: forall action a b. (MouseEvent a b -> action) -> Attribute action
onMouseOver = runFn2 handler "onMouseOver"

onMouseUp :: forall action a b. (MouseEvent a b -> action) -> Attribute action
onMouseUp = runFn2 handler "onMouseUp"

onSelect :: forall action a b. (SelectionEvent a b -> action) -> Attribute action
onSelect = runFn2 handler "onSelect"

onTouchCancel :: forall action a b. (TouchEvent a b -> action) -> Attribute action
onTouchCancel = runFn2 handler "onTouchCancel"

onTouchEnd :: forall action a b. (TouchEvent a b -> action) -> Attribute action
onTouchEnd = runFn2 handler "onTouchEnd"

onTouchMove :: forall action a b. (TouchEvent a b -> action) -> Attribute action
onTouchMove = runFn2 handler "onTouchMove"

onTouchStart :: forall action a b. (TouchEvent a b -> action) -> Attribute action
onTouchStart = runFn2 handler "onTouchStart"

onScroll :: forall action a b. (UIEvent a b -> action) -> Attribute action
onScroll = runFn2 handler "onScroll"

onWheel :: forall action a b. (WheelEvent a b -> action) -> Attribute action
onWheel = runFn2 handler "onWheel"

onAbort :: forall action a b. (MediaEvent a b -> action) -> Attribute action
onAbort = runFn2 handler "onAbort"

onCanPlay :: forall action a b. (MediaEvent a b -> action) -> Attribute action
onCanPlay = runFn2 handler "onCanPlay"

onCanPlayThrough :: forall action a b. (MediaEvent a b -> action) -> Attribute action
onCanPlayThrough = runFn2 handler "onCanPlayThrough"

onDurationChange :: forall action a b. (MediaEvent a b -> action) -> Attribute action
onDurationChange = runFn2 handler "onDurationChange"

onEmptied :: forall action a b. (MediaEvent a b -> action) -> Attribute action
onEmptied = runFn2 handler "onEmptied"

onEncrypted :: forall action a b. (MediaEvent a b -> action) -> Attribute action
onEncrypted = runFn2 handler "onEncrypted"

onEnded :: forall action a b. (MediaEvent a b -> action) -> Attribute action
onEnded = runFn2 handler "onEnded"

onError :: forall action a b. (MediaEvent a b -> action) -> Attribute action
onError = runFn2 handler "onError"

onLoad :: forall action a b. (MediaEvent a b -> action) -> Attribute action
onLoad = runFn2 handler "onLoad"

onLoadedData :: forall action a b. (MediaEvent a b -> action) -> Attribute action
onLoadedData = runFn2 handler "onLoadedData"

onLoadedMetadata :: forall action a b. (MediaEvent a b -> action) -> Attribute action
onLoadedMetadata = runFn2 handler "onLoadedMetadata"

onLoadStart :: forall action a b. (MediaEvent a b -> action) -> Attribute action
onLoadStart = runFn2 handler "onLoadStart"

onPause :: forall action a b. (MediaEvent a b -> action) -> Attribute action
onPause = runFn2 handler "onPause"

onPlay :: forall action a b. (MediaEvent a b -> action) -> Attribute action
onPlay = runFn2 handler "onPlay"

onPlaying :: forall action a b. (MediaEvent a b -> action) -> Attribute action
onPlaying = runFn2 handler "onPlaying"

onProgress :: forall action a b. (MediaEvent a b -> action) -> Attribute action
onProgress = runFn2 handler "onProgress"

onRateChange :: forall action a b. (MediaEvent a b -> action) -> Attribute action
onRateChange = runFn2 handler "onRateChange"

onSeeked :: forall action a b. (MediaEvent a b -> action) -> Attribute action
onSeeked = runFn2 handler "onSeeked"

onSeeking :: forall action a b. (MediaEvent a b -> action) -> Attribute action
onSeeking = runFn2 handler "onSeeking"

onStalled :: forall action a b. (MediaEvent a b -> action) -> Attribute action
onStalled = runFn2 handler "onStalled"

onSuspend :: forall action a b. (MediaEvent a b -> action) -> Attribute action
onSuspend = runFn2 handler "onSuspend"

onTimeUpdate :: forall action a b. (MediaEvent a b -> action) -> Attribute action
onTimeUpdate = runFn2 handler "onTimeUpdate"

onVolumeChange :: forall action a b. (MediaEvent a b -> action) -> Attribute action
onVolumeChange = runFn2 handler "onVolumeChange"

onWaiting :: forall action a b. (MediaEvent a b -> action) -> Attribute action
onWaiting = runFn2 handler "onWaiting"

foreign import handler :: forall ev a. Fn2 String (ev -> a) (Attribute a)

foreign import onKeyHandler :: forall ev a. Fn2 String (ev -> a) (Attribute a)
