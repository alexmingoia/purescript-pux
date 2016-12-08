'use strict';

// module Pux.Html.Events

exports.on = function (key, decoder, action) {
  return [eventNameMap[key] || key, function (input, parentAction) {
    return function (event) {
      var ev = decoder(event)();
      input(parentAction(action(ev)))();
    };
  }];
};

exports.onKeyHandler = function (keyName, decoder, action) {
  return ["onKeyUp", function (input, parentAction) {
    return function (event) {
      var ev = decoder(event)();
      if (ev.key.toLowerCase() === keyName.toLowerCase()) {
        input(parentAction(action(ev)))();
      }
    };
  }];
};

exports.defaultDecoder = function (event) {
  return function () {
    event.preventDefault();
    event.value = event.currentTarget.value;
    return event;
  };
};


var eventNameMap = {
  copy: "onCopy",
  cut: "onCut",
  paste: "onPaste",
  compositionend: "onCompositionEnd",
  compositionstart: "onCompositionStart",
  compositionupdate: "onCompositionUpdate",
  keydown: "onKeyDown",
  keypress: "onKeyPress",
  keyup: "onKeyUp",
  focus: "onFocus",
  blur: "onBlur",
  change: "onChange",
  input: "onInput",
  submit: "onSubmit",
  click: "onClick",
  contextmenu: "onContextMenu",
  doubleclick: "onDoubleClick",
  drag: "onDrag",
  dragend: "onDragEnd",
  dragenter: "onDragEnter",
  dragexit: "onDragExit",
  dragleave: "onDragLeave",
  dragover: "onDragOver",
  dragstart: "onDragStart",
  drop: "onDrop",
  mousedown: "onMouseDown",
  mouseenter: "onMouseEnter",
  mouseleave: "onMouseLeave",
  mousemove: "onMouseMove",
  mouseout: "onMouseOut",
  mouseover: "onMouseOver",
  mouseup: "onMouseUp",
  select: "onSelect",
  touchcancel: "onTouchCancel",
  touchend: "onTouchEnd",
  touchmove: "onTouchMove",
  touchstart: "onTouchStart",
  scroll: "onScroll",
  wheel: "onWheel",
  abort: "onAbort",
  canplay: "onCanPlay",
  canplaythrough: "onCanPlayThrough",
  durationchange: "onDurationChange",
  emptied: "onEmptied",
  encrypted: "onEncrypted",
  ended: "onEnded",
  error: "onError",
  load: "onLoad",
  loadeddata: "onLoadedData",
  loadedmetadata: "onLoadedMetadata",
  loadstart: "onLoadStart",
  pause: "onPause",
  play: "onPlay",
  playing: "onPlaying",
  progress: "onProgress",
  ratechange: "onRateChange",
  seeked: "onSeeked",
  seeking: "onSeeking",
  stalled: "onStalled",
  suspend: "onSuspend",
  timeupdate: "onTimeUpdate",
  volumechange: "onVolumeChange",
  waiting: "onWaiting"
}
