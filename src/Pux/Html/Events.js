'use strict';

// module Pux.Html.Events

function puxHandler(h) {
  h.isPuxHandler = true
  return h;
};

exports.handler = function (key, action) {
  return [key, puxHandler(function (input, parentAction) {
    return function (ev) {
      if ((key === 'onSubmit')
      || (key === 'onClick' && ev.currentTarget.nodeName.toLowerCase() === 'a')) {
        ev.preventDefault();
      }
      input(parentAction(action(ev)))();
    };
  })];
};

exports.onKeyHandler = function (keyName, action) {
  return ["onKeyUp", function (input, parentAction) {
    return function (ev) {
      if (ev.key.toLowerCase() === keyName.toLowerCase()) {
        input(parentAction(action(ev)))();
      }
    };
  }];
};
