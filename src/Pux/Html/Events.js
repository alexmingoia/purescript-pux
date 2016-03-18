'use strict';

// module Pux.Html.Events

exports.handler = function (key, action) {
  return [key, function (input, parentAction) {
    return function (ev) {
      ev.preventDefault();
      input(parentAction(action(ev)))();
    };
  }];
};
