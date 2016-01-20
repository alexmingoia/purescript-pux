"use strict";

// module Pux.Render.DOM

exports.renderFF = function (component) {
  return function (container) {
    return function () {
      ReactDOM.render(component, container);
      return function () {};
    };
  };
};
