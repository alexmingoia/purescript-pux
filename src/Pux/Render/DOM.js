"use strict";

// module Pux.Render.DOM

var React = (typeof require === 'function' && require('react'))
         || (typeof window === 'object' && window.React);

exports.renderFF = function (component) {
  var ReactDOM = (typeof require === 'function' && require('react-dom'))
              || (typeof window === 'object' && window.ReactDOM);

  return function (container) {
    return function () {
      ReactDOM.render(React.createElement(component), container);
      return function () {};
    };
  };
};
