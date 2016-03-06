"use strict";

// module Pux.Render.HTML

var React = (typeof require === 'function' && require('react'))
         || (typeof window === 'object' && window.React);

exports.renderToStringFF = function (component) {
  var ReactDOMServer = (typeof require === 'function' && require('react-dom/server'))
                    || (typeof window === 'object' && window.ReactDOMServer);
  return function (container) {
    return function () {
      return ReactDOMServer.renderToString(React.createElement(component), container);
    };
  };
};
