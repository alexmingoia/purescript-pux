"use strict";

// module Pux.Render.HTML

exports.renderToStringFF = function (component) {
  return function (container) {
    return function () {
      var ReactDOMServer = require('react-dom/server');
      return ReactDOMServer.renderToString(React.createElement(component), container);
    };
  };
};
