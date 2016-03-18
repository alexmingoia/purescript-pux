'use strict';

// module Pux.Html.Elements

var React = (typeof require === 'function' && require('react'))
         || (typeof window === 'object' && window.React);

exports.text = function (text) {
  return text;
};

exports.element = function (tagName, attrs, children) {
  if (Array.isArray(children[0])) children = children[0];

  var props = attrs.reduce(function (obj, attr) {
    var key = attr[0];
    var val = attr[1];
    obj[key] = val;
    return obj;
  }, {});

  return React.createElement.apply(React, [tagName, props].concat(children));
};
