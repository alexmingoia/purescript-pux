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

// :: (a -> b) -> Html a -> Html b
exports.forwardTo = function (parentAction) {
  return function (html) {
    if (!html.props) return html
    var childAction = html.props.puxParentAction;
    var action = parentAction;
    if (childAction) {
      action = function (a) {
        return parentAction(childAction(a));
      }
    }
    return React.cloneElement(html, { puxParentAction: action });
  };
};

// :: (a -> b) -> Attribute a -> Attribute b
exports.mapAttribute = function (f) {
  return function (attr) {
    if (typeof attr[1] !== 'function') {
      return attr;
    }
    return [attr[0], function(input, parentAction) {
      return attr[1](input, function(e) {
        return f(parentAction(e));
      });
    }];
  };
};
