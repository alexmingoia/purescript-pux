"use strict";

// module Pux.React

exports.getInputFF = function (ctx) {
  return function () {
    return ctx.state.input;
  };
};

exports.writeStateFF = function (ctx) {
  return function (state) {
    ctx.replaceState(state);
    return function () {
      return function () {};
    };
  };
};

exports.makeReactComponentFF = function (render) {
  return function (componentWillMount) {
    return React.createElement(React.createClass({
      componentWillMount: function () {
        componentWillMount(this)();
      },
      render: function () {
        return render(this)(this.state)();
      }
    }));
  };
};

exports.makeAttrFF = function (key) {
  return function(value) {
    var result = {};
    result[key] = value;
    return result;
  };
};

exports.makeHandlerFF = function (key) {
  return function (f) {
    return function (input) {
      var result = {};
      result[key] = function (ev) {
        f(input)(ev)();
      };
      return result;
    };
  };
};

exports.makeAttrWithObjFF = function (key) {
  return function(value) {
    var result = {};
    var props = {};
    props[key] = result;
    for (var subprop in value) {
      if (value.hasOwnProperty(subprop)) {
        result[subprop] = value[subprop];
      }
    }
    return props;
  };
};

exports.stopPropagationFF = function (ev) {
  ev.stopPropagation();
  return function () {
    return function () {};
  };
};

exports.preventDefaultFF = function (ev) {
  ev.preventDefault();
  return function () {
    return function () {};
  };
};

function makeProps(props) {
  var result = {};
  for (var i = 0, len = props.length; i < len; i++) {
    var prop = props[i];
    for (var key in prop) {
      if (prop.hasOwnProperty(key)) {
        result[key] = prop[key];
      }
    }
  }
  return result;
};

exports.makeReactElementFF = function (tagName) {
  return function (props) {
    return function (children) {
      return React.createElement(tagName, props.length > 0 ? makeProps(props) : null, children);
    }
  }
};

exports.makeReactTextFF = function (text) {
  return text;
};
