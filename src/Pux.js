"use strict";

// module Pux

var React = (typeof require === 'function' && require('react'))
         || (typeof window === 'object' && window.React);

function reactClass(htmlSignal) {
  return React.createClass({
    componentWillMount: function () {
      var ctx = this;
      htmlSignal.subscribe(function () {
        ctx.forceUpdate();
      });
    },
    render: function () {
      return htmlSignal.get()
    }
  });
}

exports.renderToDOM = function (selector) {
  var ReactDOM = (typeof require === 'function' && require('react-dom'))
              || (typeof window === 'object' && window.ReactDOM);
  return function (htmlSignal) {
    var elem = React.createElement(reactClass(htmlSignal));
    ReactDOM.render(elem, document.querySelector(selector))
    return function () {};
  };
};

exports.renderToString = function (htmlSignal) {
  var ReactDOMServer = (typeof require === 'function' && require('react-dom/server'))
                    || (typeof window === 'object' && window.ReactDOMServer);
  var elem = React.createElement(reactClass(htmlSignal));
  return function () {
    return ReactDOMServer.renderToString(elem);
  };
};

exports.toReact = function (htmlSignal) {
  return function () {
    return reactClass(htmlSignal);
  };
};

exports.fromReact = function (comp) {
  return function (attrs) {
    return function (children) {
      if (Array.isArray(children[0])) children = children[0];

      var props = attrs.reduce(function (obj, attr) {
        var key = attr[0];
        var val = attr[1];
        obj[key] = val;
        return obj;
      }, {});

      return React.createElement.apply(null, [comp, props].concat(children))
    };
  };
};

exports.render = function (input, parentAction, html) {
  if (typeof html === 'string') {
    html = React.createElement('div', null, html);
  }

  function composeAction(parentAction, html) {
    var childAction = html.props && html.props.puxParentAction;
    var action = parentAction;
    if (childAction) {
      action = function (a) {
        return parentAction(childAction(a));
      };
    }
    return action;
  }

  function render(input, parentAction, html) {
    var props = html.props
    var newProps = {};

    for (var key in props) {
      if (key !== 'puxParentAction' && typeof props[key] === 'function') {
        newProps[key] = props[key](input, parentAction);
      }
    }

    var composeChild = function (child) {
      if (typeof child === 'string') {
        return child;
      } else {
        return render(input, composeAction(parentAction, child), child);
      }
    };

    var children = html.props && html.props.children;
    var newChildren = !children || Array.isArray(children)
      ? React.Children.map(children, composeChild)
      : composeChild(children)
      ;

    return React.cloneElement(html, newProps, newChildren);
  }

  return render(input, composeAction(parentAction, html), html);
};
