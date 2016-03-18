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

exports.render = function (input, parentAction, html) {
  if (typeof html === 'string') return html;

  var props = html.props
  var newProps = {};

  for (var key in props) {
    if (key !== 'puxParentAction' && typeof props[key] === 'function') {
      newProps[key] = props[key](input, parentAction);
    }
  }

  var newChildren = React.Children.map(html.props.children, function (child) {
    var childAction = child.props && child.props.puxParentAction;
    return exports.render(input, childAction || parentAction, child);
  });

  return React.cloneElement(html, newProps, newChildren);
};
