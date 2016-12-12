"use strict";

// module Pux

var React = (typeof require === 'function' && require('react'))
         || (typeof window === 'object' && window.React);

function composeAction (parentAction, html) {
  var childAction = html.props && html.props.puxParentAction;
  var action = parentAction;
  if (childAction) {
    action = function (a) {
      return parentAction(childAction(a));
    };
  }
  return action;
};

function threadInput (input, parentAction, html) {
  var props = html.props
  var newProps = {};

  for (var key in props) {
    if (key !== 'puxParentAction' && key !== 'view' && typeof props[key] === 'function') {
      newProps[key] = props[key](input, parentAction);
    }
  }

  var newChildren = React.Children.map(html.props.children, function (child) {
    if (typeof child === 'string') {
      return child;
    } else {
      return threadInput(input, composeAction(parentAction, child), child);
    }
  });

  return React.cloneElement(html, newProps, newChildren);
};

function reactClass(htmlSignal) {
  return React.createClass({
    getInitialState: function () {
      return { html: htmlSignal.get() }
    },
    componentWillMount: function () {
      var ctx = this;
      htmlSignal.subscribe(function (html) {
        if (html !== ctx.state.html) {
          ctx.setState({ html: html })
        }
      });
    },
    render: function () {
      return this.state.html;
    }
  });
}

exports.renderToDOM = function (selector) {
  var ReactDOM = (typeof require === 'function' && require('react-dom'))
              || (typeof window === 'object' && window.ReactDOM);
  return function (htmlSignal) {
    ReactDOM.render(htmlSignal.get(), document.querySelector(selector))
    return function () {};
  };
};

exports.renderToString = function (htmlSignal) {
  var ReactDOMServer = (typeof require === 'function' && require('react-dom/server'))
                    || (typeof window === 'object' && window.ReactDOMServer);
  return function () {
    return ReactDOMServer.renderToString(htmlSignal.get());
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

exports.render = function (input) {
  return function (stateSignal) {
    return function (view) {
      if (typeof html === 'string') {
        html = React.createElement('div', null, html);
      }

      return React.createElement(React.createClass({
        componentWillMount: function () {
          var ctx = this;
          stateSignal.subscribe(function (state) {
            ctx.setState({ state: state })
          });
        },
        getInitialState: function () {
          return { state: stateSignal.get() }
        },
        childContextTypes: {
          input: React.PropTypes.func
        },
        getChildContext: function () {
          return { input: input };
        },
        render: function () {
          var html = view(this.state.state);
          var parentAction = function (a) { return a; };
          return threadInput(input, composeAction(parentAction, html), html);
        }
      }));
    };
  };
};
