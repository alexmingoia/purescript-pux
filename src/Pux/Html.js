"use strict";

// module Pux.Html

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

var SomeComponent = React.createClass({
  contextTypes: {
    input: React.PropTypes.func
  },
  shouldComponentUpdate: function (nextProps) {
    return nextProps.state !== this.props.state
  },
  render: function () {
    var html = this.props.view(this.props.state);
    var input = this.context.input;
    var parentAction = this.props.puxParentAction;
    return threadInput(input, composeAction(parentAction, html), html);
  }
});

exports.memoize = function (view) {
  return function (state) {
    return React.createElement(SomeComponent, { state: state, view: view });
  };
};
