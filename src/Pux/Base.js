"use strict";

// module Pux.Base

var React = (typeof require === 'function' && require('react'))
    || (typeof window === 'object' && window.React);

// Copy of Pux.render to remove potential conversion of html into <div>{html}</div>
exports.render = function (input, parentAction, html) {
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

        var newChildren = React.Children.map(html.props.children, function (child) {
            if (typeof child === 'string') {
                return child;
            } else {
                return render(input, composeAction(parentAction, child), child);
            }
        });

        return React.cloneElement(html, newProps, newChildren);
    }

    return render(input, composeAction(parentAction, html), html);
};

// Copy of Pux.Html.Elements.element, to generalise type
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
