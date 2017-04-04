'use strict';

// module Pux.Renderer.React

var React = (typeof require === 'function' && require('react'))
         || (typeof window === 'object' && window.React);

var reactClasses = {};

exports.renderToDOM_ = function (selector) {
  var ReactDOM = (typeof require === 'function' && require('react-dom'))
              || (typeof window === 'object' && window.ReactDOM);

  return function (reactClass) {
    return function () {
      ReactDOM.render(React.createElement(reactClass), document.querySelector(selector))
    };
  };
};

exports.renderToString_ = function (reactClass) {
  var ReactDOMServer = (typeof require === 'function' && require('react-dom/server'))
                    || (typeof window === 'object' && window.ReactDOMServer);

  return function () {
    return ReactDOMServer.renderToString(React.createElement(reactClass));
  };
};

exports.renderToStaticMarkup_ = function (reactClass) {
  var ReactDOMServer = (typeof require === 'function' && require('react-dom/server'))
                    || (typeof window === 'object' && window.ReactDOMServer);

  return function () {
    return ReactDOMServer.renderToStaticMarkup(React.createElement(reactClass));
  };
};

// Return a React component from virtual DOM signal.
exports.toReact = function (vdomSignal) {
  var isBrowser = typeof window === 'object';

  // Sets the focus of element with "data-focused" attribute (`focused` constructor).
  // Provides declarative focus control.
  function setFocus () {
    if (isBrowser && window.__puxActiveElement === true) {
      if (window.__puxActiveElement !== document.activeElement) {
        var el = window.__puxActiveElement = document.querySelector('[data-focused]')
        if (el !== null && document.activeElement !== el) {
          el.focus();
        }
      }
    }
  }

  return React.createClass({
    componentWillMount: function () {
      var ctx = this;
      vdomSignal.subscribe(function () {
        ctx.forceUpdate();
      });
    },
    componentDidMount: setFocus,
    componentDidUpdate: setFocus,
    render: function () {
      var vdom = vdomSignal.get();

      if (vdom.length === 1) return vdom[0];

      // Wrap multiple root elements in a div
      return React.createElement('div', null, vdom);
    }
  });
};

// Create an HTML constructor for a React class using a unique key.
// When rendered this element is replaced by the class.
exports.registerClass = function (reactClass) {
  return function (key) {
    return function (markup) {
      reactClasses[key] = reactClass;
      return markup;
    };
  };
};

exports.reactAttr = function (str) {
  return str;
};

exports.reactHandler = function (input) {
  return function (handler) {
    return function (ev) {
      if (ev.nativeEvent === undefined) {
        return input(handler(ev))();
      }
      return input(handler(ev.nativeEvent))();
    };
  };
};

exports.reactElement = function (node, name, attrs, children) {
  if (node.__pux_react_elm !== undefined) return node.__pux_react_elm;

  if (name === 'style') {
    // Convert style element children to string
    if (children !== null && children.length) {
      attrs.dangerouslySetInnerHTML = { __html: children.join(' ') };
      children = null
    }
  } else if (name === 'reactclass') {
    // Support rendering of foreign react classes registered through
    // `registerClass`
    var key = attrs.key;
    var reactClass = reactClasses[key];

    if (reactClass) {
      return React.createElement(reactClass, reactAttrs, children);
    } else {
      return React.createElement('div', reactAttrs, children);
    }
  }

  // Support declarative focus attribute
  if (attrs.focused !== undefined) {
    if (typeof window === 'object') {
      window.__puxActiveElement = true;
      attrs['data-focused'] = 'focused';
    }
  }

  // Parse inline style, because React expects a map instead of a string.
  if (attrs.style !== undefined) {
    attrs.style = attrs.style.split(';').reduce(function (prev, curr) {
      if (!curr) return prev;
      var prop = curr.split(':');
      var key = prop[0].replace(/^ */, '').replace(/ *$/, '').replace(/(-\w)/g, function (m, w) {
        return w[1].toUpperCase();
      });
      var val = prop[1].replace(/^ */, '').replace(/ *$/, '');
      prev[key] = val;
      return prev;
    }, {});
  }

  if (attrs.dangerouslySetInnerHTML !== undefined) {
    attrs.dangerouslySetInnerHTML = { __html : attrs.dangerouslySetInnerHTML };
  }

  // convert smolder attribute names to react attribute names
  var reactAttrs = {};
  for (var key in attrs) {
    if (attrMap[key]) {
      reactAttrs[attrMap[key]] = attrs[key];
    } else {
      reactAttrs[key] = attrs[key];
    }
  }

  // Eliminate React "key" errors for parents with a single child
  // (React checks for keys when children is passed as an array)
  if (children !== null && children.length === 1) {
    children = children[0];
  }

  // Cache react element. If the same node is rendered again the cached element will be used.
  node.__pux_react_elm = React.createElement(name, reactAttrs, children);

  return node.__pux_react_elm;
};

exports.reactText = function (string) {
  return string;
};

// Normalize Smolder attribute names with React attribute names
var attrMap = {
  'accesskey': 'accessKey',
  'allowfullscreen': 'allowFullScreen',
  'allowtransparency': 'allowTransparency',
  'autocomplete': 'autoComplete',
  'autofocus': 'autoFocus',
  'autoplay': 'autoPlay',
  'cellpadding': 'cellPadding',
  'cellspacing': 'cellSpacing',
  'charset': 'charSet',
  'class': 'className',
  'classid': 'classID',
  'colspan': 'colSpan',
  'contextmenu': 'contextMenu',
  'crossorigin': 'crossOrigin',
  'datetime': 'dateTime',
  'enctype': 'encType',
  'formaction': 'formAction',
  'formenctype': 'formEncType',
  'formmethod': 'formMethod',
  'formnovalidate': 'formNoValidate',
  'formtarget': 'formTarget',
  'frameborder': 'frameBorder',
  'for': 'htmlFor',
  'inputmode': 'inputMode',
  'keyparams': 'keyParams',
  'keytype': 'keyType',
  'marginheight': 'marginHeight',
  'marginwidth': 'marginWidth',
  'maxlength': 'maxLength',
  'mediagroup': 'mediaGroup',
  'minlength': 'minLength',
  'novalidate': 'noValidate',
  'radiogroup': 'radioGroup',
  'readonly': 'readOnly',
  'rowspan': 'rowSpan',
  'spellcheck': 'spellCheck',
  'srcdoc': 'srcDoc',
  'srclang': 'srcLang',
  'srcset': 'srcSet',
  'tabindex': 'tabIndex',
  'usemap': 'useMap'
}
