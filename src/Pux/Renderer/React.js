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

exports.toReact = function (vdomSignal) {
  return React.createClass({
    componentWillMount: function () {
      var ctx = this;
      vdomSignal.subscribe(function () {
        ctx.forceUpdate();
      });
    },
    componentDidMount: function () {
      this.isBrowser = (typeof window === 'object');
      this.setFocus();
    },
    componentDidUpdate: function () {
      this.setFocus();
    },
    setFocus: function () {
      if (this.isBrowser) {
        if (window.__puxActiveElement) {
          if (window.__puxActiveElement !== document.activeElement) {
            var el = window.__puxActiveElement = document.querySelector('[data-focused]')
            if (el && document.activeElement !== el) {
              el.focus();
            }
          }
        }
      }
    },
    render: function () {
      var vdom = vdomSignal.get();

      if (vdom.length === 1) return vdom[0];

      // Wrap multiple root elements in a div
      return React.createElement('div', null, vdom);
    }
  })
};

exports.registerClass = function (reactClass) {
  return function (key) {
    return function (markup) {
      reactClasses[key] = reactClass;
      return markup;
    };
  };
};

exports.reactElement = function (renderNodes, node, input, name, attrs, handlers, children) {
  if (node.__pux_react_elm) return node.__pux_react_elm;

  if (children !== null) {
    children = renderNodes(input)(children);
  }

  // Support declarative focus attribute
  if (attrs.focused) {
    if (typeof window === 'object') {
      window.__puxActiveElement = true;
      attrs['data-focused'] = 'focused';
    }
  }

  // Parse inline style, because React expects a map instead of a string.
  if (attrs.style) {
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

  // Hook event handlers to input channel
  Object.keys(handlers).forEach(function (key) {
    attrs[key] = function (e) {
      input(handlers[key](e))();
    };
  });

  if (attrs.dangerouslySetInnerHTML) {
    attrs.dangerouslySetInnerHTML = { __html : attrs.dangerouslySetInnerHTML };
  } else if (name === 'style' && children && children.length) {
    attrs.dangerouslySetInnerHTML = { __html : children.join(' ') };
    children = null
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

  if (children !== null) {
    if (children.length === 0) {
      children = null;
    } else if (children.length === 1) {
      children = children[0];
    }
  }

  // Support rendering of foreign react classes registered through
  // `registerClass`
  if (name === 'reactclass') {
    var key = attrs.key;
    var reactClass = reactClasses[key];

    if (reactClass) {
      return React.createElement(reactClass, reactAttrs, children);
    } else {
      return React.createElement('div', reactAttrs, children);
    }
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
