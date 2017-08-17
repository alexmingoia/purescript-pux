'use strict';
var React = require('react');
var ReactDOMServer = require('react-dom/server');

function list(props) {
  var children = props.children;
  return React.createElement('ul', null, children);
}

exports.listComponent = list;
