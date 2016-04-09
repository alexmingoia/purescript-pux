'use strict';

// module Pux.Html

var React = (typeof require === 'function' && require('react'))
         || (typeof window === 'object' && window.React);

exports.append = function (html1, html2) {
  if (!Array.isArray(html1)) html1 = [html1];
  if (!Array.isArray(html2)) html2 = [html2];
  return html1.concat(html2);
};
