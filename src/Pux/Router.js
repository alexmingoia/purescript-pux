"use strict";

// module Pux.Router

exports.sampleUrlFF = function locationChanged(constant) {
  var out = constant(window.location.pathname + window.location.search);
  window.onpopstate = function () {
    out.set(window.location.pathname + window.location.search);
  };
  return function () {
    return out;
  };
};

exports.pushStateFF = function (url) {
  return function (event) {
    if (event.currentTarget.nodeName === 'A') {
      event.preventDefault();
    }
    window.history.pushState({}, document.title, url);
    window.dispatchEvent(new Event('popstate'));
    return function () {};
  };
};
