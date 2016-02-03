"use strict";

// module Pux.Router

exports.sampleUrlFF = function locationChanged(constant) {
  var url = "";
  if (typeof window !== 'undefined') {
    url = window.location.pathname + window.location.search;
  }
  var out = constant(url);
  if (typeof window !== 'undefined') {
    window.onpopstate = function () {
      out.set(window.location.pathname + window.location.search);
    };
  }
  return function () {
    return out;
  };
};

exports.pushStateFF = function (url) {
  return function (event) {
    if (event.currentTarget.nodeName === 'A') {
      event.preventDefault();
    }
    if (typeof window !== 'undefined') {
      window.history.pushState({}, document.title, url);
      window.dispatchEvent(new Event('popstate'));
    }
    return function () {};
  };
};
