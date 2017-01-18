"use strict";

// module Pux.Router

exports.createUrlSignal = function locationChanged(constant) {
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


function puxHandler(h) {
  h.isPuxHandler = true
  return h;
};

exports.linkHandler = function (url) {
  return ['onClick', puxHandler(function (input, parentAction) {
    return function (ev) {
      ev.preventDefault();
      if (typeof window !== 'undefined') {
        window.history.pushState({}, document.title, url);
        window.dispatchEvent(new Event('popstate'));
      }
    };
  })];
};

exports.navigateTo = function (url) {
  return function () {
    if (typeof window !== 'undefined') {
      window.history.pushState({}, document.title, url);
      window.dispatchEvent(new Event('popstate'));
    }
  }
};
