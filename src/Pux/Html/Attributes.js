'use strict';

// module Pux.Html.Attributes

exports.attr = function (key) {
  return function (val) {
    if (key === 'dangerouslySetInnerHTML') {
      val = { __html: val };
    }
    return [key, val];
  };
};
