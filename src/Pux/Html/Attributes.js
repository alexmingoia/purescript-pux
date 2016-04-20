'use strict';

// module Pux.Html.Attributes

exports.attr = function (key) {
  return function (val) {
    if (key === 'dangerouslySetInnerHTML') {
      val = { __html: val };
    } else if (key === 'style') {
      val = val.reduce(function (obj, tuple) {
        obj[tuple.value0] = tuple.value1;
        return obj;
      }, {});
    }
    return [key, val];
  };
};
