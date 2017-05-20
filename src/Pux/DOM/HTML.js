exports.memoize_ = function (wrapper) {
  return function (view) {
    return (function () {
      var n, memo = null;

      return function (st) {
        if (st !== n) {
          n = st;
          memo = view(st);
        }
        return wrapper(new PuxStateString(st))(memo);
      };
    })();
  };
};

// Used to store out-of-band state as a String type safely.
// This allows renderes to cache vdom trees memoized by state.
// In the worst case the attribute is manipulated, it will be treated as an
// empty string and any renderer cache will always miss.
function PuxStateString (st) {
  this.st = st;
};

PuxStateString.prototype.toString = function () {
  return '';
};
