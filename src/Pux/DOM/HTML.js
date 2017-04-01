exports.memoize = function (fn) {
  return (function () {
    var n, memo = null;

    return function (st) {
      if (st !== n) {
        n = st;
        memo = fn(st);
      }
      return memo;
    };
  })();
};
