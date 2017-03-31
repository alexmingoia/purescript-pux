exports.memoize = function (fn) {
  return (function () {
    var memo, vdom = null;

    return function (st) {
      console.log('view called')
      if (st !== memo) {
        console.log('cache miss')
        memo = st;
        vdom = fn(st);
      }
      return vdom;
    };
  })();
};
