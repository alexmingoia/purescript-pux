exports.memoize = function (fn) {
  return function (st) {
    var vdom = st.__vdom;
    if (st.__st === st) return vdom;
    vdom = fn(st);
    st.__vdom = vdom;
    st.__st = st;
    return vdom;
  };
};
