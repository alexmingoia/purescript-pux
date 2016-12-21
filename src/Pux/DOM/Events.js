exports.targetValue = function (ev) {
  return ev.currentTarget && ev.currentTarget.value || '';
};
