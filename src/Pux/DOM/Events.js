exports.targetValue = function (ev) {
  if (ev.target === undefined) return '';
  return ev.target.value;
};
