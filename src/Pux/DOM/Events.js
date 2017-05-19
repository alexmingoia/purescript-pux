exports.targetValue = function (ev) {
  var target = ev['target'];
  if (target === undefined) return '';
  var value = target['value'];
  if (value === undefined) return '';
  return value;
};
