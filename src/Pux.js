exports.start_ = function (app) {
  if (typeof window === 'object' && typeof CustomEvent === 'function') {
    var initialized, setState, index, events, states;

    // Listen for devtool and connect
    window.addEventListener('pux:devtool:init', function () {
      index = 0;
      events = [app.events.get()];
      states = [app.state.get()];

      window.dispatchEvent(new CustomEvent('pux:state:change', {
        detail: {
          index: index,
          length: states.length,
          state: stateToString(app.state.get()),
          event: eventToString(app.events.get())
        }
      }));

      if (initialized) return;
      initialized = true

      app.events.subscribe(function (ev) {
        events.push(ev);
      });

      app.state.subscribe(function (st) {
        setTimeout(function () {
          if (!setState) {
            states.push(st);
            index = states.length - 1;
          }
          setState = false;
          window.dispatchEvent(new CustomEvent('pux:state:change', {
            detail: {
              index: index,
              length: states.length,
              state: stateToString(st),
              event: eventToString(events[index])
            }
          }));
        }, 1);
      });

      window.addEventListener('pux:state:first', function () {
        setState = true;
        index = 0;
        app.state.set(states[0]);
      })

      window.addEventListener('pux:state:prev', function () {
        setState = true;
        if (states[index - 1]) index--;
        app.state.set(states[index]);
      })

      window.addEventListener('pux:state:next', function () {
        setState = true;
        if (states[index + 1]) index++;
        app.state.set(states[index]);
      })

      window.addEventListener('pux:state:last', function () {
        setState = true;
        index = states.length - 1;
        app.state.set(states[index]);
      })
    });
  }

  return app;
};

function eventToString (a) {
  a = a.value0 ? a.value0 : a;

  function toString(a) {
    var name = a.constructor.name.match(/(String|Number|Boolean)/) ? a : a.constructor.name;
    var str = [name];
    if (a.constructor.name === 'Object') {
      return JSON.stringify(stateToJSON(a));
    }
    Object.keys(a).forEach(function (key) {
      if (key[0] === 'v' && key[4] === 'e') {
        str.push('(' + toString(a[key]) + ')');
      }
    });
    return str.join(' ');
  }

  return toString(a);
};

function eventToJSON (a) {
  function toJSON(a, obj) {
    if (a.constructor.name.match(/(String|Number|Boolean)/)) {
      return a;
    } else if (a.constructor.name === 'Object') {
      Object.keys(a).forEach(function (key) {
        obj[key] = toJSON(a[key], obj[key] || {});
      });
    } else if (a.constructor.name === 'Array') {
      return a.map(function (b) {
        return toJSON(b, {});
      });
    } else {
      obj[a.constructor.name] = {};
      if (a.value0 && !a.value1) {
        obj[a.constructor.name] = toJSON(a.value0, obj[a.constructor.name] || {});
      } else {
        Object.keys(a).forEach(function (key) {
          if (key[0] === 'v' && key[4] === 'e') {
            obj[a.constructor.name][key[5]] = toJSON(a[key], obj[a.constructor.name][key[5]] || {});
          }
        });
      }
    }
    return obj;
  }

  return toJSON(a, {});
};

function stateToString (s) {
  return JSON.stringify(s, function (key, val) {
    if (!val.constructor.name.match(/(Object|Boolean|Array|String|Number|Date|Symbol)/)) {
      return eventToJSON(val);
    }
    return val;
  }, 2)
};

exports.waitState_ = function (until) {
  return function (app) {
    return function (success) {
      return function () {
        var run = false;
        app.state.subscribe(function (st) {
          if (!run && until(st)) {
            run = true;
            success(st)();
          }
        });
      };
    };
  };
};

exports.waitEvent_ = function (until) {
  return function (app) {
    return function (success) {
      return function () {
        var run = false;
        app.input.subscribe(function (ev) {
          if (!run && until(ev)) {
            run = true;
            success(app.state.get())();
          }
        });
      };
    };
  };
};
