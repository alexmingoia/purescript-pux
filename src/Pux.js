exports.start_ = function (app) {
  if (typeof window === 'object' && typeof CustomEvent === 'function') {
    var hook = HookDevtool(app);

    if (window.__pux_devtool_hook) {
      window.removeEventListener('pux:devtool:init', window.__pux_devtool_hook);
      hook();
    }

    window.__pux_devtool_hook = hook;

    // Listen for devtool and connect
    window.addEventListener('pux:devtool:init', window.__pux_devtool_hook);
  }

  return app;
};

function HookDevtool (app) {
  if (window.__pux_conn === undefined) {
    window.__pux_conn = {
      setState: false,
      index: 0,
      events: [app.events.get()],
      states: [app.state.get()]
    };
  }

  var conn = window.__pux_conn;

  return function () {
    window.dispatchEvent(new CustomEvent('pux:state:change', {
      detail: {
        index: conn.index,
        length: conn.states.length,
        state: stateToString(conn.states[conn.index]),
        event: eventToString(conn.events[conn.index])
      }
    }));

    if (app.__devtool_connected) return;
    app.__devtool_connected = true;

    app.events.subscribe(function (ev) {
      conn.events.push(ev);
      conn.states.push(app.state.get());
      conn.index = conn.states.length - 1;

      window.dispatchEvent(new CustomEvent('pux:state:change', {
        detail: {
          index: conn.index,
          length: conn.states.length,
          state: stateToString(conn.states[conn.index]),
          event: eventToString(ev)
        }
      }));
    });

    app.state.subscribe(function (st) {
      if (conn.setState) {
        conn.setState = false;
        window.dispatchEvent(new CustomEvent('pux:state:change', {
          detail: {
            index: conn.index,
            length: conn.states.length,
            state: stateToString(st),
            event: eventToString(conn.events[conn.index])
          }
        }));
      }
    });

    if (window.__pux_handler) {
      window.removeEventListener('pux:state:first', window.__pux_handler);
      window.removeEventListener('pux:state:prev', window.__pux_handler);
      window.removeEventListener('pux:state:next', window.__pux_handler);
      window.removeEventListener('pux:state:last', window.__pux_handler);
    }

    window.__pux_handler = function (ev) {
      if (ev.type === 'pux:state:first') {
        conn.setState = true;
        conn.index = 0;
        app.state.set(conn.states[0]);
      } else if (ev.type === 'pux:state:prev') {
        conn.setState = true;
        if (conn.states[conn.index - 1]) conn.index--;
        app.state.set(conn.states[conn.index]);
      } else if (ev.type === 'pux:state:next') {
        conn.setState = true;
        if (conn.states[conn.index + 1]) conn.index++;
        app.state.set(conn.states[conn.index]);
      } else if (ev.type === 'pux:state:last') {
        conn.setState = true;
        conn.index = conn.states.length - 1;
        app.state.set(conn.states[conn.index]);
      }
    }

    window.addEventListener('pux:state:first', window.__pux_handler);
    window.addEventListener('pux:state:prev', window.__pux_handler);
    window.addEventListener('pux:state:next', window.__pux_handler);
    window.addEventListener('pux:state:last', window.__pux_handler);
  }
}

function eventToString (a) {
  a = a.value0 ? a.value0 : a;

  function toString(a) {
    var name = a.constructor.name.match(/(String|Number|Boolean)/) ? a : a.constructor.name;
    var str = [name];
    if (a.constructor.name === 'Object') {
      return stateToString(a);
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
