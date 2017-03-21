var Invoker = function(target, func) {
  this.target = target;
  this.func = func;
};

(function() {
  this.normalCall =
  this.immediateCall = function() {
    this.func.apply(this.target, arguments);
  };
}).call(Invoker.prototype);

var Debouncer = function(target, func, delayMs) {
  this.target = target;
  this.func = func;
  this.delayMs = delayMs;

  this.timerId = null;
  this.args = null;
};

(function() {
  this.normalCall = function() {
    var self = this;

    this.$clearTimer();
    this.args = arguments;

    this.timerId = setTimeout(function() {
      // IE8 doesn't reliably clear timeout, so this additional
      // check is needed
      if (self.timerId === null)
        return;
      self.$clearTimer();
      self.$invoke();
    }, this.delayMs);
  };
  this.immediateCall = function() {
    this.$clearTimer();
    this.args = arguments;
    this.$invoke();
  };
  this.isPending = function() {
    return this.timerId !== null;
  };
  this.$clearTimer = function() {
    if (this.timerId !== null) {
      clearTimeout(this.timerId);
      this.timerId = null;
    }
  };
  this.$invoke = function() {
    this.func.apply(this.target, this.args);
    this.args = null;
  };
}).call(Debouncer.prototype);

var Throttler = function(target, func, delayMs) {
  this.target = target;
  this.func = func;
  this.delayMs = delayMs;

  this.timerId = null;
  this.args = null;
};

(function() {
  this.normalCall = function() {
    var self = this;

    this.args = arguments;
    if (this.timerId === null) {
      this.$invoke();
      this.timerId = setTimeout(function() {
        // IE8 doesn't reliably clear timeout, so this additional
        // check is needed
        if (self.timerId === null)
          return;
        self.$clearTimer();
        if (self.args)
          self.normalCall.apply(self, self.args);
      }, this.delayMs);
    }
  };
  this.immediateCall = function() {
    this.$clearTimer();
    this.args = arguments;
    this.$invoke();
  };
  this.isPending = function() {
    return this.timerId !== null;
  };
  this.$clearTimer = function() {
    if (this.timerId !== null) {
      clearTimeout(this.timerId);
      this.timerId = null;
    }
  };
  this.$invoke = function() {
    this.func.apply(this.target, this.args);
    this.args = null;
  };
}).call(Throttler.prototype);

// Returns a debounced version of the given function.
// Debouncing means that when the function is invoked,
// there is a delay of `threshold` milliseconds before
// it is actually executed, and if the function is
// invoked again before that threshold has elapsed then
// the clock starts over.
//
// For example, if a function is debounced with a
// threshold of 1000ms, then calling it 17 times at
// 900ms intervals will result in a single execution
// of the underlying function, 1000ms after the 17th
// call.
function debounce(threshold, func) {
  var timerId = null;
  var self, args;
  return function() {
    self = this;
    args = arguments;
    if (timerId !== null) {
      clearTimeout(timerId);
      timerId = null;
    }
    timerId = setTimeout(function() {
      // IE8 doesn't reliably clear timeout, so this additional
      // check is needed
      if (timerId === null)
        return;
      timerId = null;
      func.apply(self, args);
    }, threshold);
  };
}

// Returns a throttled version of the given function.
// Throttling means that the underlying function will
// be executed no more than once every `threshold`
// milliseconds.
//
// For example, if a function is throttled with a
// threshold of 1000ms, then calling it 17 times at
// 900ms intervals will result in something like 15
// or 16 executions of the underlying function.
// eslint-disable-next-line no-unused-vars
function throttle(threshold, func) {
  var executionPending = false;
  var timerId = null;
  var self, args;

  function throttled() {
    self = null;
    args = null;
    if (timerId === null) {
      // Haven't seen a call recently. Execute now and
      // start a timer to buffer any subsequent calls.
      timerId = setTimeout(function() {
        // When time expires, clear the timer; and if
        // there has been a call in the meantime, repeat.
        timerId = null;
        if (executionPending) {
          executionPending = false;
          throttled.apply(self, args);
        }
      }, threshold);
      func.apply(this, arguments);
    }
    else {
      // Something executed recently. Don't do anything
      // except set up target/arguments to be called later
      executionPending = true;
      self = this;
      args = arguments;
    }
  }
  return throttled;
}


// Schedules data to be sent to shinyapp at the next setTimeout(0).
// Batches multiple input calls into one websocket message.
var InputBatchSender = function(shinyapp) {
  this.shinyapp = shinyapp;
  this.timerId = null;
  this.pendingData = {};
  this.reentrant = false;
  this.lastChanceCallback = [];
};
(function() {
  this.setInput = function(name, value) {
    var self = this;

    this.pendingData[name] = value;

    if (!this.timerId && !this.reentrant) {
      this.timerId = setTimeout(function() {
        self.reentrant = true;
        try {
          $.each(self.lastChanceCallback, function(i, callback) {
            callback();
          });
          self.timerId = null;
          var currentData = self.pendingData;
          self.pendingData = {};
          self.shinyapp.sendInput(currentData);
        } finally {
          self.reentrant = false;
        }
      }, 0);
    }
  };
}).call(InputBatchSender.prototype);


var InputNoResendDecorator = function(target, initialValues) {
  this.target = target;
  this.lastSentValues = this.reset(initialValues);
};
(function() {
  this.setInput = function(name, value) {
    // Note that opts is not passed to setInput at this stage of the input
    // decorator stack. If in the future this setInput keeps track of opts, it
    // would be best not to store the `el`, because that could prevent it from
    // being GC'd.
    const { name: inputName, inputType: inputType } = splitInputNameType(name);
    const jsonValue = JSON.stringify(value);

    if (this.lastSentValues[inputName] &&
        this.lastSentValues[inputName].jsonValue === jsonValue &&
        this.lastSentValues[inputName].inputType === inputType) {
      return;
    }
    this.lastSentValues[inputName] = { jsonValue, inputType };
    this.target.setInput(name, value);
  };
  this.reset = function(values = {}) {
    // Given an object with flat name-value format:
    //   { x: "abc", "y.shiny.number": 123 }
    // Create an object in cache format and save it:
    //   { x: { jsonValue: '"abc"', inputType: "" },
    //     y: { jsonValue: "123", inputType: "shiny.number" } }
    const cacheValues = {};

    for (let inputName in values) {
      if (values.hasOwnProperty(inputName)) {
        let { name, inputType } = splitInputNameType(inputName);
        cacheValues[name] = {
          jsonValue: JSON.stringify(values[inputName]),
          inputType: inputType
        };
      }
    }

    this.lastSentValues = cacheValues;
  };
}).call(InputNoResendDecorator.prototype);


var InputEventDecorator = function(target) {
  this.target = target;
};
(function() {
  this.setInput = function(name, value, opts) {
    var evt = jQuery.Event("shiny:inputchanged");

    const input = splitInputNameType(name);
    evt.name      = input.name;
    evt.inputType = input.inputType;
    evt.value     = value;
    evt.binding   = opts.binding;
    evt.el        = opts.el;

    $(document).trigger(evt);

    if (!evt.isDefaultPrevented()) {
      name = evt.name;
      if (evt.inputType !== '') name += ':' + evt.inputType;

      // opts aren't passed along to lower levels in the input decorator
      // stack.
      this.target.setInput(name, evt.value);
    }
  };
}).call(InputEventDecorator.prototype);


var InputRateDecorator = function(target) {
  this.target = target;
  this.inputRatePolicies = {};
};
(function() {
  this.setInput = function(name, value, opts) {
    this.$ensureInit(name);

    if (opts.immediate)
      this.inputRatePolicies[name].immediateCall(name, value, opts);
    else
      this.inputRatePolicies[name].normalCall(name, value, opts);
  };
  this.setRatePolicy = function(name, mode, millis) {
    if (mode === 'direct') {
      this.inputRatePolicies[name] = new Invoker(this, this.$doSetInput);
    }
    else if (mode === 'debounce') {
      this.inputRatePolicies[name] = new Debouncer(this, this.$doSetInput, millis);
    }
    else if (mode === 'throttle') {
      this.inputRatePolicies[name] = new Throttler(this, this.$doSetInput, millis);
    }
  };
  this.$ensureInit = function(name) {
    if (!(name in this.inputRatePolicies))
      this.setRatePolicy(name, 'direct');
  };
  this.$doSetInput = function(name, value, opts) {
    this.target.setInput(name, value, opts);
  };
}).call(InputRateDecorator.prototype);


var InputDeferDecorator = function(target) {
  this.target = target;
  this.pendingInput = {};
};
(function() {
  this.setInput = function(name, value, opts) {
    if (/^\./.test(name))
      this.target.setInput(name, value, opts);
    else
      this.pendingInput[name] = { value, opts };
  };
  this.submit = function() {
    for (var name in this.pendingInput) {
      if (this.pendingInput.hasOwnProperty(name)) {
        let input = this.pendingInput[name];
        this.target.setInput(name, input.value, input.opts);
      }
    }
  };
}).call(InputDeferDecorator.prototype);


const InputValidateDecorator = function(target) {
  this.target = target;
};
(function() {
  this.setInput = function(name, value, opts) {
    if (!name)
      throw "Can't set input with empty name.";

    opts = addDefaultInputOpts(opts);

    this.target.setInput(name, value, opts);
  };
}).call(InputValidateDecorator.prototype);


// Merge opts with defaults, and return a new object.
function addDefaultInputOpts(opts) {
  return Object.assign({
    immediate: false,
    binding: null,
    el: null
  }, opts);
}


function splitInputNameType(name) {
  const name2 = name.split(':');
  return {
    name:      name2[0],
    inputType: name2.length > 1 ? name2[1] : ''
  };
}
