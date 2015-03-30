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
  this.lastSentValues = initialValues || {};
};
(function() {
  this.setInput = function(name, value) {
    var jsonValue = JSON.stringify(value);
    if (this.lastSentValues[name] === jsonValue)
      return;
    this.lastSentValues[name] = jsonValue;
    this.target.setInput(name, value);
  };
  this.reset = function(values) {
    values = values || {};
    var strValues = {};
    $.each(values, function(key, value) {
      strValues[key] = JSON.stringify(value);
    });
    this.lastSentValues = strValues;
  };
}).call(InputNoResendDecorator.prototype);

var InputDeferDecorator = function(target) {
  this.target = target;
  this.pendingInput = {};
};
(function() {
  this.setInput = function(name, value) {
    if (/^\./.test(name))
      this.target.setInput(name, value);
    else
      this.pendingInput[name] = value;
  };
  this.submit = function() {
    for (var name in this.pendingInput) {
      if (this.pendingInput.hasOwnProperty(name))
        this.target.setInput(name, this.pendingInput[name]);
    }
  };
}).call(InputDeferDecorator.prototype);

var InputRateDecorator = function(target) {
  this.target = target;
  this.inputRatePolicies = {};
};
(function() {
  this.setInput = function(name, value, immediate) {
    this.$ensureInit(name);
    if (immediate)
      this.inputRatePolicies[name].immediateCall(name, value, immediate);
    else
      this.inputRatePolicies[name].normalCall(name, value, immediate);
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
  this.$doSetInput = function(name, value) {
    this.target.setInput(name, value);
  };
}).call(InputRateDecorator.prototype);
