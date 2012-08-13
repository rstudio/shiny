(function() {

  var $ = jQuery;

  var exports = window.Shiny = window.Shiny || {};

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
        self.$clearTimer();
        self.$invoke();
      }, this.delayMs);
    };
    this.immediateCall = function() {
      this.$clearTimer();
      this.args = arguments;
      this.$invoke();
    };
    this.$clearTimer = function() {
      if (this.timerId != null) {
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
      if (this.timerId == null) {
        this.$invoke();
        this.timerId = setTimeout(function() {
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
    this.$clearTimer = function() {
      if (this.timerId != null) {
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
      if (timerId != null) {
        clearTimeout(timerId);
        timerId = null;
      }
      timerId = setTimeout(function() {
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
      if (timerId == null) {
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
    };
    return throttled;
  }

  // Immediately sends data to shinyapp
  var InputSender = function(shinyapp) {
    this.shinyapp = shinyapp;
  };
  (function() {
    this.setInput = function(name, value) {
      var data = {};
      data[name] = value;
      shinyapp.sendInput(data);
    };
  }).call(InputSender.prototype);

  // Schedules data to be sent to shinyapp at the next setTimeout(0).
  // Batches multiple input calls into one websocket message.
  var InputBatchSender = function(shinyapp) {
    this.shinyapp = shinyapp;
    this.timerId = null;
    this.pendingData = {};
  };
  (function() {
    this.setInput = function(name, value) {
      var self = this;

      this.pendingData[name] = value;
      if (!this.timerId) {
        this.timerId = setTimeout(function() {
          self.timerId = null;
          var currentData = self.pendingData;
          self.pendingData = {};
          self.shinyapp.sendInput(currentData);
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
    this.reset = function() {
      this.lastSentValues = {};
    };
  }).call(InputNoResendDecorator.prototype);

  var InputDeferDecorator = function(target) {
    this.target = target;
    this.pendingInput = {};
  };
  (function() {
    this.setInput = function(name, value) {
      this.pendingInput[name] = value;
    };
    this.submit = function() {
      for (var name in this.pendingInput) {
        this.target.setInput(this.pendingInput[name]);
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
      if (mode == 'direct') {
        this.inputRatePolicies[name] = new Invoker(this, this.$doSetInput);
      }
      else if (mode == 'debounce') {
        this.inputRatePolicies[name] = new Debouncer(this, this.$doSetInput, millis);
      }
      else if (mode == 'throttle') {
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


  var ShinyApp = function() {
    this.$socket = null;
    this.$bindings = {};
    this.$values = {};
    this.$pendingMessages = [];
  };

  (function() {

    this.connect = function(initialInput) {
      if (this.$socket)
        throw "Connect was already called on this application object";

      this.$socket = this.createSocket();
      this.$initialInput = initialInput;
    };

    this.createSocket = function () {
      var self = this;

      var socket = new WebSocket('ws://' + window.location.host, 'shiny');
      socket.onopen = function() {
        socket.send(JSON.stringify({
          method: 'init',
          data: self.$initialInput
        }))

        while (self.$pendingMessages.length) {
          var msg = self.$pendingMessages.shift();
          socket.send(msg);
        }
      };
      socket.onmessage = function(e) {
        self.dispatchMessage(e.data);
      };
      socket.onclose = function() {
        $(document.body).addClass('disconnected');
      };
      return socket;
    };

    this.sendInput = function(values) {
      var msg = JSON.stringify({
        method: 'update',
        data: values
      });

      if (this.$socket.readyState == WebSocket.CONNECTING) {
        this.$pendingMessages.push(msg);
      }
      else {
        this.$socket.send(msg);
      }
    };

    this.receiveError = function(name, error) {
      this.$values[name] = null;

      var binding = this.$bindings[name];
      if (binding && binding.onValueError) {
        binding.onValueError(error);
      }
    }

    this.receiveOutput = function(name, value) {
      var oldValue = this.$values[name];
      this.$values[name] = value;
      if (oldValue === value)
        return;

      var binding = this.$bindings[name];
      if (binding) {
        binding.onValueChange(value);
      }

      return value;
    };

    this.dispatchMessage = function(msg) {
      var msgObj = JSON.parse(msg);
      if (msgObj.values) {
        for (name in this.$bindings)
          this.$bindings[name].showProgress(false);
      }
      for (key in msgObj.errors) {
        this.receiveError(key, msgObj.errors[key]);
      }
      for (key in msgObj.values) {
        this.receiveOutput(key, msgObj.values[key]);
      }
      if (msgObj.progress) {
        for (var i = 0; i < msgObj.progress.length; i++) {
          var key = msgObj.progress[i];
          var binding = this.$bindings[key];
          if (binding && binding.showProgress) {
            binding.showProgress(true);
          }
        }
      }
    };

    this.bind = function(id, binding) {
      if (!id)
        throw "Can't bind an element with no ID";
      if (this.$bindings[id])
        throw "Duplicate binding for ID " + id;
      this.$bindings[id] = binding;
      return binding;
    };
  }).call(ShinyApp.prototype);


  var LiveBinding = function(el) {
    this.el = $(el);
  };
  (function() {
    this.onValueChange = function(data) {
      this.clearError();
      this.renderValue(data);
    };
    this.onValueError = function(err) {
      this.renderError(err);
    };
    this.renderError = function(err) {
      this.el.text('ERROR: ' + err.message);
      this.el.addClass('shiny-output-error');
    };
    this.clearError = function() {
      this.el.removeClass('shiny-output-error');
    };
    this.showProgress = function(show) {
      var RECALC_CLASS = 'recalculating';
      if (show)
        this.el.addClass(RECALC_CLASS);
      else
        this.el.removeClass(RECALC_CLASS);
    };
  }).call(LiveBinding.prototype);


  var LiveTextBinding = function(el) {
    LiveBinding.call(this, el);
  };
  (function() {
    this.renderValue = function(data) {
      this.el.text(data);
    };
  }).call(LiveTextBinding.prototype);
  LiveTextBinding.prototype = $.extend({}, LiveBinding.prototype, LiveTextBinding.prototype);

  var LivePlotBinding = function(el) {
    LiveBinding.call(this, el);
  };
  (function() {
    this.renderValue = function(data) {
      this.el.empty();
      if (!data)
        return;
      var img = document.createElement('img');
      img.src = data;
      this.el.append(img);
    };
  }).call(LivePlotBinding.prototype);
  LivePlotBinding.prototype = $.extend({}, LiveBinding.prototype, LivePlotBinding.prototype);

  var LiveHTMLBinding = function(el) {
    LiveBinding.call(this, el);
  };
  (function() {
    this.renderValue = function(data) {
      this.el.html(data)
    };
  }).call(LiveHTMLBinding.prototype);
  LiveHTMLBinding.prototype = $.extend({}, LiveBinding.prototype, LiveHTMLBinding.prototype);


  var inputBindings = [];
  var registerInputBinding = function(binding, priority) {
    inputBindings.push({binding: binding, priority: priority});
  }
  exports.registerInputBinding = registerInputBinding;
  
    
  var InputBinding = exports.InputBinding = function() {
  };
  
  (function() {
  
    // Returns a jQuery object or element array that contains the
    // descendants of scope that match this binding
    this.find = function(scope) { throw "Not implemented"; };
  
    this.getId = function(el) {
      return el['data-input-id'] || el.id;
    };
  
    this.getValue = function(el) { throw "Not implemented"; };
    this.subscribe = function(el) { };
    this.unsubscribe = function(el) { };
    
    this.getRatePolicy = function() { return null; };
  
  }).call(InputBinding.prototype);
  
  
  
  // Text input
  var textInputBinding = new InputBinding();
  $.extend(textInputBinding, {
    find: function(scope) {
      return $(scope).find('input[type="text"]');
    },
    getId: function(el) {
      return InputBinding.prototype.getId.call(this, el) || el.name;
    },
    getValue: function(el) {
      return el.value;
    },
    setValue: function(el, value) {
      el.value = value;
    },
    subscribe: function(el, callback) {
      var self = this;
      $(el).on('keyup.textInputBinding input.textInputBinding', function(event) {
        callback(self, el, true);
      });
      $(el).on('change.textInputBinding', function(event) {
        callback(self, el, false);
      });
    },
    unsubscribe: function(el) {
      $(el).off('.textInputBinding');
    },
    getRatePolicy: function() {
      return {
        policy: 'debounce',
        delay: 250
      };
    }
  });
  registerInputBinding(textInputBinding, 0);
  
  
  var numberInputBinding = {};
  $.extend(numberInputBinding, textInputBinding, {
    find: function(scope) {
      return $(scope).find('input[type="number"]');
    },
    getValue: function(el) {
      var numberVal = $(el).val();
      if (!isNaN(numberVal))
        return +numberVal;
      else
        return numberVal;
    }
  });
  registerInputBinding(numberInputBinding, 0);


  var sliderInputBinding = {};
  $.extend(sliderInputBinding, numberInputBinding, {
    find: function(scope) {
      // Check if jslider plugin is loaded
      if (!$.fn.slider)
        return [];

      var sliders = $(scope).find('input.jslider');
      sliders.slider();
      return sliders;
    },
    getValue: function(el) {
      var sliderVal = $(el).val();
      if (/;/.test(sliderVal)) {
        var chunks = sliderVal.split(/;/, 2);
        return [+chunks[0], +chunks[1]];
      }
      else {
        return +sliderVal;
      }
    },
    setValue: function(el, val) {
      // TODO: implement
    },
    subscribe: function(el, callback) {
      var self = this;
      $(el).on('change.inputBinding', function(event) {
        callback(self, el, !$(el).data('animating'));
      });
    },
    unsubscribe: function(el) {
      $(el).off('.inputBinding');
    },
    getRatePolicy: function() {
      return {
        policy: 'debounce',
        delay: 250
      };
    }
  });
  registerInputBinding(sliderInputBinding, 0);
  
  
  // Select input
  var selectInputBinding = new InputBinding();
  $.extend(selectInputBinding, {
    find: function(scope) {
      return scope.find('select');
    },
    getId: function(el) {
      return InputBinding.prototype.getId.call(this, el) || el.name;
    },
    getValue: function(el) {
      return $(el).val();
    },
    setValue: function(el, value) {
      $(el).val(value);
    },
    subscribe: function(el, callback) {
      var self = this;
      $(el).on('change.selectInputBinding', function(event) {
        callback(self, el);
      });
    },
    unsubscribe: function(el) {
      $(el).off('.selectInputBinding');
    }
  });
  registerInputBinding(selectInputBinding, 0);

  

  $(function() {

    var shinyapp = exports.shinyapp = new ShinyApp();

    $('.shiny-text-output').each(function() {
      shinyapp.bind(this.id, new LiveTextBinding(this));
    });
    $('.shiny-plot-output').each(function() {
      shinyapp.bind(this.id, new LivePlotBinding(this));
    });
    $('.shiny-html-output').each(function() {
      shinyapp.bind(this.id, new LiveHTMLBinding(this));
    });



    // Input bindings

    // TODO: This all needs to be refactored to be more modular, extensible

    function elementToValue(el) {
      if (el.type == 'checkbox' || el.type == 'radio')
        return el.checked ? true : false;
      else
        return $(el).val();
    }

    // Holds pending changes for deferred submit
    var pendingData = {};
    // Holds last sent data, so we know when we don't have to send again
    var lastSentData = {};

    var inputs = new InputNoResendDecorator(new InputBatchSender(shinyapp));
    var inputsRate = new InputRateDecorator(inputs);
    var inputsDefer = new InputDeferDecorator(inputs);

    inputs = inputsRate;
    $('input[type="submit"], button[type="submit"]').each(function() {
      inputs = inputsDefer;
      $(this).click(function(event) {
        event.preventDefault();
        inputsDefer.submit();
      });
    });

    exports.onInputChange = function(name, value) {
      inputs.setInput(name, value);
    };

    var boundInputs = {};
    
    function valueChangeCallback(binding, el, allowDeferred) {
      var id = binding.getId(el);
      var el = binding.getValue(el);
      if (id)
        inputs.setInput(id, el, !allowDeferred);
    }
    
    function bindInputs(scope) {
      
      scope = $(scope);
      
      var currentValues = {};
    
      for (var i = 0; i < inputBindings.length; i++) {
        var binding = inputBindings[i].binding;
        var matches = binding.find(scope) || [];
        for (var j = 0; j < matches.length; j++) {
          var el = matches[j];
          var id = binding.getId(el);
          // Check if ID is falsy, or if already bound
          if (!id || boundInputs[id])
            continue;
    
          currentValues[id] = binding.getValue(el);
          binding.subscribe(el, valueChangeCallback);
          var ratePolicy = binding.getRatePolicy();
          if (ratePolicy != null) {
            inputsRate.setRatePolicy(
              id,
              ratePolicy.policy,
              ratePolicy.delay);
          }
    
          boundInputs[id] = {
            binding: binding,
            node: el
          };
        }
      }
    
      return currentValues;
    }
    
    initialValues = bindInputs(document);

    function getMultiValue(input, exclusiveValue) {
      if (!input.name)
        return null;

      els = $(
        'input:checked' + 
        '[type="' + input.type + '"]' + 
        '[name="' + input.name + '"]');
      var values = els.map(function() { return this.value; }).get();
      if (exclusiveValue) {
        if (values.length > 0)
          return values[0];
        else
          return null;
      }
      else {
        return values;
      }
    }

    function configureMultiInput(selector, exclusiveValue) {
      $(document).on('change input', selector, function() {
        if (this.name) {
          inputs.setInput(this.name, getMultiValue(this, exclusiveValue));
        }
        if (!exclusiveValue) {
          var id = this['data-input-id'] || this.id;
          if (id) {
            inputs.setInput(id, elementToValue(this));
          }
        }
      });
      $(selector).each(function() {
        if (this.name) {
          initialValues[this.name] = getMultiValue(this, exclusiveValue);
        }
        if (!exclusiveValue) {
          var id = this['data-input-id'] || this.id;
          if (id) {
            initialValues[id] = elementToValue(this);
          }
        }
      });
    }

    configureMultiInput('input[type="checkbox"]', false);
    configureMultiInput('input[type="radio"]', true);

    $('.shiny-plot-output').each(function() {
      var width = this.offsetWidth;
      var height = this.offsetHeight;
      initialValues['.shinyout_' + this.id + '_width'] = width;
      initialValues['.shinyout_' + this.id + '_height'] = height;
      var self = this;
      $(window).resize(debounce(500, function() {
        if (self.offsetWidth != width) {
          width = self.offsetWidth;
          inputs.setInput('.shinyout_' + self.id + '_width', width);
        }
        if (self.offsetHeight != height) {
          height = self.offsetHeight;
          inputs.setInput('.shinyout_' + self.id + '_height', height);
        }
      }));
    });

    shinyapp.connect(initialValues);
    lastSentData = initialValues;
  });

  $(document).on('click', '.slider-animate-button', function(evt) {
    evt.preventDefault();
    var self = $(this);
    var target = $('#' + self.attr('data-target-id'));
    var slider = target.slider();
    var startLabel = 'Play';
    var stopLabel = 'Pause';
    var loop = self.attr('data-loop') !== undefined &&
               !/^\s*false\s*$/i.test(self.attr('data-loop'))
    var animInterval = self.attr('data-interval');
    if (isNaN(animInterval))
      animInterval = 1500;
    else
      animInterval = +animInterval;

    if (!target.data('animTimer')) {
      // If we're currently at the end, restart
      if (!slider.canStepNext())
        slider.resetToStart();

      var timer = setInterval(function() {
        if (loop && !slider.canStepNext()) {
          slider.resetToStart();
        }
        else {

          slider.stepNext();

          if (!loop && !slider.canStepNext()) {
            self.click(); // stop the animation
          }
        }
      }, animInterval);
      target.data('animTimer', timer);
      self.attr('title', stopLabel);
      self.addClass('playing');
      target.data('animating', true);
    }
    else {
      clearTimeout(target.data('animTimer'));
      target.removeData('animTimer');
      self.attr('title', startLabel);
      self.removeClass('playing');
      target.removeData('animating');
    }
  });

})();
