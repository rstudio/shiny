(function() {

  var $ = jQuery;

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

  function keyedFunc(delay, keyArgNum, func, modFunc) {
    if (delay == 0)
      return func;

    var keyedFuncs = {};
    return function() {
      var key = keyArgNum == -1 ? this : arguments[keyArgNum];
      if (!keyedFuncs[key])
        keyedFuncs[key] = modFunc(delay, func);
      keyedFuncs[key].apply(this, arguments);
    };
  }

  // Behaves similarly to the debounce function, except that
  // internally it will create a separate debounce function
  // for each distinct value of one of the arguments. You
  // specify which argument that is using keyArgNum (-1 means
  // to use 'this').
  //
  // For example:
  //
  // function printNameValue(name, value) {
  //   console.log(name + '=' + value);
  // }
  // var debouncedPNV = keyedDebounce(500, 0, printNameValue);
  // debouncedPNV('foo', 10);
  // debouncedPNV('bar', 10);
  // debouncedPNV('bar', 20);
  //
  // will print:
  // foo=10
  // bar=20
  function keyedDebounce(delay, keyArgNum, func) {
    return keyedFunc(delay, keyArgNum, func, debounce);
  }

  // Same as keyedDebounce, but for throttling
  function keyedThrottle(delay, keyArgNum, func) {
    return keyedFunc(delay, keyArgNum, func, throttle);
  }


  var ShinyApp = window.ShinyApp = function() {
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
  $.extend(LiveTextBinding.prototype, LiveBinding.prototype);

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
  $.extend(LivePlotBinding.prototype, LiveBinding.prototype);

  var LiveHTMLBinding = function(el) {
    LiveBinding.call(this, el);
  };
  (function() {
    this.renderValue = function(data) {
      this.el.html(data)
    };
  }).call(LiveHTMLBinding.prototype);
  $.extend(LiveHTMLBinding.prototype, LiveBinding.prototype);


  $(function() {

    var shinyapp = window.shinyapp = new ShinyApp();

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
      else if ($(el).attr('type') === 'slider') {
        var sliderVal = $(el).val();
        if (/;/.test(sliderVal)) {
          var chunks = sliderVal.split(/;/, 2);
          return [+chunks[0], +chunks[1]];
        }
        else {
          return +sliderVal;
        }
      }
      else if ($(el).attr('type') === 'number') {
        var numberVal = $(el).val();
        if (!isNaN(numberVal))
          return +numberVal;
        else
          return numberVal;
      }
      else
        return $(el).val();
    }

    // Holds pending changes for deferred submit
    var pendingData = {};
    // Holds last sent data, so we know when we don't have to send again
    var lastSentData = {};

    var deferredSubmit = false;
    $('input[type="submit"], button[type="submit"]').each(function() {
      deferredSubmit = true;
      $(this).click(function(event) {
        event.preventDefault();
        shinyapp.sendInput(pendingData);
        $.extend(lastSentData, pendingData);
        pendingData = {};
      });
    });

    function onInputChange(name, value) {
      if (lastSentData[name] === value) {
        delete pendingData[name];
        return;
      }

      if (deferredSubmit)
        pendingData[name] = value;
      else {
        var data = {};
        data[name] = value;
        shinyapp.sendInput(data);
        $.extend(lastSentData, data);
      }
    }

    var debouncedInputChange = keyedDebounce(
      deferredSubmit ? 0 : 250, 
      0,
      onInputChange);

    var inputSelector = ':input:not([type="submit"], [type="checkbox"], [type="radio"])';
    var initialValues = {};
    $(document).on('change keyup input', inputSelector, function() {
      var input = this;
      var name = input['data-input-id'] || input.name || input.id;
      var value = elementToValue(input);
      if (input.type == 'text')
        debouncedInputChange(name, value);
      else
        onInputChange(name, value);
    });
    $(inputSelector).each(function() {
      var input = this;
      var name = input['data-input-id'] || input.name || input.id;
      var value = elementToValue(input);
      // TODO: validate name is non-blank, and no duplicates
      // TODO: If submit button is present, don't send anything
      //   until submit button is pressed
      initialValues[name] = value;
    });

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
          onInputChange(this.name, getMultiValue(this, exclusiveValue));
        }
        var id = this['data-input-id'] || this.id;
        if (id) {
          onInputChange(id, elementToValue(this));
        }
      });
      $(selector).each(function() {
        if (this.name) {
          initialValues[this.name] = getMultiValue(this, exclusiveValue);
        }
        var id = this['data-input-id'] || this.id;
        if (id) {
          initialValues[id] = elementToValue(this);
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
          onInputChange('.shinyout_' + self.id + '_width', width);
        }
        if (self.offsetHeight != height) {
          height = self.offsetHeight;
          onInputChange('.shinyout_' + self.id + '_height', height);
        }
      }));
    });

    shinyapp.connect(initialValues);
    lastSentData = initialValues;
  });
})();
