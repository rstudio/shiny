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
    return function() {
      var self = this;
      if (timerId != null) {
        clearTimeout(timerId);
        timerId = null;
      }
      timerId = setTimeout(function() {
        timerId = null;
        func.call(self);
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

    function throttled() {
      var self = this;
      if (timerId == null) {
        // Haven't seen a call recently. Execute now and
        // start a timer to buffer any subsequent calls.
        timerId = setTimeout(function() {
          // When time expires, clear the timer; and if 
          // there has been a call in the meantime, repeat.
          timerId = null;
          if (executionPending) {
            executionPending = false;
            throttled.call(self);
          }
        }, threshold);
        func.call(self);
      }
      else {
        // Something executed recently. Don't do anything
        executionPending = true;
      }
    };
    return throttled;
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
      for (key in msgObj) {
        this.receiveOutput(key, msgObj[key]);
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


  var LiveTextBinding = function(el) {
    this.el = el;
  };
  (function() {
    this.onValueChange = function(data) {
      $(this.el).text(data);
    };
  }).call(LiveTextBinding.prototype);

  var LivePlotBinding = function(el) {
    this.el = el;
  };
  (function() {
    this.onValueChange = function(data) {
      if (this.el.tagName == 'IMG') {
        this.el.src = data ? data : '';
      }
      else {
        $(this.el).empty();
        if (!data)
          return;
        var img = document.createElement('img');
        img.src = data;
        this.el.appendChild(img);
      }
    };
  }).call(LivePlotBinding.prototype);

  var LiveHTMLBinding = function(el) {
    this.el = el;
  };
  (function() {
    this.onValueChange = function(data) {
      $(this.el).html(data)
    };
  }).call(LiveHTMLBinding.prototype);
  
  $(function() {

    var shinyapp = window.shinyapp = new ShinyApp();

    $('.live-text').each(function() {
      shinyapp.bind(this.id, new LiveTextBinding(this));
    });
    $('.live-plot').each(function() {
      shinyapp.bind(this.id, new LivePlotBinding(this));
    });
    $('.live-html').each(function() {
      shinyapp.bind(this.id, new LiveHTMLBinding(this));
    });



    // Input bindings

    // TODO: This all needs to be refactored to be more modular, extensible

    function elementToValue(el) {
      if (el.type == 'checkbox')
        return el.checked ? true : false;
      else
        return $(el).val();
    }

    // Holds pending changes for deferred submit
    var pendingData = {};

    var deferredSubmit = false;
    $('input[type="submit"], button[type="submit"]').each(function() {
      deferredSubmit = true;
      $(this).click(function(event) {
        event.preventDefault();
        shinyapp.sendInput(pendingData);
      });
    });

    function onInputChange(name, value) {
      if (deferredSubmit)
        pendingData[name] = value;
      else {
        var data = {};
        data[name] = value;
        shinyapp.sendInput(data);
      }
    }

    var initialValues = {};
    $('input[type!="submit"], select').each(function() {
      var input = this;
      var name = input['data-input-id'] || input.name || input.id;
      var value = elementToValue(input);
      // TODO: validate name is non-blank, and no duplicates
      // TODO: If submit button is present, don't send anything
      //   until submit button is pressed
      initialValues[name] = value;
      $(input).each(function() {
        $(this).bind('change keyup input', debounce(500, function() {
          var newValue = elementToValue(input);
          if (value !== newValue) {
            value = newValue;
            onInputChange(name, value);
          }
        }));
      });
    });

    shinyapp.connect(initialValues);
  });
})();
