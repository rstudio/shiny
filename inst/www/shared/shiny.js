/*jshint browser:true, jquery:true, strict:false, curly:false, indent:2*/

(function() {
  var $ = jQuery;

  var exports = window.Shiny = window.Shiny || {};

  // For easy handling of Qt quirks using CSS
  if (/\bQt\//.test(window.navigator.userAgent)) {
    $(document.documentElement).addClass('qt');
  }

  $(document).on('submit', 'form:not([action])', function(e) {
    e.preventDefault();
  });

  // Escape jQuery selector metacharacters: !"#$%&'()*+,./:;<=>?@[\]^`{|}~
  var $escape = exports.$escape = function(val) {
    return val.replace(/([!"#$%&'()*+,.\/:;<=>?@\[\\\]^`{|}~])/g, '\\$1');
  };

  function randomId() {
    return Math.floor(0x100000000 + (Math.random() * 0xF00000000)).toString(16);
  }

  // A wrapper for getComputedStyle that is compatible with older browsers.
  // This is significantly faster than jQuery's .css() function.
  function getStyle(el, styleProp) {
    var x;
    if (el.currentStyle)
      x = el.currentStyle[styleProp];
    else if (window.getComputedStyle)
      x = document.defaultView.getComputedStyle(el, null)
          .getPropertyValue(styleProp);
    return x;
  }

  // Convert a number to a string with leading zeros
  function padZeros(n, digits) {
    var str = n.toString();
    while (str.length < digits)
      str = "0" + str;
    return str;
  }

  // Take a string with format "YYYY-MM-DD" and return a Date object.
  // IE8 and QTWebKit don't support YYYY-MM-DD, but they support YYYY/MM/DD
  function parseDate(dateString) {
    var date = new Date(dateString);
    if (isNaN(date))
      date = new Date(dateString.replace(/-/g, "/"));
    return date;
  }

  function slice(blob, start, end) {
    if (blob.slice)
      return blob.slice(start, end);
    if (blob.mozSlice)
      return blob.mozSlice(start, end);
    if (blob.webkitSlice)
      return blob.webkitSlice(start, end);
    throw "Blob doesn't support slice";
  }

  var _BlobBuilder = window.BlobBuilder || window.WebKitBlobBuilder ||
      window.MozBlobBuilder || window.MSBlobBuilder;

  function makeBlob(parts) {

    // Browser compatibility is a mess right now. The code as written works in
    // a variety of modern browsers, but sadly gives a deprecation warning
    // message on the console in current versions (as of this writing) of
    // Chrome.

    // Safari 6.0 (8536.25) on Mac OS X 10.8.1:
    // Has Blob constructor but it doesn't work with ArrayBufferView args

    // Google Chrome 21.0.1180.81 on Xubuntu 12.04:
    // Has Blob constructor, accepts ArrayBufferView args, accepts ArrayBuffer
    // but with a deprecation warning message

    // Firefox 15.0 on Xubuntu 12.04:
    // Has Blob constructor, accepts both ArrayBuffer and ArrayBufferView args

    // Chromium 18.0.1025.168 (Developer Build 134367 Linux) on Xubuntu 12.04:
    // No Blob constructor. Has WebKitBlobBuilder.

    try {
      return new Blob(parts);
    }
    catch (e) {
      var blobBuilder = new _BlobBuilder();
      $.each(parts, function(i, part) {
        blobBuilder.append(part);
      });
      return blobBuilder.getBlob();
    }
  }

  function pixelRatio() {
    if (window.devicePixelRatio) {
      return window.devicePixelRatio;
    } else {
      return 1;
    }
  }

  // Takes a string expression and returns a function that takes an argument.
  //
  // When the function is executed, it will evaluate that expression using
  // "with" on the argument value, and return the result.
  function scopeExprToFunc(expr) {
    /*jshint evil: true */
    var func = new Function("with (this) {return (" + expr + ");}");
    return function(scope) {
      return func.call(scope);
    };
  }


  // =========================================================================
  // Input rate stuff
  // =========================================================================

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


  // We need a stable sorting algorithm for ordering
  // bindings by priority and insertion order.
  function mergeSort(list, sortfunc) {
    function merge(sortfunc, a, b) {
      var ia = 0;
      var ib = 0;
      var sorted = [];
      while (ia < a.length && ib < b.length) {
        if (sortfunc(a[ia], b[ib]) <= 0) {
          sorted.push(a[ia++]);
        }
        else {
          sorted.push(b[ib++]);
        }
      }
      while (ia < a.length)
        sorted.push(a[ia++]);
      while (ib < b.length)
        sorted.push(b[ib++]);
      return sorted;
    }

    // Don't mutate list argument
    list = list.slice(0);

    for (var chunkSize = 1; chunkSize < list.length; chunkSize *= 2) {
      for (var i = 0; i < list.length; i += chunkSize * 2) {
        var listA = list.slice(i, i + chunkSize);
        var listB = list.slice(i + chunkSize, i + chunkSize * 2);
        var merged = merge(sortfunc, listA, listB);
        var args = [i, merged.length];
        Array.prototype.push.apply(args, merged);
        Array.prototype.splice.apply(list, args);
      }
    }

    return list;
  }


  // =========================================================================
  // ShinyApp
  // =========================================================================
  var ShinyApp = function() {
    this.$socket = null;

    // Cached input values
    this.$inputValues = {};

    // Output bindings
    this.$bindings = {};

    // Cached values/errors
    this.$values = {};
    this.$errors = {};

    // Conditional bindings (show/hide element based on expression)
    this.$conditionals = {};

    this.$pendingMessages = [];
    this.$activeRequests = {};
    this.$nextRequestId = 0;
  };

  (function() {

    this.connect = function(initialInput) {
      if (this.$socket)
        throw "Connect was already called on this application object";

      $.extend(initialInput, {
        // IE8 and IE9 have some limitations with data URIs
        ".clientdata_allowDataUriScheme": typeof WebSocket !== 'undefined'
      });

      this.$socket = this.createSocket();
      this.$initialInput = initialInput;
      $.extend(this.$inputValues, initialInput);

      this.$updateConditionals();
    };

    this.isConnected = function() {
      return !!this.$socket;
    };

    this.createSocket = function () {
      var self = this;

      var createSocketFunc = exports.createSocket || function() {
        var protocol = 'ws:';
        if (window.location.protocol === 'https:')
          protocol = 'wss:';

        var defaultPath = window.location.pathname;
        if (!/\/$/.test(defaultPath))
          defaultPath += '/';
        defaultPath += 'websocket/';

        var ws = new WebSocket(protocol + '//' + window.location.host + defaultPath);
        ws.binaryType = 'arraybuffer';
        return ws;
      };

      var socket = createSocketFunc();
      socket.onopen = function() {
        socket.send(JSON.stringify({
          method: 'init',
          data: self.$initialInput
        }));

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
        self.$notifyDisconnected();
      };
      return socket;
    };

    this.sendInput = function(values) {
      var msg = JSON.stringify({
        method: 'update',
        data: values
      });

      this.$sendMsg(msg);

      $.extend(this.$inputValues, values);
      this.$updateConditionals();
    };

    this.$notifyDisconnected = function() {

      // function to normalize hostnames
      var normalize = function(hostname) {
        if (hostname == "127.0.0.1")
          return "localhost";
        else
          return hostname;
      };

      // Send a 'disconnected' message to parent if we are on the same domin
      var parentUrl = (parent !== window) ? document.referrer : null;
      if (parentUrl) {
        // parse the parent href
        var a = document.createElement('a');
        a.href = parentUrl;

        // post the disconnected message if the hostnames are the same
        if (normalize(a.hostname) == normalize(window.location.hostname)) {
          var protocol = a.protocol.replace(':',''); // browser compatability
          var origin = protocol + '://' + a.hostname;
          if (a.port)
            origin = origin + ':' + a.port;
          parent.postMessage('disconnected', origin);
        }
      }
    };

    // NB: Including blobs will cause IE to break!
    // TODO: Make blobs work with Internet Explorer
    //
    // Websocket messages are normally one-way--i.e. the client passes a
    // message to the server but there is no way for the server to provide
    // a response to that specific message. makeRequest provides a way to
    // do asynchronous RPC over websocket. Each request has a method name
    // and arguments, plus optionally one or more binary blobs can be
    // included as well. The request is tagged with a unique number that
    // the server will use to label the corresponding response.
    //
    // @param method A string that tells the server what logic to run.
    // @param args An array of objects that should also be passed to the
    //   server in JSON-ified form.
    // @param onSuccess A function that will be called back if the server
    //   responds with success. If the server provides a value in the
    //   response, the function will be called with it as the only argument.
    // @param onError A function that will be called back if the server
    //   responds with error, or if the request fails for any other reason.
    //   The parameter to onError will be a string describing the error.
    // @param blobs Optionally, an array of Blob, ArrayBuffer, or string
    //   objects that will be made available to the server as part of the
    //   request. Strings will be encoded using UTF-8.
    this.makeRequest = function(method, args, onSuccess, onError, blobs) {
      var requestId = this.$nextRequestId;
      while (this.$activeRequests[requestId]) {
        requestId = (requestId + 1) % 1000000000;
      }
      this.$nextRequestId = requestId + 1;

      this.$activeRequests[requestId] = {
        onSuccess: onSuccess,
        onError: onError
      };

      var msg = JSON.stringify({
        method: method,
        args: args,
        tag: requestId
      });

      if (blobs) {
        // We have binary data to transfer; form a different kind of packet.
        // Start with a 4-byte signature, then for each blob, emit 4 bytes for
        // the length followed by the blob. The json payload is UTF-8 encoded
        // and used as the first blob.

        var uint32_to_buf = function(val) {
          var buffer = new ArrayBuffer(4);
          var view = new DataView(buffer);
          view.setUint32(0, val, true); // little-endian
          return buffer;
        };

        var payload = [];
        payload.push(uint32_to_buf(0x01020202)); // signature

        var jsonBuf = makeBlob([msg]);
        payload.push(uint32_to_buf(jsonBuf.size));
        payload.push(jsonBuf);

        for (var i = 0; i < blobs.length; i++) {
          payload.push(uint32_to_buf(blobs[i].byteLength || blobs[i].size || 0));
          payload.push(blobs[i]);
        }

        msg = makeBlob(payload);
      }

      this.$sendMsg(msg);
    };

    this.$sendMsg = function(msg) {
      if (!this.$socket.readyState) {
        this.$pendingMessages.push(msg);
      }
      else {
        this.$socket.send(msg);
      }
    };

    this.receiveError = function(name, error) {
      if (this.$errors[name] === error)
        return;

      this.$errors[name] = error;
      delete this.$values[name];

      var binding = this.$bindings[name];
      if (binding && binding.onValueError) {
        binding.onValueError(error);
      }
    };

    this.receiveOutput = function(name, value) {
      if (this.$values[name] === value)
        return;

      this.$values[name] = value;
      delete this.$errors[name];

      var binding = this.$bindings[name];
      if (binding) {
        binding.onValueChange(value);
      }

      return value;
    };

    this.bindOutput = function(id, binding) {
      if (!id)
        throw "Can't bind an element with no ID";
      if (this.$bindings[id])
        throw "Duplicate binding for ID " + id;
      this.$bindings[id] = binding;

      if (this.$values[id] !== undefined)
        binding.onValueChange(this.$values[id]);
      else if (this.$errors[id] !== undefined)
        binding.onValueError(this.$errors[id]);

      return binding;
    };

    this.unbindOutput = function(id, binding) {
      if (this.$bindings[id] === binding) {
        delete this.$bindings[id];
        return true;
      }
      else {
        return false;
      }
    };

    this.$updateConditionals = function() {
      var inputs = {};

      // Input keys use "name:type" format; we don't want the user to
      // have to know about the type suffix when referring to inputs.
      for (var name in this.$inputValues) {
        if (this.$inputValues.hasOwnProperty(name)) {
          var shortName = name.replace(/:.*/, '');
          inputs[shortName] = this.$inputValues[name];
        }
      }

      var scope = {input: inputs, output: this.$values};

      var triggerShown  = function() { $(this).trigger('shown'); };
      var triggerHidden = function() { $(this).trigger('hidden'); };

      var conditionals = $(document).find('[data-display-if]');
      for (var i = 0; i < conditionals.length; i++) {
        var el = $(conditionals[i]);
        var condFunc = el.data('data-display-if-func');

        if (!condFunc) {
          var condExpr = el.attr('data-display-if');
          condFunc = scopeExprToFunc(condExpr);
          el.data('data-display-if-func', condFunc);
        }

        if (condFunc(scope)) {
          el.trigger('show');
          el.show(0, triggerShown);
        }
        else {
          el.trigger('hide');
          el.hide(0, triggerHidden);
        }
      }
    };

    // Message handler management functions =================================

    // Records insertion order of handlers. Maps number to name. This is so
    // we can dispatch messages to handlers in the order that handlers were
    // added.
    var messageHandlerOrder = [];
    // Keep track of handlers by name. Maps name to handler function.
    var messageHandlers = {};

    // Two categories of message handlers: those that are from Shiny, and those
    // that are added by the user. The Shiny ones handle messages in
    // msgObj.values, msgObj.errors, and so on. The user ones handle messages
    // in msgObj.custom.foo and msgObj.custom.bar.
    var customMessageHandlerOrder = [];
    var customMessageHandlers = {};

    // Adds Shiny (internal) message handler
    function addMessageHandler(type, handler) {
      if (messageHandlers[type]) {
        throw('handler for message of type "' + type + '" already added.');
      }
      if (typeof(handler) !== 'function') {
        throw('handler must be a function.');
      }
      if (handler.length !== 1) {
        throw('handler must be a function that takes one argument.');
      }

      messageHandlerOrder.push(type);
      messageHandlers[type] = handler;
    }

    // Adds custom message handler - this one is exposed to the user
    function addCustomMessageHandler(type, handler) {
      if (customMessageHandlers[type]) {
        throw('handler for message of type "' + type + '" already added.');
      }
      if (typeof(handler) !== 'function') {
        throw('handler must be a function.');
      }
      if (handler.length !== 1) {
        throw('handler must be a function that takes one argument.');
      }

      customMessageHandlerOrder.push(type);
      customMessageHandlers[type] = handler;
    }

    exports.addCustomMessageHandler = addCustomMessageHandler;

    this.dispatchMessage = function(msg) {
      var msgObj = JSON.parse(msg);

      // Send msgObj.foo and msgObj.bar to appropriate handlers
      this._sendMessagesToHandlers(msgObj, messageHandlers, messageHandlerOrder);

      this.$updateConditionals();
    };


    // A function for sending messages to the appropriate handlers.
    // - msgObj: the object containing messages, with format {msgObj.foo, msObj.bar
    this._sendMessagesToHandlers = function(msgObj, handlers, handlerOrder) {
      // Dispatch messages to handlers, if handler is present
      for (var i = 0; i < handlerOrder.length; i++) {
        var msgType = handlerOrder[i];

        if (msgObj[msgType]) {
          // Execute each handler with 'this' referring to the present value of
          // 'this'
          handlers[msgType].call(this, msgObj[msgType]);
        }
      }
    };

    // Message handlers =====================================================

    addMessageHandler('values', function(message) {
      $(document.documentElement).removeClass('shiny-busy');
      for (var name in this.$bindings) {
        if (this.$bindings.hasOwnProperty(name))
          this.$bindings[name].showProgress(false);
      }

      for (var key in message) {
        if (message.hasOwnProperty(key))
          this.receiveOutput(key, message[key]);
      }
    });

    addMessageHandler('errors', function(message) {
      for (var key in message) {
        if (message.hasOwnProperty(key))
          this.receiveError(key, message[key]);
      }
    });

    addMessageHandler('inputMessages', function(message) {
      // inputMessages should be an array
      for (var i = 0; i < message.length; i++) {
        var $obj = $('.shiny-bound-input#' + $escape(message[i].id));
        var inputBinding = $obj.data('shiny-input-binding');

        // Dispatch the message to the appropriate input object
        if ($obj.length > 0) {
          inputBinding.receiveMessage($obj[0], message[i].message);
        }
      }
    });

    addMessageHandler('javascript', function(message) {
      /*jshint evil: true */
      eval(message);
    });

    addMessageHandler('console', function(message) {
      for (var i = 0; i < message.length; i++) {
        if (console.log)
          console.log(message[i]);
      }
    });

    addMessageHandler('progress', function(message) {
      $(document.documentElement).addClass('shiny-busy');
      for (var i = 0; i < message.length; i++) {
        var key = message[i];
        var binding = this.$bindings[key];
        if (binding && binding.showProgress) {
          binding.showProgress(true);
        }
      }
    });

    addMessageHandler('response', function(message) {
      var requestId = message.tag;
      var request = this.$activeRequests[requestId];
      if (request) {
        delete this.$activeRequests[requestId];
        if ('value' in message)
          request.onSuccess(message.value);
        else
          request.onError(message.error);
      }
    });

    addMessageHandler('custom', function(message) {
      // For old-style custom messages - should deprecate and migrate to new
      // method
      if (exports.oncustommessage) {
        exports.oncustommessage(message);
      }

      // Send messages.foo and messages.bar to appropriate handlers
      this._sendMessagesToHandlers(message, customMessageHandlers,
                                   customMessageHandlerOrder);
    });

    addMessageHandler('config', function(message) {
      this.config = message;
    });

  }).call(ShinyApp.prototype);


  // =========================================================================
  // File Processor
  // =========================================================================

  // Generic driver class for doing chunk-wise asynchronous processing of a
  // FileList object. Subclass/clone it and override the `on*` functions to
  // make it do something useful.
  var FileProcessor = function(files) {
    this.files = files;
    this.fileIndex = -1;
    // Currently need to use small chunk size because R-Websockets can't
    // handle continuation frames
    this.aborted = false;
    this.completed = false;

    // TODO: Register error/abort callbacks

    this.$run();
  };
  (function() {
    // Begin callbacks. Subclassers/cloners may override any or all of these.
    this.onBegin = function(files, cont) {
      setTimeout(cont, 0);
    };
    this.onFile = function(file, cont) {
      setTimeout(cont, 0);
    };
    this.onComplete = function() {
    };
    this.onAbort = function() {
    };
    // End callbacks

    // Aborts processing, unless it's already completed
    this.abort = function() {
      if (this.completed || this.aborted)
        return;

      this.aborted = true;
      this.onAbort();
    };

    // Returns a bound function that will call this.$run one time.
    this.$getRun = function() {
      var self = this;
      var called = false;
      return function() {
        if (called)
          return;
        called = true;
        self.$run();
      };
    };

    // This function will be called multiple times to advance the process.
    // It relies on the state of the object's fields to know what to do next.
    this.$run = function() {

      var self = this;

      if (this.aborted || this.completed)
        return;

      if (this.fileIndex < 0) {
        // Haven't started yet--begin
        this.fileIndex = 0;
        this.onBegin(this.files, this.$getRun());
        return;
      }

      if (this.fileIndex === this.files.length) {
        // Just ended
        this.completed = true;
        this.onComplete();
        return;
      }

      // If we got here, then we have a file to process, or we are
      // in the middle of processing a file, or have just finished
      // processing a file.

      var file = this.files[this.fileIndex++];
      this.onFile(file, this.$getRun());
    };
  }).call(FileProcessor.prototype);


  // =========================================================================
  // Binding registry
  // =========================================================================
  var BindingRegistry = function() {
    this.bindings = [];
    this.bindingNames = {};
  };
  (function() {
    this.register = function(binding, bindingName, priority) {
      var bindingObj = {binding: binding, priority: priority || 0};
      this.bindings.unshift(bindingObj);
      if (bindingName) {
        this.bindingNames[bindingName] = bindingObj;
        binding.name = bindingName;
      }
    };
    this.setPriority = function(bindingName, priority) {
      var bindingObj = this.bindingNames[bindingName];
      if (!bindingObj)
        throw "Tried to set priority on unknown binding " + bindingName;
      bindingObj.priority = priority || 0;
    };
    this.getPriority = function(bindingName) {
      var bindingObj = this.bindingNames[bindingName];
      if (!bindingObj)
        return false;
      return bindingObj.priority;
    };
    this.getBindings = function() {
      // Sort the bindings. The ones with higher priority are consulted
      // first; ties are broken by most-recently-registered.
      return mergeSort(this.bindings, function(a, b) {
        return b.priority - a.priority;
      });
    };
  }).call(BindingRegistry.prototype);


  var inputBindings = exports.inputBindings = new BindingRegistry();
  var outputBindings = exports.outputBindings = new BindingRegistry();

  // =========================================================================
  // Output bindings
  // =========================================================================

  var OutputBinding = exports.OutputBinding = function() {};
  (function() {
    // Returns a jQuery object or element array that contains the
    // descendants of scope that match this binding
    this.find = function(scope) { throw "Not implemented"; };

    this.getId = function(el) {
      return el['data-input-id'] || el.id;
    };

    this.onValueChange = function(el, data) {
      this.clearError(el);
      this.renderValue(el, data);
    };
    this.onValueError = function(el, err) {
      this.renderError(el, err);
    };
    this.renderError = function(el, err) {
      $(el).addClass('shiny-output-error').text(err.message);
    };
    this.clearError = function(el) {
      $(el).removeClass('shiny-output-error');
    };
    this.showProgress = function(el, show) {
      var RECALC_CLASS = 'recalculating';
      if (show)
        $(el).addClass(RECALC_CLASS);
      else
        $(el).removeClass(RECALC_CLASS);
    };
  }).call(OutputBinding.prototype);


  var textOutputBinding = new OutputBinding();
  $.extend(textOutputBinding, {
    find: function(scope) {
      return $(scope).find('.shiny-text-output');
    },
    renderValue: function(el, data) {
      $(el).text(data);
    }
  });
  outputBindings.register(textOutputBinding, 'shiny.textOutput');

  var imageOutputBinding = new OutputBinding();
  $.extend(imageOutputBinding, {
    find: function(scope) {
      return $(scope).find('.shiny-image-output, .shiny-plot-output');
    },
    renderValue: function(el, data) {
      var self = this;
      var $el = $(el);
      // Load the image before emptying, to minimize flicker
      var img = null;
      var clickId, hoverId;

      if (data) {
        clickId = $el.data('click-id');
        hoverId = $el.data('hover-id');

        $el.data('coordmap', data.coordmap);
        delete data.coordmap;

        img = document.createElement('img');
        // Copy items from data to img. This should include 'src'
        $.each(data, function(key, value) {
          if (value !== null)
            img[key] = value;
        });

        // Firefox doesn't have offsetX/Y, so we need to use an alternate
        // method of calculation for it
        var mouseOffset = function(mouseEvent) {
          if (typeof(mouseEvent.offsetX) !== 'undefined') {
            return {
              x: mouseEvent.offsetX,
              y: mouseEvent.offsetY
            };
          }
          var offset = $el.offset();
          return {
            x: mouseEvent.pageX - offset.left,
            y: mouseEvent.pageY - offset.top
          };
        };

        var createMouseHandler = function(inputId) {
          return function(e) {
            if (e === null) {
              exports.onInputChange(inputId, null);
              return;
            }

            // TODO: Account for scrolling within the image??

            var coordmap = $el.data('coordmap');
            function devToUsrX(deviceX) {
              var x = deviceX - coordmap.bounds.left;
              var factor = (coordmap.usr.right - coordmap.usr.left) /
                  (coordmap.bounds.right - coordmap.bounds.left);
              return (x * factor) + coordmap.usr.left;
            }
            function devToUsrY(deviceY) {
              var y = deviceY - coordmap.bounds.bottom;
              var factor = (coordmap.usr.top - coordmap.usr.bottom) /
                  (coordmap.bounds.top - coordmap.bounds.bottom);
              return (y * factor) + coordmap.usr.bottom;
            }

            var offset = mouseOffset(e);

            var userX = devToUsrX(offset.x);
            if (coordmap.log.x)
              userX = Math.pow(10, userX);

            var userY = devToUsrY(offset.y);
            if (coordmap.log.y)
              userY = Math.pow(10, userY);

            exports.onInputChange(inputId, {
              x: userX,
              y: userY,
              ".nonce": Math.random()
            });
          };
        };

        if (!$el.data('hover-func')) {
          var hoverDelayType = $el.data('hover-delay-type') || 'debounce';
          var delayFunc = (hoverDelayType === 'throttle') ? throttle : debounce;
          var hoverFunc = delayFunc($el.data('hover-delay') || 300,
                                    createMouseHandler(hoverId));
          $el.data('hover-func', hoverFunc);
        }

        if (clickId)
          $(img).on('mousedown', createMouseHandler(clickId));
        if (hoverId) {
          $(img).on('mousemove', $el.data('hover-func'));
          $(img).on('mouseout', function(e) {
            $el.data('hover-func')(null);
          });
        }

        if (clickId || hoverId) {
          $(img).addClass('crosshair');
        }
      }

      $el.empty();
      if (img)
        $el.append(img);
    }
  });
  outputBindings.register(imageOutputBinding, 'shiny.imageOutput');

  var htmlOutputBinding = new OutputBinding();
  $.extend(htmlOutputBinding, {
    find: function(scope) {
      return $(scope).find('.shiny-html-output');
    },
    onValueError: function(el, err) {
      exports.unbindAll(el);
      this.renderError(el, err);
    },
    renderValue: function(el, data) {
      exports.unbindAll(el);

      var html;
      if (data === null) {
        html = '';
      } else {
        html = data;
      }

      exports.renderHtml(html, el);
      exports.initializeInputs(el);
      exports.bindAll(el);
    }
  });
  outputBindings.register(htmlOutputBinding, 'shiny.htmlOutput');

  // Render HTML in a DOM element, inserting singletons into head as needed
  exports.renderHtml = function(html, el) {
    return singletons.renderHtml(html, el);
  };

  var singletons = {
    knownSingletons: {},
    renderHtml: function(html, el) {
      var processed = this._processHtml(html);
      this._addToHead(processed.head);
      this.register(processed.singletons);
      $(el).html(processed.html);
      return processed;
    },
    // Take an object where keys are names of singletons, and merges it into
    // knownSingletons
    register: function(s) {
      $.extend(this.knownSingletons, s);
    },
    // Takes a string or array of strings and adds them to knownSingletons
    registerNames: function(s) {
      if (typeof s === 'string') {
        this.knownSingletons[s] = true;
      } else if (s instanceof Array) {
        for (var i = 0; i < s.length; i++) {
          this.knownSingletons[s[i]] = true;
        }
      }
    },
    // Inserts new content into document head
    _addToHead: function(head) {
      if (head.length > 0) {
        var tempDiv = $("<div>" + head + "</div>")[0];
        var $head = $('head');
        while (tempDiv.hasChildNodes()) {
          $head.append(tempDiv.firstChild);
        }
      }
    },
    // Reads HTML and returns an object with info about singletons
    _processHtml: function(val) {
      var self = this;
      var newSingletons = {};
      var newVal;

      var findNewPayload = function(match, p1, sig, payload) {
        if (self.knownSingletons[sig] || newSingletons[sig])
          return "";
        newSingletons[sig] = true;
        return payload;
      };
      while (true) {
        newVal = val.replace(self._reSingleton, findNewPayload);
        if (val.length === newVal.length)
          break;
        val = newVal;
      }

      var heads = [];
      var headAddPayload = function(match, payload) {
        heads.push(payload);
        return "";
      };
      while (true) {
        newVal = val.replace(self._reHead, headAddPayload);
        if (val.length === newVal.length)
          break;
        val = newVal;
      }

      return {
        html: val,
        head: heads.join("\n"),
        singletons: newSingletons
      };
    },
    _reSingleton: /<!--(SHINY.SINGLETON\[([\w]+)\])-->([\s\S]*?)<!--\/\1-->/,
    _reHead: /<head(?:\s[^>]*)?>([\s\S]*?)<\/head>/
  };

  var downloadLinkOutputBinding = new OutputBinding();
  $.extend(downloadLinkOutputBinding, {
    find: function(scope) {
      return $(scope).find('a.shiny-download-link');
    },
    renderValue: function(el, data) {
      $(el).attr('href', data);
    }
  });
  outputBindings.register(downloadLinkOutputBinding, 'shiny.downloadLink');

  var datatableOutputBinding = new OutputBinding();
  $.extend(datatableOutputBinding, {
    find: function(scope) {
      return $(scope).find('.shiny-datatable-output');
    },
    onValueError: function(el, err) {
      exports.unbindAll(el);
      this.renderError(el, err);
    },
    renderValue: function(el, data) {
      var $el = $(el).empty();
      if (!data || !data.colnames) return;

      var colnames = $.makeArray(data.colnames);
      var header = $.map(colnames, function(x) {
        return '<th>' + x + '</th>';
      }).join('');
      header = '<thead><tr>' + header + '</tr></thead>';
      var footer = '';
      if (data.options === null || data.options.bFilter !== false) {
        footer = $.map(colnames, function(x) {
          return '<th><input type="text" placeholder="' + x + '" /></th>';
        }).join('');
        footer = '<tfoot>' + footer + '</tfoot>';
      }
      var content = '<table class="table table-striped table-hover">' +
                    header + footer + '</table>';
      $el.append(content);

      // options that should be eval()ed
      if (data.evalOptions)
        $.each(data.evalOptions, function(i, x) {
          /*jshint evil: true */
          data.options[x] = eval('(' + data.options[x] + ')');
        });

      var oTable = $(el).children("table").dataTable($.extend({
        "bProcessing": true,
        "bServerSide": true,
        "aaSorting": [],
        "bSortClasses": false,
        "iDisplayLength": 25,
        "sAjaxSource": data.action
      }, data.options));
      // the table object may need post-processing
      if (typeof data.callback === 'string') {
        /*jshint evil: true */
        var callback = eval('(' + data.callback + ')');
        if (typeof callback === 'function') callback(oTable);
      }

      // use debouncing for searching boxes
      $el.find('label input').first().unbind('keyup')
           .keyup(debounce(data.searchDelay, function() {
              oTable.fnFilter(this.value);
            }));
      var searchInputs = $el.find("tfoot input");
      if (searchInputs.length > 0) {
        $.each(oTable.fnSettings().aoColumns, function(i, x) {
          // hide the text box if not searchable
          if (!x.bSearchable) searchInputs.eq(i).hide();
        });
        searchInputs.keyup(debounce(data.searchDelay, function() {
          oTable.fnFilter(this.value, searchInputs.index(this));
        }));
      }
      // FIXME: ugly scrollbars in tab panels b/c Bootstrap uses 'visible: auto'
      $el.parents('.tab-content').css('overflow', 'visible');
    }
  });
  outputBindings.register(datatableOutputBinding, 'shiny.datatableOutput');

  // =========================================================================
  // Input bindings
  // =========================================================================

  var InputBinding = exports.InputBinding = function() {
  };

  (function() {

    // Returns a jQuery object or element array that contains the
    // descendants of scope that match this binding
    this.find = function(scope) { throw "Not implemented"; };

    this.getId = function(el) {
      return el['data-input-id'] || el.id;
    };

    // Gives the input a type in case the server needs to know it
    // to deserialize the JSON correctly
    this.getType = function() { return false; };
    this.getValue = function(el) { throw "Not implemented"; };
    this.subscribe = function(el, callback) { };
    this.unsubscribe = function(el) { };

    // This is used for receiving messages that tell the input object to do
    // things, such as setting values (including min, max, and others).
    // 'data' should be an object with elements corresponding to value, min,
    // max, etc., as appropriate for the type of input object. It also should
    // trigger a change event.
    this.receiveMessage = function(el, data) { throw "Not implemented"; };
    this.getState = function(el, data) { throw "Not implemented"; };

    this.getRatePolicy = function() { return null; };

    // Some input objects need initialization before being bound. This is
    // called when the document is ready (for statically-added input objects),
    // and when new input objects are added to the document with
    // htmlOutputBinding.renderValue() (for dynamically-added input objects).
    // This is called before the input is bound.
    this.initialize = function(el) { };

    // This is called after unbinding the output.
    this.dispose = function(el) { };

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
      $(el).on('keyup.textInputBinding input.textInputBinding', function(event) {
        callback(true);
      });
      $(el).on('change.textInputBinding', function(event) {
        callback(false);
      });
    },
    unsubscribe: function(el) {
      $(el).off('.textInputBinding');
    },
    receiveMessage: function(el, data) {
      if (data.hasOwnProperty('value'))
        this.setValue(el, data.value);

      if (data.hasOwnProperty('label'))
        $(el).parent().find('label[for="' + $escape(el.id) + '"]').text(data.label);

      $(el).trigger('change');
    },
    getState: function(el) {
      return {
        label: $(el).parent().find('label[for="' + $escape(el.id) + '"]').text(),
        value: el.value
      };
    },
    getRatePolicy: function() {
      return {
        policy: 'debounce',
        delay: 250
      };
    }
  });
  inputBindings.register(textInputBinding, 'shiny.textInput');


  var textareaInputBinding = {};
  $.extend(textareaInputBinding, textInputBinding, {
    find: function(scope) {
      return $(scope).find('textarea');
    }
  });
  inputBindings.register(textareaInputBinding, 'shiny.textareaInput');


  var numberInputBinding = {};
  $.extend(numberInputBinding, textInputBinding, {
    find: function(scope) {
      return $(scope).find('input[type="number"]');
    },
    getValue: function(el) {
      var numberVal = $(el).val();
      if (/^\s*$/.test(numberVal))  // Return null if all whitespace
        return null;
      else if (!isNaN(numberVal))   // If valid Javascript number string, coerce to number
        return +numberVal;
      else
        return numberVal;           // If other string like "1e6", send it unchanged
    },
    setValue: function(el, value) {
      el.value = value;
    },
    getType: function(el) {
      return "shiny.number";
    },
    receiveMessage: function(el, data) {
      if (data.hasOwnProperty('value'))  el.value = data.value;
      if (data.hasOwnProperty('min'))    el.min   = data.min;
      if (data.hasOwnProperty('max'))    el.max   = data.max;
      if (data.hasOwnProperty('step'))   el.step  = data.step;

      if (data.hasOwnProperty('label'))
        $(el).parent().find('label[for="' + $escape(el.id) + '"]').text(data.label);

      $(el).trigger('change');
    },
    getState: function(el) {
      return { label: $(el).parent().find('label[for="' + $escape(el.id) + '"]').text(),
               value: this.getValue(el),
               min:   Number(el.min),
               max:   Number(el.max),
               step:  Number(el.step) };
    }
  });
  inputBindings.register(numberInputBinding, 'shiny.numberInput');


  var checkboxInputBinding = new InputBinding();
  $.extend(checkboxInputBinding, {
    find: function(scope) {
      return $(scope).find('input[type="checkbox"]');
    },
    getValue: function(el) {
      return el.checked;
    },
    setValue: function(el, value) {
      el.checked = value;
    },
    getState: function(el) {
      return {
        label: $(el).parent().find('span').text(),
        value: el.checked
      };
    },
    receiveMessage: function(el, data) {
      if (data.hasOwnProperty('value'))
        el.checked = data.value;

      if (data.hasOwnProperty('label'))
        $(el).parent().find('span').text(data.label);

      $(el).trigger('change');
    }
  });
  inputBindings.register(checkboxInputBinding, 'shiny.checkboxInput');

  var sliderInputBinding = {};
  $.extend(sliderInputBinding, textInputBinding, {
    find: function(scope) {
      // Check if jslider plugin is loaded
      if (!$.fn.slider)
        return [];

      return $(scope).find('input.jslider');
    },
    getValue: function(el) {
      var sliderVal = $(el).slider("value");
      if (/;/.test(sliderVal)) {
        var chunks = sliderVal.split(/;/, 2);
        return [+chunks[0], +chunks[1]];
      }
      else {
        return +sliderVal;
      }
    },
    setValue: function(el, value) {
      if (value instanceof Array) {
        $(el).slider("value", value[0], value[1]);
      } else {
        $(el).slider("value", value);
      }
    },
    subscribe: function(el, callback) {
      $(el).on('change.sliderInputBinding', function(event) {
        callback(!$(el).data('animating'));
      });
    },
    unsubscribe: function(el) {
      $(el).off('.sliderInputBinding');
    },
    receiveMessage: function(el, data) {
      if (data.hasOwnProperty('value'))
        this.setValue(el, data.value);

      if (data.hasOwnProperty('label'))
        $(el).parent().find('label[for="' + $escape(el.id) + '"]').text(data.label);

      // jslider doesn't support setting other properties

      $(el).trigger('change');
    },
    getRatePolicy: function() {
      return {
        policy: 'debounce',
        delay: 250
      };
    },
    getState: function(el) {
      var $el = $(el);
      var settings = $el.slider().settings;

      return { label: $el.parent().find('label[for="' + $escape(el.id) + '"]').text(),
               value:  this.getValue(el),
               min:    Number(settings.from),
               max:    Number(settings.to),
               step:   Number(settings.step),
               round:  settings.round,
               format: settings.format.format,
               locale: settings.format.locale
             };
    },
    initialize: function(el) {
      $(el).slider();
    }
  });
  inputBindings.register(sliderInputBinding, 'shiny.sliderInput');


  var dateInputBinding = new InputBinding();
  $.extend(dateInputBinding, {
    find: function(scope) {
      return $(scope).find('.shiny-date-input');
    },
    getType: function(el) {
      return "shiny.date";
    },
    // Return the date in an unambiguous format, yyyy-mm-dd (as opposed to a
    // format like mm/dd/yyyy)
    getValue: function(el) {
      var date = $(el).find('input').data('datepicker').getUTCDate();
      return this._formatDate(date);
    },
    // value must be an unambiguous string like '2001-01-01', or a Date object.
    setValue: function(el, value) {
      var date = this._newDate(value);
      // If date is invalid, do nothing
      if (isNaN(date))
        return;

      $(el).find('input').datepicker('update', date);
    },
    getState: function(el) {
      var $el = $(el);
      var $input = $el.find('input');

      var min = $input.data('datepicker').startDate;
      var max = $input.data('datepicker').endDate;

      // Stringify min and max. If min and max aren't set, they will be
      // -Infinity and Infinity; replace these with null.
      min = (min === -Infinity) ? null : this._formatDate(min);
      max = (max ===  Infinity) ? null : this._formatDate(max);

      // startViewMode is stored as a number; convert to string
      var startview = $input.data('datepicker').startViewMode;
      if      (startview === 2)  startview = 'decade';
      else if (startview === 1)  startview = 'year';
      else if (startview === 0)  startview = 'month';

      return {
        label:       $el.find('label[for="' + $escape(el.id) + '"]').text(),
        value:       this.getValue(el),
        valueString: $input.val(),
        min:         min,
        max:         max,
        language:    $input.data('datepicker').language,
        weekstart:   $input.data('datepicker').weekStart,
        format:      this._formatToString($input.data('datepicker').format),
        startview:   startview
      };
    },
    receiveMessage: function(el, data) {
      var $input = $(el).find('input');

      if (data.hasOwnProperty('value'))
        this.setValue(el, data.value);

      if (data.hasOwnProperty('label'))
        $(el).find('label[for="' + $escape(el.id) + '"]').text(data.label);

      if (data.hasOwnProperty('min'))
        this._setMin($input[0], data.min);

      if (data.hasOwnProperty('max'))
        this._setMax($input[0], data.max);

      $(el).trigger('change');
    },
    subscribe: function(el, callback) {
      $(el).on('keyup.dateInputBinding input.dateInputBinding', function(event) {
        // Use normal debouncing policy when typing
        callback(true);
      });
      $(el).on('changeDate.dateInputBinding change.dateInputBinding', function(event) {
        // Send immediately when clicked
        callback(false);
      });
    },
    unsubscribe: function(el) {
      $(el).off('.dateInputBinding');
    },
    getRatePolicy: function() {
      return {
        policy: 'debounce',
        delay: 250
      };
    },
    initialize: function(el) {
      var $input = $(el).find('input');

      var date = $input.data('initial-date');
      // If initial_date is null, set to current date
      if (date === undefined || date === null) {
        // Get local date, but as UTC
        date = this._dateAsUTC(new Date());
      }

      this.setValue(el, date);

      // Set the start and end dates, from min-date and max-date. These always
      // use yyyy-mm-dd format, instead of bootstrap-datepicker's built-in
      // support for date-startdate and data-enddate, which use the current
      // date format.
      this._setMin($input[0], $input.data('min-date'));
      this._setMax($input[0], $input.data('max-date'));
    },
    // Given a Date object, return a string in yyyy-mm-dd format, using the
    // UTC date. This may be a day off from the date in the local time zone.
    _formatDate: function(date) {
      if (date instanceof Date) {
        return date.getUTCFullYear() + '-' +
               padZeros(date.getUTCMonth()+1, 2) + '-' +
               padZeros(date.getUTCDate(), 2);

      } else {
        return null;
      }
    },
    // Given a format object from a date picker, return a string
    _formatToString: function(format) {
      // Format object has structure like:
      // { parts: ['mm', 'dd', 'yy'], separators: ['', '/', '/' ,''] }
      var str = '';
      for (var i = 0; i < format.parts.length; i++) {
        str += format.separators[i] + format.parts[i];
      }
      str += format.separators[i];
      return str;
    },
    // Given an unambiguous date string or a Date object, set the min (start) date.
    // null will unset.
    _setMin: function(el, date) {
      if (date === null) {
        $(el).datepicker('setStartDate', null);

      } else {
        date = this._newDate(date);
        if (!isNaN(date))
          $(el).datepicker('setStartDate', date);
      }
    },
    // Given an unambiguous date string or a Date object, set the max (end) date
    // null will unset.
    _setMax: function(el, date) {
      if (date === null) {
        $(el).datepicker('setEndDate', null);

      } else {
        date = this._newDate(date);
        if (!isNaN(date))
          $(el).datepicker('setEndDate', date);
      }
    },
    // Given a date string of format yyyy-mm-dd, return a Date object with
    // that date at 12AM UTC.
    // If date is a Date object, return it unchanged.
    _newDate: function(date) {
      if (date instanceof Date)
        return date;
      if (!date)
        return null;

      // Get Date object - this will be at 12AM in UTC, but may print
      // differently at the Javascript console.
      var d = parseDate(date);

      // If invalid date, return null
      if (isNaN(d))
        return null;

      return new Date(d.getTime());
    },
    // Given a Date object, return a Date object which has the same "clock time"
    // in UTC. For example, if input date is 2013-02-01 23:00:00 GMT-0600 (CST),
    // output will be 2013-02-01 23:00:00 UTC. Note that the JS console may
    // print this in local time, as "Sat Feb 02 2013 05:00:00 GMT-0600 (CST)".
    _dateAsUTC: function(date) {
      return new Date(date.getTime() - date.getTimezoneOffset() * 60000);
    }
  });
  inputBindings.register(dateInputBinding, 'shiny.dateInput');


  var dateRangeInputBinding = {};
  $.extend(dateRangeInputBinding, dateInputBinding, {
    find: function(scope) {
      return $(scope).find('.shiny-date-range-input');
    },
    // Return the date in an unambiguous format, yyyy-mm-dd (as opposed to a
    // format like mm/dd/yyyy)
    getValue: function(el) {
      var $inputs = $(el).find('input');
      var start = $inputs.eq(0).data('datepicker').getUTCDate();
      var end   = $inputs.eq(1).data('datepicker').getUTCDate();

      return [this._formatDate(start), this._formatDate(end)];
    },
    // value must be an array of unambiguous strings like '2001-01-01', or
    // Date objects.
    setValue: function(el, value) {
      if (!(value instanceof Array)) {
        return;
      }

      // Get the start and end input objects
      var $inputs = $(el).find('input');

      // If value is undefined, don't try to set
      if (value[0] !== undefined) {
        var start = this._newDate(value[0]);
        $inputs.eq(0).datepicker('update', start);
      }
      if (value[1] !== undefined) {
        var end = this._newDate(value[1]);
        $inputs.eq(1).datepicker('update', end);
      }

      // Make it so that the correct items are highlighted when the calendar is
      // displayed
      $(el).datepicker('updateDates');
    },
    getState: function(el) {
      var $el = $(el);
      var $inputs     = $el.find('input');
      var $startinput = $inputs.eq(0);
      var $endinput   = $inputs.eq(1);

      // For many of the properties, assume start and end have the same values
      var min = $startinput.data('datepicker').startDate;
      var max = $startinput.data('datepicker').endDate;

      // Stringify min and max. If min and max aren't set, they will be
      // -Infinity and Infinity; replace these with null.
      min = (min === -Infinity) ? null : this._formatDate(min);
      max = (max ===  Infinity) ? null : this._formatDate(max);

      // startViewMode is stored as a number; convert to string
      var startview = $startinput.data('datepicker').startViewMode;
      if      (startview === 2)  startview = 'decade';
      else if (startview === 1)  startview = 'year';
      else if (startview === 0)  startview = 'month';

      return {
        label:       $el.find('label[for="' + $escape(el.id) + '"]').text(),
        value:       this.getValue(el),
        valueString: [ $startinput.val(), $endinput.val() ],
        min:         min,
        max:         max,
        weekstart:   $startinput.data('datepicker').weekStart,
        format:      this._formatToString($startinput.data('datepicker').format),
        language:    $startinput.data('datepicker').language,
        startview:   startview
      };
    },
    receiveMessage: function(el, data) {
      var $el = $(el);
      var $inputs     = $el.find('input');
      var $startinput = $inputs.eq(0);
      var $endinput   = $inputs.eq(1);

      if (data.hasOwnProperty('value'))
        this.setValue(el, data.value);

      if (data.hasOwnProperty('label'))
        $el.find('label[for="' + $escape(el.id) + '"]').text(data.label);

      if (data.hasOwnProperty('min')) {
        this._setMin($startinput[0], data.min);
        this._setMin($endinput[0],   data.min);
      }

      if (data.hasOwnProperty('max')) {
        this._setMax($startinput[0], data.max);
        this._setMax($endinput[0],   data.max);
      }

      $el.trigger('change');
    },
    initialize: function(el) {
      var $el = $(el);
      var $inputs     = $el.find('input');
      var $startinput = $inputs.eq(0);
      var $endinput   = $inputs.eq(1);

      var start = $startinput.data('initial-date');
      var end   = $endinput.data('initial-date');

      // If empty/null, use local date, but as UTC
      if (start === undefined || start === null)
        start = this._dateAsUTC(new Date());

      if (end === undefined || end === null)
        end = this._dateAsUTC(new Date());

      this.setValue(el, [start, end]);

      // // Set the start and end dates, from min-date and max-date. These always
      // // use yyyy-mm-dd format, instead of bootstrap-datepicker's built-in
      // // support for date-startdate and data-enddate, which use the current
      // // date format.
      this._setMin($startinput[0], $startinput.data('min-date'));
      this._setMin($endinput[0],   $startinput.data('min-date'));
      this._setMax($startinput[0], $endinput.data('max-date'));
      this._setMax($endinput[0],   $endinput.data('max-date'));
    },
    subscribe: function(el, callback) {
      $(el).on('keyup.dateRangeInputBinding input.dateRangeInputBinding', function(event) {
        // Use normal debouncing policy when typing
        callback(true);
      });
      $(el).on('changeDate.dateRangeInputBinding change.dateRangeInputBinding', function(event) {
        // Send immediately when clicked
        callback(false);
      });
    },
    unsubscribe: function(el) {
      $(el).off('.dateRangeInputBinding');
    }
  });
  inputBindings.register(dateRangeInputBinding, 'shiny.dateRangeInput');


  // Select input
  var selectInputBinding = new InputBinding();
  $.extend(selectInputBinding, {
    find: function(scope) {
      return $(scope).find('select');
    },
    getId: function(el) {
      return InputBinding.prototype.getId.call(this, el) || el.name;
    },
    getValue: function(el) {
      return $(el).val();
    },
    setValue: function(el, value) {
      var selectize = this._selectize(el);
      if (selectize) {
        selectize[0].selectize.setValue(value);
      } else $(el).val(value);
    },
    getState: function(el) {
      // Store options in an array of objects, each with with value and label
      var options = new Array(el.length);
      for (var i = 0; i < el.length; i++) {
        options[i] = { value:    el[i].value,
                       label:    el[i].label };
      }

      return {
        label: $(el).parent().find('label[for="' + $escape(el.id) + '"]').text(),
        value:    this.getValue(el),
        options:  options
      };
    },
    receiveMessage: function(el, data) {
      var $el = $(el);

      // This will replace all the options
      if (data.hasOwnProperty('options')) {
        // Clear existing options and add each new one
        $el.empty();
        var selectize = this._selectize(el);
        if (selectize) {
          selectize = selectize[0].selectize;
          selectize.clearOptions();
          // Selectize.js doesn't maintain insertion order on Chrome on Mac
          // with >10 items if inserted using addOption (versus being present
          // in the DOM at selectize() time). Putting $order on each option
          // makes it work.
          $.each(data.options, function(i, opt) {
            opt.$order = i;
          });
          selectize.addOption(data.options);
        }
        for (var i = 0; i < data.options.length; i++) {
          var in_opt = data.options[i];

          var $newopt = $('<option/>', {
            value: in_opt.value,
            text: in_opt.label
          });

          $el.append($newopt);
        }
      }

      if (data.hasOwnProperty('value'))
        this.setValue(el, data.value);

      if (data.hasOwnProperty('label'))
        $(el).parent().find('label[for="' + $escape(el.id) + '"]').text(data.label);

      $(el).trigger('change');
    },
    subscribe: function(el, callback) {
      $(el).on('change.selectInputBinding', function(event) {
        callback();
      });
    },
    unsubscribe: function(el) {
      $(el).off('.selectInputBinding');
    },
    initialize: function(el) {
      this._selectize(el);
    },
    _selectize: function(el) {
      if (!$.fn.selectize) return;
      var $el = $(el);
      var config = $el.parent().find('script[data-for="' + $escape(el.id) + '"]');
      if (config.length > 0) {
        var options = $.extend({
          labelField: 'label',
          valueField: 'value',
          searchField: ['label']
        }, JSON.parse(config.html()));
        if (config.data('nonempty') !== undefined) {
          options = $.extend(options, {
            onItemRemove: function(value) {
              if (this.getValue() === "")
                $("select#" + $escape(el.id)).empty().append($("<option/>", {
                  "value": value, "selected": true
                })).trigger("change");
            },
            onDropdownClose: function($dropdown) {
              if (this.getValue() === "")
                this.setValue($("select#" + $escape(el.id)).val());
            }
          });
        }
        // options that should be eval()ed
        if (config.data('eval') instanceof Array)
          $.each(config.data('eval'), function(i, x) {
            /*jshint evil: true*/
            options[x] = eval('(' + options[x] + ')');
          });

        return $el.selectize(options);
      }
    }
  });
  inputBindings.register(selectInputBinding, 'shiny.selectInput');


  // Radio input groups
  var radioInputBinding = new InputBinding();
  $.extend(radioInputBinding, {
    find: function(scope) {
      return $(scope).find('.shiny-input-radiogroup');
    },
    getValue: function(el) {
      // Select the radio objects that have name equal to the grouping div's id
      return $('input:radio[name="' + $escape(el.id) + '"]:checked').val();
    },
    setValue: function(el, value) {
      $('input:radio[name="' + $escape(el.id) + '"][value="' + $escape(value) + '"]').prop('checked', true);
    },
    getState: function(el) {
      var $objs = $('input:radio[name="' + $escape(el.id) + '"]');

      // Store options in an array of objects, each with with value and label
      var options = new Array($objs.length);
      for (var i = 0; i < options.length; i++) {
        options[i] = { value:   $objs[i].value,
                       label:   this._getLabel($objs[i]) };
      }

      return {
        label:    $(el).parent().find('label[for="' + $escape(el.id) + '"]').text(),
        value:    this.getValue(el),
        options:  options
      };
    },
    receiveMessage: function(el, data) {
      var $el = $(el);

      // This will replace all the options
      if (data.hasOwnProperty('options')) {
        // Clear existing options and add each new one
        $el.find('label.radio').remove();
        for (var i = 0; i < data.options.length; i++) {
          var in_opt = data.options[i];

          var $newopt = $('<label class="radio"/>');
          var $radio = $('<input/>', {
            type:  "radio",
            name:  el.id,
            id:    el.id + (i+1).toString(),
            value: in_opt.value
          });

          $newopt.append($radio);
          $newopt.append('<span>' + in_opt.label + '</span>');

          $el.append($newopt);
        }
      }

      if (data.hasOwnProperty('value'))
        this.setValue(el, data.value);

      if (data.hasOwnProperty('label'))
        $(el).parent().find('label[for="' + $escape(el.id) + '"]').text(data.label);

      $(el).trigger('change');
    },
    subscribe: function(el, callback) {
      $(el).on('change.radioInputBinding', function(event) {
        callback();
      });
    },
    unsubscribe: function(el) {
      $(el).off('.radioInputBinding');
    },
    // Given an input DOM object, get the associated label. Handles labels
    // that wrap the input as well as labels associated with 'for' attribute.
    _getLabel: function(obj) {
      // If <input id='myid'><label for='myid'>label text</label>
      var $label_for = $('label[for="' + $escape(obj.id) + '"]');
      if ($label_for.length > 0) {
        return $.trim($label_for.text());
      }

      // If <label><input /><span>label text</span></label>
      if (obj.parentNode.tagName === "LABEL") {
        return $.trim($(obj.parentNode).find('span').text());
      }

      return null;
    },
    // Given an input DOM object, set the associated label. Handles labels
    // that wrap the input as well as labels associated with 'for' attribute.
    _setLabel: function(obj, value) {
      // If <input id='myid'><label for='myid'>label text</label>
      var $label_for = $('label[for="' + $escape(obj.id) + '"]');
      if ($label_for.length > 0) {
        $label_for.text(value);
      }

      // If <label><input /><span>label text</span></label>
      if (obj.parentNode.tagName === "LABEL") {
        $(obj.parentNode).find('span').text(value);
      }

      return null;
    }

  });
  inputBindings.register(radioInputBinding, 'shiny.radioInput');


  // Checkbox input groups
  var checkboxGroupInputBinding = new InputBinding();
  $.extend(checkboxGroupInputBinding, {
    find: function(scope) {
      return $(scope).find('.shiny-input-checkboxgroup');
    },
    getValue: function(el) {
      // Select the checkbox objects that have name equal to the grouping div's id
      var $objs = $('input:checkbox[name="' + $escape(el.id) + '"]:checked');
      var values = new Array($objs.length);
      for (var i = 0; i < $objs.length; i ++) {
        values[i] = $objs[i].value;
      }
      return values;
    },
    setValue: function(el, value) {
      // Clear all checkboxes
      $('input:checkbox[name="' + $escape(el.id) + '"]').prop('checked', false);

      // Accept array
      if (value instanceof Array) {
        for (var i = 0; i < value.length; i++) {
          $('input:checkbox[name="' + $escape(el.id) + '"][value="' + $escape(value[i]) + '"]')
            .prop('checked', true);
        }
      // Else assume it's a single value
      } else {
        $('input:checkbox[name="' + $escape(el.id) + '"][value="' + $escape(value) + '"]')
          .prop('checked', true);
      }

    },
    getState: function(el) {
      var $objs = $('input:checkbox[name="' + $escape(el.id) + '"]');

      // Store options in an array of objects, each with with value and label
      var options = new Array($objs.length);
      for (var i = 0; i < options.length; i++) {
        options[i] = { value:   $objs[i].value,
                       label:   this._getLabel($objs[i]) };
      }

      return { label:    $(el).find('label[for="' + $escape(el.id) + '"]').text(),
               value:    this.getValue(el),
               options:  options
             };
    },
    receiveMessage: function(el, data) {
      var $el = $(el);

      // This will replace all the options
      if (data.hasOwnProperty('options')) {
        // Clear existing options and add each new one
        $el.find('label.checkbox').remove();
        for (var i = 0; i < data.options.length; i++) {
          var in_opt = data.options[i];

          var $newopt = $('<label class="checkbox"/>');
          var $checkbox = $('<input/>', {
            type:  "checkbox",
            name:  el.id,
            id:    el.id + (i+1).toString(),
            value: in_opt.value
          });

          $newopt.append($checkbox);
          $newopt.append('<span>' + in_opt.label + '</span>');

          $el.append($newopt);
        }
      }

      if (data.hasOwnProperty('value'))
        this.setValue(el, data.value);

      if (data.hasOwnProperty('label'))
        $el.find('label[for="' + $escape(el.id) + '"]').text(data.label);

      $(el).trigger('change');
    },
    subscribe: function(el, callback) {
      $(el).on('change.checkboxGroupInputBinding', function(event) {
        callback();
      });
    },
    unsubscribe: function(el) {
      $(el).off('.checkboxGroupInputBinding');
    },
    // Given an input DOM object, get the associated label. Handles labels
    // that wrap the input as well as labels associated with 'for' attribute.
    _getLabel: function(obj) {
      // If <input id='myid'><label for='myid'>label text</label>
      var $label_for = $('label[for="' + $escape(obj.id) + '"]');
      if ($label_for.length > 0) {
        return $.trim($label_for.text());
      }

      // If <label><input /><span>label text</span></label>
      if (obj.parentNode.tagName === "LABEL") {
        return $.trim($(obj.parentNode).find('span').text());
      }

      return null;
    },
    // Given an input DOM object, set the associated label. Handles labels
    // that wrap the input as well as labels associated with 'for' attribute.
    _setLabel: function(obj, value) {
      // If <input id='myid'><label for='myid'>label text</label>
      var $label_for = $('label[for="' + $escape(obj.id) + '"]');
      if ($label_for.length > 0) {
        $label_for.text(value);
      }

      // If <label><input /><span>label text</span></label>
      if (obj.parentNode.tagName === "LABEL") {
        $(obj.parentNode).find('span').text(value);
      }

      return null;
    }

  });
  inputBindings.register(checkboxGroupInputBinding, 'shiny.checkboxGroupInput');


  var actionButtonInputBinding = new InputBinding();
  $.extend(actionButtonInputBinding, {
    find: function(scope) {
      return $(scope).find(".action-button");
    },
    getValue: function(el) {
      return $(el).data('val') || 0;
    },
    setValue: function(el, value) {
      $(el).data('val', value);
    },
    subscribe: function(el, callback) {
      $(el).on("click.actionButtonInputBinding", function(e) {
        var $el = $(this);
        var val = $el.data('val') || 0;
        $el.data('val', val + 1);

        callback();
      });
    },
    getState: function(el) {
      return { value: this.getValue(el) };
    },
    receiveMessage: function(el, data) {
    },
    unsubscribe: function(el) {
      $(el).off(".actionButtonInputBinding");
    }
  });
  inputBindings.register(actionButtonInputBinding, 'shiny.actionButtonInput');


  var bootstrapTabInputBinding = new InputBinding();
  $.extend(bootstrapTabInputBinding, {
    find: function(scope) {
      return $(scope).find('ul.nav.shiny-tab-input');
    },
    getValue: function(el) {
      var anchor = $(el).find('li:not(.dropdown).active').children('a');
      if (anchor.length === 1)
        return this._getTabName(anchor);

      return null;
    },
    setValue: function(el, value) {
      var self = this;
      var anchors = $(el).find('li:not(.dropdown)').children('a');
      anchors.each(function() {
        if (self._getTabName($(this)) === value) {
          $(this).tab('show');
          return false;
        }
      });
    },
    getState: function(el) {
      return { value: this.getValue(el) };
    },
    receiveMessage: function(el, data) {
      if (data.hasOwnProperty('value'))
        this.setValue(el, data.value);
    },
    subscribe: function(el, callback) {
      $(el).on('shown.bootstrapTabInputBinding', function(event) {
        callback();
      });
    },
    unsubscribe: function(el) {
      $(el).off('.bootstrapTabInputBinding');
    },
    _getTabName: function(anchor) {
      return anchor.attr('data-value') || anchor.text();
    }
  });
  inputBindings.register(bootstrapTabInputBinding, 'shiny.bootstrapTabInput');


  var FileUploader = function(shinyapp, id, files) {
    this.shinyapp = shinyapp;
    this.id = id;
    FileProcessor.call(this, files);
  };
  $.extend(FileUploader.prototype, FileProcessor.prototype);
  (function() {
    this.makeRequest = function(method, args, onSuccess, onFailure, blobs) {
      this.shinyapp.makeRequest(method, args, onSuccess, onFailure, blobs);
    };
    this.onBegin = function(files, cont) {
      var self = this;

      // Reset progress bar
      this.$setError(null);
      this.$setActive(true);
      this.$setVisible(true);
      this.onProgress(null, 0);

      this.totalBytes = 0;
      this.progressBytes = 0;
      $.each(files, function(i, file) {
        self.totalBytes += file.size;
      });

      var fileInfo = $.map(files, function(file, i) {
        return {
          name: file.name,
          size: file.size,
          type: file.type
        };
      });

      this.makeRequest(
        'uploadInit', [fileInfo],
        function(response) {
          self.jobId = response.jobId;
          self.uploadUrl = response.uploadUrl;
          cont();
        },
        function(error) {
          self.onError(error);
        });
    };
    this.onFile = function(file, cont) {
      var self = this;
      this.onProgress(file, 0);

      $.ajax(this.uploadUrl, {
        type: 'POST',
        cache: false,
        xhr: function() {
          var xhrVal = $.ajaxSettings.xhr();
          if (xhrVal.upload) {
            xhrVal.upload.onprogress = function(e) {
              if (e.lengthComputable) {
                self.onProgress(
                  file,
                  (self.progressBytes + e.loaded) / self.totalBytes);
              }
            };
          }
          return xhrVal;
        },
        data: file,
        processData: false,
        success: function() {
          self.progressBytes += file.size;
          cont();
        },
        error: function(jqXHR, textStatus, errorThrown) {
          self.onError(jqXHR.responseText || textStatus);
        }
      });
    };
    this.onComplete = function() {
      var self = this;
      this.makeRequest(
        'uploadEnd', [this.jobId, this.id],
        function(response) {
          self.$setActive(false);
          self.onProgress(null, 1);
          self.$label().text('Upload complete');
        },
        function(error) {
          self.onError(error);
        });
      this.$label().text('Finishing upload');
    };
    this.onError = function(message) {
      this.$setError(message || '');
      this.$setActive(false);
    };
    this.onAbort = function() {
      this.$setVisible(false);
    };
    this.onProgress = function(file, completed) {
      this.$bar().width(Math.round(completed*100) + '%');
      this.$label().text(file ? file.name : '');
    };
    this.$container = function() {
      return $('#' + $escape(this.id) + '_progress.shiny-file-input-progress');
    };
    this.$bar = function() {
      return $('#' + $escape(this.id) + '_progress.shiny-file-input-progress .bar');
    };
    this.$label = function() {
      return $('#' + $escape(this.id) + '_progress.shiny-file-input-progress label');
    };
    this.$setVisible = function(visible) {
      this.$container().css('visibility', visible ? 'visible' : 'hidden');
    };
    this.$setError = function(error) {
      this.$bar().toggleClass('bar-danger', (error !== null));
      if (error !== null) {
        this.onProgress(null, 1);
        this.$label().text(error);
      }
    };
    this.$setActive = function(active) {
      this.$container().toggleClass('active', !!active);
    };
  }).call(FileUploader.prototype);


  function uploadFiles(evt) {
    // If previously selected files are uploading, abort that.
    var el = $(evt.target);
    var uploader = el.data('currentUploader');
    if (uploader)
      uploader.abort();

    var files = evt.target.files;
    var id = fileInputBinding.getId(evt.target);

    if (files.length === 0)
      return;

    // Start the new upload and put the uploader in 'currentUploader'.
    el.data('currentUploader', new FileUploader(exports.shinyapp, id, files));
  }

  var fileInputBinding = new InputBinding();
  $.extend(fileInputBinding, {
    find: function(scope) {
      return $(scope).find('input[type="file"]');
    },
    getId: function(el) {
      return InputBinding.prototype.getId.call(this, el) || el.name;
    },
    getValue: function(el) {
      return null;
    },
    setValue: function(el, value) {
      // Not implemented
    },
    subscribe: function(el, callback) {
      $(el).on('change.fileInputBinding', uploadFiles);
    },
    unsubscribe: function(el) {
      $(el).off('.fileInputBinding');
    }
  });
  inputBindings.register(fileInputBinding, 'shiny.fileInputBinding');


  var OutputBindingAdapter = function(el, binding) {
    this.el = el;
    this.binding = binding;
  };
  (function() {
    this.onValueChange = function(data) {
      this.binding.onValueChange(this.el, data);
    };
    this.onValueError = function(err) {
      this.binding.onValueError(this.el, err);
    };
    this.showProgress = function(show) {
      this.binding.showProgress(this.el, show);
    };
  }).call(OutputBindingAdapter.prototype);


  // =========================================================================
  // initShiny
  // =========================================================================
  function initShiny() {

    var shinyapp = exports.shinyapp = new ShinyApp();

    function bindOutputs(scope) {

      if (scope === undefined)
        scope = document;

      scope = $(scope);

      var bindings = outputBindings.getBindings();

      for (var i = 0; i < bindings.length; i++) {
        var binding = bindings[i].binding;
        var matches = binding.find(scope) || [];
        for (var j = 0; j < matches.length; j++) {
          var el = matches[j];
          var id = binding.getId(el);

          // Check if ID is falsy
          if (!id)
            continue;

          var bindingAdapter = new OutputBindingAdapter(el, binding);
          shinyapp.bindOutput(id, bindingAdapter);
          $(el).data('shiny-output-binding', bindingAdapter);
          $(el).addClass('shiny-bound-output');
        }
      }

      // Send later in case DOM layout isn't final yet.
      setTimeout(sendImageSize, 0);
      setTimeout(sendOutputHiddenState, 0);
    }

    function unbindOutputs(scope) {
      if (scope === undefined)
        scope = document;

      var outputs = $(scope).find('.shiny-bound-output');
      for (var i = 0; i < outputs.length; i++) {
        var bindingAdapter = $(outputs[i]).data('shiny-output-binding');
        if (!bindingAdapter)
          continue;
        var id = bindingAdapter.binding.getId(outputs[i]);
        shinyapp.unbindOutput(id, bindingAdapter);
        $(outputs[i]).removeClass('shiny-bound-output');
      }

      setTimeout(sendOutputHiddenState, 0);
    }

    function elementToValue(el) {
      if (el.type === 'checkbox' || el.type === 'radio')
        return el.checked ? true : false;
      else
        return $(el).val();
    }

    var inputBatchSender = new InputBatchSender(shinyapp);
    var inputsNoResend = new InputNoResendDecorator(inputBatchSender);
    var inputsRate = new InputRateDecorator(inputsNoResend);
    var inputsDefer = new InputDeferDecorator(inputsNoResend);

    // By default, use rate decorator
    var inputs = inputsRate;
    $('input[type="submit"], button[type="submit"]').each(function() {
      // If there is a submit button on the page, use defer decorator
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
      if (id) {
        var value = binding.getValue(el);
        var type = binding.getType(el);
        if (type)
          id = id + ":" + type;
        inputs.setInput(id, value, !allowDeferred);
      }
    }

    function bindInputs(scope) {

      if (scope === undefined)
        scope = document;

      var bindings = inputBindings.getBindings();

      var currentValues = {};

      for (var i = 0; i < bindings.length; i++) {
        var binding = bindings[i].binding;
        var matches = binding.find(scope) || [];
        for (var j = 0; j < matches.length; j++) {
          var el = matches[j];
          var id = binding.getId(el);

          // Check if ID is falsy, or if already bound
          if (!id || boundInputs[id])
            continue;

          var type = binding.getType(el);
          var effectiveId = type ? id + ":" + type : id;
          currentValues[effectiveId] = binding.getValue(el);

          /*jshint loopfunc:true*/
          var thisCallback = (function() {
            var thisBinding = binding;
            var thisEl = el;
            return function(allowDeferred) {
              valueChangeCallback(thisBinding, thisEl, allowDeferred);
            };
          })();

          binding.subscribe(el, thisCallback);
          $(el).data('shiny-input-binding', binding);
          $(el).addClass('shiny-bound-input');
          var ratePolicy = binding.getRatePolicy();
          if (ratePolicy !== null) {
            inputsRate.setRatePolicy(
              effectiveId,
              ratePolicy.policy,
              ratePolicy.delay);
          }

          boundInputs[id] = {
            binding: binding,
            node: el
          };

          if (shinyapp.isConnected()) {
            valueChangeCallback(binding, el, false);
          }
        }
      }

      return currentValues;
    }

    function unbindInputs(scope) {
      if (scope === undefined)
        scope = document;

      var inputs = $(scope).find('.shiny-bound-input');
      for (var i = 0; i < inputs.length; i++) {
        var binding = $(inputs[i]).data('shiny-input-binding');
        if (!binding)
          continue;
        var id = binding.getId(inputs[i]);
        $(inputs[i]).removeClass('shiny-bound-input');
        delete boundInputs[id];
        binding.unsubscribe(inputs[i]);
      }
    }


    function getMultiValue(input, exclusiveValue) {
      if (!input.name)
        return null;

      var els = $(
        'input:checked' +
        '[type="' + $escape(input.type) + '"]' +
        '[name="' + $escape(input.name) + '"]');
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

    function bindMultiInput(selector, exclusiveValue) {
      $(document).on('change input', selector, function() {
        // Check if this radio or checkbox is of the new style (after 0.5.0),
        // where there's a wrapping div that has an associated input binding object.
        // If so, then exit. This function is only meant to operate on old-style
        // checkbox and radio groups.
        if (isNewStyleMultiInput(this)) {
          return;
        }

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

      function isNewStyleMultiInput(el) {
        if ((el.type === "checkbox" &&
             ancestorHasClass(el, 'shiny-input-checkboxgroup')) ||
            (el.type === "radio" &&
             ancestorHasClass(el, 'shiny-input-radiogroup'))) {
          return true;

        } else {
          return false;
        }
      }

      function ancestorHasClass(el, classname) {
        if (el === null) {
          return false;
        } else if (hasClass(el, classname)) {
          return true;
        } else {
          return ancestorHasClass(el.parentNode, classname);
        }
      }

      // Fast (non-jQuery) function for checking if an element has a class
      function hasClass(el, classname) {
        return (' ' + el.className + ' ').indexOf(' ' + classname + ' ') > -1;
      }
    }

    function getMultiInputValues(scope, selector, exclusiveValue) {
      var initialValues = {};
      $(scope).find(selector).each(function() {
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
      return initialValues;
    }

    function _bindAll(scope) {
      bindOutputs(scope);
      return $.extend(
        {},
        getMultiInputValues(scope, 'input[type="checkbox"]', false),
        getMultiInputValues(scope, 'input[type="radio"]', true),
        bindInputs(scope)
      );
    }
    function unbindAll(scope) {
      unbindInputs(scope);
      unbindOutputs(scope);
    }
    exports.bindAll = function(scope) {
      // _bindAll alone returns initial values, it doesn't send them to the
      // server. export.bindAll needs to send the values to the server, so we
      // wrap _bindAll in a closure that does that.
      var currentValues = _bindAll(scope);
      $.each(currentValues, function(name, value) {
        inputs.setInput(name, value);
      });
    };
    exports.unbindAll = unbindAll;

    // Calls .initialize() for all of the input objects in all input bindings,
    // in the given scope.
    function initializeInputs(scope) {
      if (scope === undefined)
        scope = document;

      var bindings = inputBindings.getBindings();

      // Iterate over all bindings
      for (var i = 0; i < bindings.length; i++) {
        var binding = bindings[i].binding;
        var inputObjects = binding.find(scope) || [];

        // Iterate over all input objects for this binding
        for (var j = 0; j < inputObjects.length; j++) {
          binding.initialize(inputObjects[j]);
        }
      }
    }
    exports.initializeInputs = initializeInputs;


    // Initialize all input objects in the document, before binding
    initializeInputs(document);

    // Binding multiInputs in this method is useful for old-style (<=0.5.0)
    // HTML generated by Shiny. This should be deprecated at some point.
    bindMultiInput('input[type="checkbox"]', false);
    bindMultiInput('input[type="radio"]', true);
    var initialValues = _bindAll(document);


    // The server needs to know the size of each image and plot output element,
    // in case it is auto-sizing
    $('.shiny-image-output, .shiny-plot-output').each(function() {
      if (this.offsetWidth !== 0 || this.offsetHeight !== 0) {
        initialValues['.clientdata_output_' + this.id + '_width'] = this.offsetWidth;
        initialValues['.clientdata_output_' + this.id + '_height'] = this.offsetHeight;
      }
    });
    function sendImageSize() {
      $('.shiny-image-output, .shiny-plot-output').each(function() {
        if (this.offsetWidth !== 0 || this.offsetHeight !== 0) {
          inputs.setInput('.clientdata_output_' + this.id + '_width', this.offsetWidth);
          inputs.setInput('.clientdata_output_' + this.id + '_height', this.offsetHeight);
        }
      });
    }

    // Return true if the object or one of its ancestors in the DOM tree has
    // style='display:none'; otherwise return false.
    function isHidden(obj) {
      // null means we've hit the top of the tree. If width or height is
      // non-zero, then we know that no ancestor has display:none.
      if (obj === null || obj.offsetWidth !== 0 || obj.offsetHeight !== 0) {
        return false;
      } else if (getStyle(obj, 'display') === 'none') {
        return true;
      } else {
        return(isHidden(obj.parentNode));
      }
    }
    var lastKnownVisibleOutputs = {};
    // Set initial state of outputs to hidden, if needed
    $('.shiny-bound-output').each(function() {
      if (isHidden(this)) {
        initialValues['.clientdata_output_' + this.id + '_hidden'] = true;
      } else {
        lastKnownVisibleOutputs[this.id] = true;
        initialValues['.clientdata_output_' + this.id + '_hidden'] = false;
      }
    });
    // Send update when hidden state changes
    function doSendOutputHiddenState() {
      var visibleOutputs = {};
      $('.shiny-bound-output').each(function() {
        delete lastKnownVisibleOutputs[this.id];
        // Assume that the object is hidden when width and height are 0
        if (isHidden(this)) {
          inputs.setInput('.clientdata_output_' + this.id + '_hidden', true);
        } else {
          visibleOutputs[this.id] = true;
          inputs.setInput('.clientdata_output_' + this.id + '_hidden', false);
        }
      });
      // Anything left in lastKnownVisibleOutputs is orphaned
      for (var name in lastKnownVisibleOutputs) {
        if (lastKnownVisibleOutputs.hasOwnProperty(name))
          inputs.setInput('.clientdata_output_' + name + '_hidden', true);
      }
      // Update the visible outputs for next time
      lastKnownVisibleOutputs = visibleOutputs;
    }
    // sendOutputHiddenState gets called each time DOM elements are shown or
    // hidden. This can be in the hundreds or thousands of times at startup.
    // We'll debounce it, so that we do the actual work once per tick.
    var sendOutputHiddenStateDebouncer = new Debouncer(null, doSendOutputHiddenState, 0);
    function sendOutputHiddenState() {
      sendOutputHiddenStateDebouncer.normalCall();
    }
    // We need to make sure doSendOutputHiddenState actually gets called before
    // the inputBatchSender sends data to the server. The lastChanceCallback
    // here does that - if the debouncer has a pending call, flush it.
    inputBatchSender.lastChanceCallback.push(function() {
      if (sendOutputHiddenStateDebouncer.isPending())
        sendOutputHiddenStateDebouncer.immediateCall();
    });

    // The size of each image may change either because the browser window was
    // resized, or because a tab was shown/hidden (hidden elements report size
    // of 0x0). It's OK to over-report sizes because the input pipeline will
    // filter out values that haven't changed.
    $(window).resize(debounce(500, sendImageSize));
    $('body').on('shown.sendImageSize', '*', sendImageSize);
    $('body').on('shown.sendOutputHiddenState hidden.sendOutputHiddenState', '*',
                 sendOutputHiddenState);

    // Send initial pixel ratio, and update it if it changes
    initialValues['.clientdata_pixelratio'] = pixelRatio();
    $(window).resize(function() {
      inputs.setInput('.clientdata_pixelratio', pixelRatio());
    });

    // Send initial URL
    initialValues['.clientdata_url_protocol'] = window.location.protocol;
    initialValues['.clientdata_url_hostname'] = window.location.hostname;
    initialValues['.clientdata_url_port']     = window.location.port;
    initialValues['.clientdata_url_pathname'] = window.location.pathname;
    initialValues['.clientdata_url_search']   = window.location.search;
    // This is only the initial value of the hash. The hash can change, but
    // a reactive version of this isn't sent because w atching for changes can
    // require polling on some browsers. The JQuery hashchange plugin can be
    // used if this capability is important.
    initialValues['.clientdata_url_hash_initial'] = window.location.hash;

    // The server needs to know what singletons were rendered as part of
    // the page loading
    var singletonText = initialValues['.clientdata_singletons'] =
        $('script[type="application/shiny-singletons"]').text();
    singletons.registerNames(singletonText.split(/,/));

    // We've collected all the initial values--start the server process!
    inputsNoResend.reset(initialValues);
    shinyapp.connect(initialValues);
  } // function initShiny()

  $(function() {
    // Init Shiny a little later than document ready, so user code can
    // run first (i.e. to register bindings)
    setTimeout(initShiny, 1);
  });

  $(document).on('click', '.slider-animate-button', function(evt) {
    evt.preventDefault();
    var self = $(this);
    var target = $('#' + $escape(self.attr('data-target-id')));
    var slider = target.slider();
    var startLabel = 'Play';
    var stopLabel = 'Pause';
    var loop = self.attr('data-loop') !== undefined &&
               !/^\s*false\s*$/i.test(self.attr('data-loop'));
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

  $(document).on('keydown', function(e) {
    if (e.which !== 114 || (!e.ctrlKey && !e.metaKey) || (e.shiftKey || e.altKey))
      return;
    var url = 'reactlog?w=' + exports.shinyapp.config.workerId;
    window.open(url);
    e.preventDefault();
  });

})();
