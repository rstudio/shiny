(function() {

  var $ = jQuery;

  var exports = window.Shiny = window.Shiny || {};

  function randomId() {
    return Math.floor(0x100000000 + (Math.random() * 0xF00000000)).toString(16);
  }

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


  var ShinyApp = function() {
    this.$socket = null;
    this.$bindings = {};
    this.$values = {};
    this.$pendingMessages = [];
    this.$activeRequests = {};
    this.$nextRequestId = 0;
  };

  (function() {

    this.connect = function(initialInput) {
      if (this.$socket)
        throw "Connect was already called on this application object";

      this.$socket = this.createSocket();
      this.$initialInput = initialInput;
    };

    this.isConnected = function() {
      return !!this.$socket;
    };

    this.createSocket = function () {
      var self = this;

      var createSocketFunc = exports.createSocket || function() {
        var ws = new WebSocket('ws://' + window.location.host, 'shiny');
        ws.binaryType = 'arraybuffer';
        return ws;
      };

      var socket = createSocketFunc();
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

      this.$sendMsg(msg);
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
    //   The parameter to onError will be an error object or message (format
    //   TBD).
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

        function uint32_to_buf(val) {
          var buffer = new ArrayBuffer(4);
          var view = new DataView(buffer);
          view.setUint32(0, val, true); // little-endian
          return new Uint8Array(buffer);
        }

        var payload = [];
        payload.push(uint32_to_buf(0x01020202)); // signature

        var jsonBuf = new Blob([msg]);
        payload.push(uint32_to_buf(jsonBuf.size));
        payload.push(jsonBuf);

        for (var i = 0; i < blobs.length; i++) {
          payload.push(uint32_to_buf(blobs[i].byteLength || blobs[i].size || 0));
          payload.push(blobs[i]);
        }

        msg = new Blob(payload);
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
      if (msgObj.response) {
        var resp = msgObj.response;
        var requestId = resp.tag;
        var request = this.$activeRequests[requestId];
        if (request) {
          delete this.$activeRequests[requestId];
          if ('value' in resp)
            request.onSuccess(resp.value);
          else
            request.onError(resp.error);
        }
      };
    };

    this.bindOutput = function(id, binding) {
      if (!id)
        throw "Can't bind an element with no ID";
      if (this.$bindings[id])
        throw "Duplicate binding for ID " + id;
      this.$bindings[id] = binding;
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
  }).call(ShinyApp.prototype);



  var FileProcessor = function(files) {
    this.files = files;
    this.fileReader = new FileReader();
    this.fileIndex = -1;
    this.pos = 0;
    // Currently need to use small chunk size because R-Websockets can't
    // handle continuation frames
    this.chunkSize = 4096;
    this.aborted = false;
    this.completed = false;
    
    var self = this;
    $(this.fileReader).on('load', function(evt) {
      self.$endReadChunk();
    });
    // TODO: Register error/abort callbacks
    
    this.$run();
  };
  $.extend(FileProcessor.prototype, {
    // Begin callbacks. Subclassers may override any or all of these.
    onBegin: function(files, cont) {
      setTimeout(cont, 0);
    },
    onFileBegin: function(file, cont) {
      setTimeout(cont, 0);
    },
    onFileChunk: function(file, offset, blob, cont) {
      setTimeout(cont, 0);
    },
    onFileEnd: function(file, cont) {
      setTimeout(cont, 0);
    },
    onComplete: function() {
    },
    onAbort: function() {
    },
    // End callbacks
    
    // Aborts processing, unless it's already completed
    abort: function() {
      if (this.completed || this.aborted)
        return;
      
      this.aborted = true;
      this.onAbort();
    },
    
    // Returns a bound function that will call this.$run one time.
    $getRun: function() {
      var self = this;
      var called = false;
      return function() {
        if (called)
          return;
        called = true;
        self.$run();
      };
    },
    
    // This function will be called multiple times to advance the process.
    // It relies on the state of the object's fields to know what to do next.
    $run: function() {
      
      var self = this;

      if (this.aborted || this.completed)
        return;
      
      if (this.fileIndex < 0) {
        // Haven't started yet--begin
        this.fileIndex = 0;
        this.onBegin(this.files, this.$getRun());
        return;
      }
      
      if (this.fileIndex == this.files.length) {
        // Just ended
        this.completed = true;
        this.onComplete();
        return;
      }
      
      // If we got here, then we have a file to process, or we are
      // in the middle of processing a file, or have just finished
      // processing a file.
      
      var file = this.files[this.fileIndex];
      if (this.pos >= file.size) {
        // We've read past the end of this file--it's done
        this.fileIndex++;
        this.pos = 0;
        this.onFileEnd(file, this.$getRun());
      }
      else if (this.pos == 0) {
        // We're just starting with this file, need to call onFileBegin
        // before we actually start reading
        var called = false;
        this.onFileBegin(file, function() {
          if (called)
            return;
          called = true;
          self.$beginReadChunk();
        });
      }
      else {
        // We're neither starting nor ending--just start the next chunk
        this.$beginReadChunk();
      }
    },
    
    // Starts asynchronous read of the current chunk of the current file
    $beginReadChunk: function() {
      var file = this.files[this.fileIndex];
      var blob = file.slice(this.pos, this.pos + this.chunkSize);
      this.fileReader.readAsArrayBuffer(blob);
    },
    
    // Called when a chunk has been successfully read
    $endReadChunk: function() {
      var file = this.files[this.fileIndex];
      var offset = this.pos;
      var data = this.fileReader.result;
      this.pos = this.pos + this.chunkSize;
      this.onFileChunk(file, offset, new Blob([new Uint8Array(data)]),
                       this.$getRun());
    }
  });




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
    },
    this.setPriority = function(bindingName, priority) {
      var bindingObj = this.bindingNames[bindingName];
      if (!bindingObj)
        throw "Tried to set priority on unknown binding " + bindingName;
      bindingObj.priority = priority || 0;
    },
    this.getPriority = function(bindingName) {
      var bindingObj = this.bindingNames[bindingName];
      if (!bindingObj)
        return false;
      return bindingObj.priority;
    },
    this.getBindings = function() {
      // Sort the bindings. The ones with higher priority are consulted
      // first; ties are broken by most-recently-registered.
      return mergeSort(this.bindings, function(a, b) {
        return b.priority - a.priority;
      });
    }
  }).call(BindingRegistry.prototype);


  var inputBindings = exports.inputBindings = new BindingRegistry();
  var outputBindings = exports.outputBindings = new BindingRegistry();

  
  var OutputBinding = function() {};
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
      $(el).addClass('shiny-output-error').text('ERROR: ' + err.message);
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

  var plotOutputBinding = new OutputBinding();
  $.extend(plotOutputBinding, {
    find: function(scope) {
      return $(scope).find('.shiny-plot-output');
    },
    renderValue: function(el, data) {
      $(el).empty();
      if (!data)
        return;
      var img = document.createElement('img');
      img.src = data;
      $(el).append(img);
    }
  });
  outputBindings.register(plotOutputBinding, 'shiny.plotOutput');

  var htmlOutputBinding = new OutputBinding();
  $.extend(htmlOutputBinding, {
    find: function(scope) {
      return $(scope).find('.shiny-html-output');
    },
    renderValue: function(el, data) {
      $(el).html(data);
    }
  });
  outputBindings.register(htmlOutputBinding, 'shiny.htmlOutput');


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
      if (!isNaN(numberVal))
        return +numberVal;
      else
        return numberVal;
    }
  });
  inputBindings.register(numberInputBinding, 'shiny.numberInput');


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
  inputBindings.register(sliderInputBinding, 'shiny.sliderInput');
  
  
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
  inputBindings.register(selectInputBinding, 'shiny.selectInput');


  var bootstrapTabInputBinding = new InputBinding();
  $.extend(bootstrapTabInputBinding, {
    find: function(scope) {
      return scope.find('ul.nav.nav-tabs');
    },
    getValue: function(el) {
      var anchor = $(el).children('li.active').children('a');
      if (anchor.length == 1)
        return this.$getTabName(anchor);
      return null;
    },
    setValue: function(el, value) {
      var self = this;
      var anchors = $(el).children('li').children('a');
      anchors.each(function() {
        if (self.$getTabName($(this)) === value) {
          $(this).tab('show');
          return false;
        }
      });
    },
    subscribe: function(el, callback) {
      var self = this;
      $(el).on('shown.bootstrapTabInputBinding', function(event) {
        callback(self, el);
      });
    },
    unsubscribe: function(el) {
      $(el).off('.bootstrapTabInputBinding');
    },
    $getTabName: function(anchor) {
      return anchor.attr('data-value') || anchor.text();
    }
  });
  inputBindings.register(bootstrapTabInputBinding, 'shiny.bootstrapTabInput');


  var FileUploader = function(shinyapp, id, files) {
    this.shinyapp = shinyapp;
    this.id = id;
    FileProcessor.call(this, files);
  };
  $.extend(FileUploader.prototype, FileProcessor.prototype, {
    makeRequest: function(method, args, onSuccess, onFailure, blobs) {
      this.shinyapp.makeRequest(method, args, onSuccess, onFailure, blobs);
    },
    onBegin: function(files, cont) {
      var self = this;
      this.makeRequest(
        'uploadInit', [],
        function(response) {
          self.jobId = response.jobId;
          cont();
        },
        function(error) {
        });
    },
    onFileBegin: function(file, cont) {
      this.onProgress(file, 0);
      
      this.makeRequest(
        'uploadFileBegin', [this.jobId, file.name, file.type, file.size],
        function(response) {
          cont();
        },
        function(error) {
        });
    },
    onFileChunk: function(file, offset, blob, cont) {
      this.onProgress(file, (offset + blob.size) / file.size);

      this.makeRequest(
        'uploadFileChunk', [this.jobId],
        function(response) {
          cont();
        },
        function(error) {
        },
        [blob]);
    },
    onFileEnd: function(file, cont) {
      this.makeRequest(
        'uploadFileEnd', [this.jobId],
        function(response) {
          cont();
        },
        function(error) {
        });
    },
    onComplete: function() {
      this.makeRequest(
        'uploadEnd', [this.jobId, this.id], 
        function(response) {
        },
        function(error) {
        });
    },
    onAbort: function() {
    },
    onProgress: function(file, completed) {
      console.log('file: ' + file.name + ' [' + Math.round(completed*100) + '%]');
    }
  });

  function uploadFiles(evt) {
    // If previously selected files are uploading, abort that.
    var el = $(evt.target);
    var uploader = el.data('currentUploader');
    if (uploader)
      uploader.abort();

    var files = evt.target.files;
    var id = fileInputBinding.getId(evt.target);

    // Start the new upload and put the uploader in 'currentUploader'.
    el.data('currentUploader', new FileUploader(exports.shinyapp, id, files));
  };

  var fileInputBinding = new InputBinding();
  $.extend(fileInputBinding, {
    find: function(scope) {
      return scope.find('input[type="file"]');
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
  })
  inputBindings.register(fileInputBinding, 'shiny.fileInputBinding');

  

  function initShiny() {

    var shinyapp = exports.shinyapp = new ShinyApp();

    function bindOutputs(scope) {

      var OutputBindingAdapter = function(el, binding) {
        this.el = el;
        this.binding = binding;
      };
      $.extend(OutputBindingAdapter.prototype, {
        onValueChange: function(data) {
          this.binding.onValueChange(this.el, data);
        },
        onValueError: function(err) {
          this.binding.onValueError(this.el, err);
        },
        showProgress: function(show) {
          this.binding.showProgress(this.el, show);
        }
      });

      if (scope == undefined)
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

          shinyapp.bindOutput(id, new OutputBindingAdapter(el, binding));
          $(el).data('shiny-output-binding', binding);
          $(el).addClass('shiny-bound-output');
        }
      }
    }

    function unbindOutputs(scope) {
      if (scope == undefined)
        scope = document;

      var outputs = $(scope).find('.shiny-bound-output');
      for (var i = 0; i < outputs.length; i++) {
        var binding = $(outputs[i]).data('shiny-output-binding');
        if (!binding)
          continue;
        var id = binding.getId(outputs[i]);
        shinyapp.unbindOutput(id, binding);
        $(outputs[i]).removeClass('shiny-bound-output');
      }
    }

    function elementToValue(el) {
      if (el.type == 'checkbox' || el.type == 'radio')
        return el.checked ? true : false;
      else
        return $(el).val();
    }

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

      if (scope == undefined)
        scope = document;
      
      scope = $(scope);

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
    
          currentValues[id] = binding.getValue(el);
          binding.subscribe(el, valueChangeCallback);
          $(el).data('shiny-input-binding', binding);
          $(el).addClass('shiny-bound-input');
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

          if (shinyapp.isConnected()) {
            valueChangeCallback(binding, el, false);
          }
        }
      }
    
      return currentValues;
    }

    function unbindInputs(scope) {
      if (scope == undefined)
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
    
    bindOutputs(document);
    initialValues = bindInputs(document);
    exports.bindOutputs = bindOutputs;
    exports.bindInputs = bindInputs;
    exports.unbindInputs = unbindInputs;

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

    // The server needs to know the size of each plot output element, in case
    // the plot is auto-sizing
    $('.shiny-plot-output').each(function() {
      var width = this.offsetWidth;
      var height = this.offsetHeight;
      initialValues['.shinyout_' + this.id + '_width'] = width;
      initialValues['.shinyout_' + this.id + '_height'] = height;
    });
    function sendPlotSize() {
      $('.shiny-plot-output').each(function() {
        inputs.setInput('.shinyout_' + this.id + '_width', this.offsetWidth);
        inputs.setInput('.shinyout_' + this.id + '_height', this.offsetHeight);
      });
    }
    // The size of each plot may change either because the browser window was
    // resized, or because a tab was shown/hidden (hidden elements report size
    // of 0x0). It's OK to over-report sizes because the input pipeline will
    // filter out values that haven't changed.
    $(window).resize(debounce(500, sendPlotSize));
    $('body').on('shown.sendPlotSize hidden.sendPlotSize', '*', sendPlotSize);

    // We've collected all the initial values--start the server process!
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
