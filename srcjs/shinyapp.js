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
      // some older WebKit browsers return the pathname already decoded;
      // if we find invalid URL characters in the path, encode them
      if (!/^([$#!&-;=?-[\]_a-z~]|%[0-9a-fA-F]{2})+$/.test(defaultPath)) {
        defaultPath = encodeURI(defaultPath);
        // Bizarrely, QtWebKit requires us to encode these characters *twice*
        if (browser.isQt) {
          defaultPath = encodeURI(defaultPath);
        }
      }
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

    var conditionals = $(document).find('[data-display-if]');
    for (var i = 0; i < conditionals.length; i++) {
      var el = $(conditionals[i]);
      var condFunc = el.data('data-display-if-func');

      if (!condFunc) {
        var condExpr = el.attr('data-display-if');
        condFunc = scopeExprToFunc(condExpr);
        el.data('data-display-if-func', condFunc);
      }

      var show = condFunc(scope);
      var showing = el.css("display") !== "none";
      if (show !== showing) {
        if (show) {
          el.trigger('show');
          el.show();
          el.trigger('shown');
        }
        else {
          el.trigger('hide');
          el.hide();
          el.trigger('hidden');
        }
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
    if (message.type && message.message) {
      var handler = progressHandlers[message.type];
      if (handler)
        handler.call(this, message.message);
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


  // Progress reporting ====================================================

  var progressHandlers = {
    // Progress for a particular object
    binding: function(message) {
      $(document.documentElement).addClass('shiny-busy');
      var key = message.id;
      var binding = this.$bindings[key];
      if (binding && binding.showProgress) {
        binding.showProgress(true);
      }
    },
    // Open a page-level progress bar
    open: function(message) {
      // Add progress container (for all progress items) if not already present
      var $container = $('.shiny-progress-container');
      if ($container.length === 0) {
        $container = $('<div class="shiny-progress-container"></div>');
        $('body').append($container);
      }

      // Add div for just this progress ID
      var depth = $('.shiny-progress.open').length;
      var $progress = $(progressHandlers.progressHTML);
      $progress.attr('id', message.id);
      $container.append($progress);

      // Stack bars
      var $progressBar = $progress.find('.progress');
      $progressBar.css('top', depth * $progressBar.height() + 'px');

      // Stack text objects
      var $progressText = $progress.find('.progress-text');
      $progressText.css('top', 3 * $progressBar.height() +
        depth * $progressText.outerHeight() + 'px');

      $progress.hide();
    },

    // Update page-level progress bar
    update: function(message) {
      var $progress = $('#' + message.id + '.shiny-progress');
      if (typeof(message.message) !== 'undefined') {
        $progress.find('.progress-message').text(message.message);
      }
      if (typeof(message.detail) !== 'undefined') {
        $progress.find('.progress-detail').text(message.detail);
      }
      if (typeof(message.value) !== 'undefined') {
        if (message.value !== null) {
          $progress.find('.progress').show();
          $progress.find('.bar').width((message.value*100) + '%');
        }
        else {
          $progress.find('.progress').hide();
        }
      }

      $progress.fadeIn();
    },

    // Close page-level progress bar
    close: function(message) {
      var $progress = $('#' + message.id + '.shiny-progress');
      $progress.removeClass('open');

      $progress.fadeOut({
        complete: function() {
          $progress.remove();

          // If this was the last shiny-progress, remove container
          if ($('.shiny-progress').length === 0)
            $('.shiny-progress-container').remove();
        }
      });
    },

    // The 'bar' class is needed for backward compatibility with Bootstrap 2.
    progressHTML: '<div class="shiny-progress open">' +
      '<div class="progress progress-striped active"><div class="progress-bar bar"></div></div>' +
      '<div class="progress-text">' +
        '<span class="progress-message">message</span>' +
        '<span class="progress-detail"></span>' +
      '</div>' +
    '</div>'
  };

  exports.progressHandlers = progressHandlers;


}).call(ShinyApp.prototype);
