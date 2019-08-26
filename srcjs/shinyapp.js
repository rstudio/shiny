var ShinyApp = function() {
  this.$socket = null;

  // Cached input values
  this.$inputValues = {};

  // Input values at initialization (and reconnect)
  this.$initialInput = {};

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

  this.$allowReconnect = false;
};

(function() {

  this.connect = function(initialInput) {
    if (this.$socket)
      throw "Connect was already called on this application object";

    this.$socket = this.createSocket();
    this.$initialInput = initialInput;
    $.extend(this.$inputValues, initialInput);

    this.$updateConditionals();
  };

  this.isConnected = function() {
    return !!this.$socket;
  };

  var scheduledReconnect = null;
  this.reconnect = function() {
    // This function can be invoked directly even if there's a scheduled
    // reconnect, so be sure to clear any such scheduled reconnects.
    clearTimeout(scheduledReconnect);

    if (this.isConnected())
      throw "Attempted to reconnect, but already connected.";

    this.$socket = this.createSocket();
    this.$initialInput = $.extend({}, this.$inputValues);
    this.$updateConditionals();
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
    var hasOpened = false;
    socket.onopen = function() {
      hasOpened = true;

      $(document).trigger({
        type: 'shiny:connected',
        socket: socket
      });

      self.onConnected();

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
    // Called when a successfully-opened websocket is closed, or when an
    // attempt to open a connection fails.
    socket.onclose = function() {
      // These things are needed only if we've successfully opened the
      // websocket.
      if (hasOpened) {
        $(document).trigger({
          type: 'shiny:disconnected',
          socket: socket
        });

        self.$notifyDisconnected();
      }

      self.onDisconnected(); // Must be run before self.$removeSocket()
      self.$removeSocket();
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
    if (window.parent) {
      window.parent.postMessage("disconnected", "*");
    }
  };

  this.$removeSocket = function() {
    this.$socket = null;
  };

  this.$scheduleReconnect = function(delay) {
    var self = this;
    scheduledReconnect = setTimeout(function() { self.reconnect(); }, delay);
  };

  // How long should we wait before trying the next reconnection?
  // The delay will increase with subsequent attempts.
  // .next: Return the time to wait for next connection, and increment counter.
  // .reset: Reset the attempt counter.
  var reconnectDelay = (function() {
    var attempts = 0;
    // Time to wait before each reconnection attempt. If we go through all of
    // these values, repeated use the last one. Add 500ms to each one so that
    // in the last 0.5s, it shows "..."
    var delays = [1500, 1500, 2500, 2500, 5500, 5500, 10500];

    return {
      next: function() {
        var i = attempts;
        // Instead of going off the end, use the last one
        if (i >= delays.length) {
          i = delays.length - 1;
        }

        attempts++;
        return delays[i];
      },
      reset: function() {
        attempts = 0;
      }
    };
  })();

  this.onDisconnected = function() {
    // Add gray-out overlay, if not already present
    var $overlay = $('#shiny-disconnected-overlay');
    if ($overlay.length === 0) {
      $(document.body).append('<div id="shiny-disconnected-overlay"></div>');
    }

    // To try a reconnect, both the app (this.$allowReconnect) and the
    // server (this.$socket.allowReconnect) must allow reconnections, or
    // session$allowReconnect("force") was called. The "force" option should
    // only be used for testing.
    if ((this.$allowReconnect === true && this.$socket.allowReconnect === true) ||
        this.$allowReconnect === "force")
    {
      var delay = reconnectDelay.next();
      exports.showReconnectDialog(delay);
      this.$scheduleReconnect(delay);
    }
  };

  this.onConnected = function() {
    $('#shiny-disconnected-overlay').remove();
    exports.hideReconnectDialog();
    reconnectDelay.reset();
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
    var evt = jQuery.Event('shiny:error');
    evt.name = name;
    evt.error = error;
    evt.binding = binding;
    $(binding ? binding.el : document).trigger(evt);
    if (!evt.isDefaultPrevented() && binding && binding.onValueError) {
      binding.onValueError(evt.error);
    }
  };

  this.receiveOutput = function(name, value) {
    var binding = this.$bindings[name];
    var evt = jQuery.Event('shiny:value');
    evt.name = name;
    evt.value = value;
    evt.binding = binding;

    if (this.$values[name] === value) {
      $(binding ? binding.el : document).trigger(evt);
      return undefined;
    }

    this.$values[name] = value;
    delete this.$errors[name];

    $(binding ? binding.el : document).trigger(evt);

    if (!evt.isDefaultPrevented() && binding) {
      binding.onValueChange(evt.value);
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


  // Narrows a scopeComponent -- an input or output object -- to one constrained
  // by nsPrefix. Returns a new object with keys removed and renamed as
  // necessary.
  function narrowScopeComponent(scopeComponent, nsPrefix) {
    return Object.keys(scopeComponent)
      .filter(k => k.indexOf(nsPrefix) === 0)
      .map(k => ({[k.substring(nsPrefix.length)]: scopeComponent[k]}))
      .reduce((obj, pair) => $.extend(obj, pair),
              {});
  }

  // Narrows a scope -- an object with input and output "subComponents" -- to
  // one constrained by the nsPrefix string.
  //
  // If nsPrefix is null or empty, returns scope without modification.
  //
  // Otherwise, returns a new object with keys in subComponents removed and
  // renamed as necessary.
  function narrowScope(scope, nsPrefix) {
    return nsPrefix ? {
      input: narrowScopeComponent(scope.input, nsPrefix),
      output: narrowScopeComponent(scope.output, nsPrefix)
    } : scope;
  }

  this.$updateConditionals = function() {
    $(document).trigger({
      type: 'shiny:conditional'
    });

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

      var nsPrefix = el.attr('data-ns-prefix');
      var nsScope = narrowScope(scope, nsPrefix);
      var show = condFunc(nsScope);
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
    // Remove any previously defined handlers so that only the most recent one
    // will be called
    if (customMessageHandlers[type]) {
      var typeIdx = customMessageHandlerOrder.indexOf(type);
      if (typeIdx !== -1) {
        customMessageHandlerOrder.splice(typeIdx, 1);
        delete customMessageHandlers[type];
      }
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

  this.dispatchMessage = function(data) {
    var msgObj = {};
    if(typeof data === "string") {
      msgObj = JSON.parse(data);
    } else { // data is arraybuffer
      var len = new DataView(data,0,1).getUint8(0);
      var typedv = new DataView(data,1,len);
      var typebuf = [];
      for(var i=0; i<len; i++){
        typebuf.push(String.fromCharCode(typedv.getUint8(i)));
      }
      var type = typebuf.join("");
      data = data.slice(len+1);
      msgObj.custom = {};
      msgObj.custom[type] = data;
    }

    var evt = jQuery.Event('shiny:message');
    evt.message = msgObj;
    $(document).trigger(evt);
    if (evt.isDefaultPrevented()) return;

    // Send msgObj.foo and msgObj.bar to appropriate handlers
    this._sendMessagesToHandlers(evt.message, messageHandlers, messageHandlerOrder);

    this.$updateConditionals();
  };

  // A function for sending messages to the appropriate handlers.
  // - msgObj: the object containing messages, with format {msgObj.foo, msObj.bar
  this._sendMessagesToHandlers = function(msgObj, handlers, handlerOrder) {
    // Dispatch messages to handlers, if handler is present
    for (let i = 0; i < handlerOrder.length; i++) {
      var msgType = handlerOrder[i];
      if (msgObj.hasOwnProperty(msgType)) {
        // Execute each handler with 'this' referring to the present value of
        // 'this'
        handlers[msgType].call(this, msgObj[msgType]);
      }
    }
  };

  // Message handlers =====================================================

  addMessageHandler('values', function(message) {
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
        var el = $obj[0];
        var evt = jQuery.Event('shiny:updateinput');
        evt.message = message[i].message;
        evt.binding = inputBinding;
        $(el).trigger(evt);
        if (!evt.isDefaultPrevented())
          inputBinding.receiveMessage(el, evt.message);
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

  addMessageHandler('notification', function(message) {
    if (message.type === 'show')
      exports.notifications.show(message.message);
    else if (message.type === 'remove')
      exports.notifications.remove(message.message);
    else
      throw('Unkown notification type: ' + message.type);
  });

  addMessageHandler('modal', function(message) {
    if (message.type === 'show')
      exports.modal.show(message.message);
    else if (message.type === 'remove')
      exports.modal.remove(); // For 'remove', message content isn't used
    else
      throw('Unkown modal type: ' + message.type);
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

  addMessageHandler('allowReconnect', function(message) {
    if (message === true || message === false || message === "force") {
      this.$allowReconnect = message;
    } else {
      throw "Invalid value for allowReconnect: " + message;
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
    this.config = {workerId: message.workerId, sessionId: message.sessionId};
    if (message.user) exports.user = message.user;
    $(document).trigger('shiny:sessioninitialized');
  });

  addMessageHandler('busy', function(message) {
    if (message === 'busy') {
      $(document.documentElement).addClass('shiny-busy');
      $(document).trigger('shiny:busy');
    } else if (message === 'idle') {
      $(document.documentElement).removeClass('shiny-busy');
      $(document).trigger('shiny:idle');
    }
  });

  addMessageHandler('recalculating', function(message) {
    if (message.hasOwnProperty('name') && message.hasOwnProperty('status')) {
      var binding = this.$bindings[message.name];
      $(binding ? binding.el : null).trigger({
        type: 'shiny:' + message.status
      });
    }
  });

  addMessageHandler('reload', function(message) {
    window.location.reload();
  });

  addMessageHandler('shiny-insert-ui', function(message) {
    var targets = $(message.selector);
    if (targets.length === 0) {
      // render the HTML and deps to a null target, so
      // the side-effect of rendering the deps, singletons,
      // and <head> still occur
      console.warn('The selector you chose ("' + message.selector +
                   '") could not be found in the DOM.');
      exports.renderHtml(message.content.html, $([]), message.content.deps);
    } else {
      targets.each(function (i, target) {
        exports.renderContent(target, message.content, message.where);
        return message.multiple;
      });
    }
  });

  addMessageHandler('shiny-remove-ui', function(message) {
    var els = $(message.selector);
    els.each(function (i, el) {
      exports.unbindAll(el, true);
      $(el).remove();
      // If `multiple` is false, returning false terminates the function
      // and no other elements are removed; if `multiple` is true,
      // returning true continues removing all remaining elements.
      return message.multiple;
    });
  });

  function getTabset(id) {
    var $tabset = $("#" + $escape(id));
    if ($tabset.length === 0)
      throw "There is no tabsetPanel (or navbarPage or navlistPanel) " +
            "with id equal to '" + id + "'";
    return $tabset;
  }

  function getTabContent($tabset) {
    var tabsetId = $tabset.attr("data-tabsetid");
    var $tabContent = $("div.tab-content[data-tabsetid='" +
                        $escape(tabsetId) + "']");
    return $tabContent;
  }

  function getTargetTabs($tabset, $tabContent, target) {
    var dataValue = "[data-value='" + $escape(target) + "']";
    var $aTag = $tabset.find("a" + dataValue);
    var $liTag = $aTag.parent();
    if ($liTag.length === 0) {
      throw "There is no tabPanel (or navbarMenu) with value" +
            " (or menuName) equal to '" + target + "'";
    }
    var $liTags = [];
    var $divTags = [];

    if ($aTag.attr("data-toggle") === "dropdown") {
      // dropdown
      var $dropdownTabset = $aTag.find("+ ul.dropdown-menu");
      var dropdownId = $dropdownTabset.attr("data-tabsetid");

      var $dropdownLiTags = $dropdownTabset.find("a[data-toggle='tab']").parent("li");
      $dropdownLiTags.each(function (i, el) {
        $liTags.push($(el));
      });
      var selector = "div.tab-pane[id^='tab-" + $escape(dropdownId) + "']";
      var $dropdownDivs = $tabContent.find(selector);
      $dropdownDivs.each(function (i, el) {
        $divTags.push($(el));
      });

    }
    else {
      // regular tab
      $divTags.push($tabContent.find("div" + dataValue));
    }
    return { $liTag: $liTag, $liTags: $liTags, $divTags: $divTags };
  }

  addMessageHandler("shiny-insert-tab", function(message) {
    var $parentTabset = getTabset(message.inputId);
    var $tabset = $parentTabset;
    var $tabContent = getTabContent($tabset);
    var tabsetId = $parentTabset.attr("data-tabsetid");

    var $divTag = $(message.divTag.html);
    var $liTag = $(message.liTag.html);
    var $aTag = $liTag.find("> a");

    // Unless the item is being prepended/appended, the target tab
    // must be provided
    var target = null;
    var $targetLiTag = null;
    if (message.target !== null) {
      target = getTargetTabs($tabset, $tabContent, message.target);
      $targetLiTag = target.$liTag;
    }

    // If the item is to be placed inside a navbarMenu (dropdown),
    // change the value of $tabset from the parent's ul tag to the
    // dropdown's ul tag
    var dropdown = getDropdown();
    if (dropdown !== null) {
      if ($aTag.attr("data-toggle") === "dropdown")
        throw "Cannot insert a navbarMenu inside another one";
      $tabset = dropdown.$tabset;
      tabsetId = dropdown.id;
    }

    // For regular tab items, fix the href (of the li > a tag)
    // and the id (of the div tag). This does not apply to plain
    // text items (which function as dividers and headers inside
    // navbarMenus) and whole navbarMenus (since those get
    // constructed from scratch on the R side and therefore
    // there are no ids that need matching)
    if ($aTag.attr("data-toggle") === "tab") {
      var index = getTabIndex($tabset, tabsetId);
      var tabId = "tab-" + tabsetId + "-" + index;
      $liTag.find("> a").attr("href", "#" + tabId);
      $divTag.attr("id", tabId);
    }

    // actually insert the item into the right place
    if (message.position === "before") {
      if ($targetLiTag) {
        $targetLiTag.before($liTag);
      } else {
        $tabset.append($liTag);
      }
    } else if (message.position === "after") {
      if ($targetLiTag) {
        $targetLiTag.after($liTag);
      } else {
        $tabset.prepend($liTag);
      }
    }

    exports.renderContent($liTag[0], {html: $liTag.html(), deps: message.liTag.deps});
    // jcheng 2017-07-28: This next part might look a little insane versus the
    // more obvious `$tabContent.append($divTag);`, but there's a method to the
    // madness.
    //
    // 1) We need to load the dependencies, and this needs to happen before
    //    any scripts in $divTag get a chance to run.
    // 2) The scripts in $divTag need to run only once.
    // 3) The contents of $divTag need to be sent through renderContent so that
    //    singletons may be registered and/or obeyed, and so that inputs/outputs
    //    may be bound.
    //
    // Add to these constraints these facts:
    //
    // A) The (non-jQuery) DOM manipulation functions don't cause scripts to
    //    run, but the jQuery functions all do.
    // B) renderContent must be called on an element that's attached to the
    //    document.
    // C) $divTag may be of length > 1 (e.g. navbarMenu). I also noticed text
    //    elements consisting of just "\n" being included in the nodeset of
    //    $divTag.
    // D) renderContent has a bug where only position "replace" (the default)
    //    uses the jQuery functions, so other positions like "beforeend" will
    //    prevent child script tags from running.
    //
    // In theory the same problem exists for $liTag but since that content is
    // much less likely to include arbitrary scripts, we're skipping it.
    //
    // This code could be nicer if we didn't use renderContent, but rather the
    // lower-level functions that renderContent uses. Like if we pre-process
    // the value of message.divTag.html for singletons, we could do that, then
    // render dependencies, then do $tabContent.append($divTag).
    exports.renderContent($tabContent[0], {html: "", deps: message.divTag.deps}, "beforeend");
    $divTag.get().forEach(el => {
      // Must not use jQuery for appending el to the doc, we don't want any
      // scripts to run (since they will run when renderContent takes a crack).
      $tabContent[0].appendChild(el);
      // If `el` itself is a script tag, this approach won't work (the script
      // won't be run), since we're only sending innerHTML through renderContent
      // and not the whole tag. That's fine in this case because we control the
      // R code that generates this HTML, and we know that the element is not
      // a script tag.
      exports.renderContent(el, el.innerHTML || el.textContent);
    });

    if (message.select) {
      $liTag.find("a").tab("show");
    }

    /* Barbara -- August 2017
    Note: until now, the number of tabs in a tabsetPanel (or navbarPage
    or navlistPanel) was always fixed. So, an easy way to give an id to
    a tab was simply incrementing a counter. (Just like it was easy to
    give a random 4-digit number to identify the tabsetPanel). Now that
    we're introducing dynamic tabs, we must retrieve these numbers and
    fix the dummy id given to the tab in the R side -- there, we always
    set the tab id (counter dummy) to "id" and the tabset id to "tsid")
    */
    function getTabIndex($tabset, tabsetId) {
      // The 0 is to ensure this works for empty tabsetPanels as well
      var existingTabIds = [0];
      // loop through all existing tabs, find the one with highest id
      // (since this is based on a numeric counter), and increment
      $tabset.find("> li").each(function() {
        var $tab = $(this).find("> a[data-toggle='tab']");
        if ($tab.length > 0) {
          // remove leading url if it exists. (copy of bootstrap url stripper)
          var href = $tab.attr("href").replace(/.*(?=#[^\s]+$)/, '');
          // remove tab id to get the index
          var index = href.replace("#tab-" + tabsetId + "-", "");
          existingTabIds.push(Number(index));
        }
      });
      return Math.max.apply(null, existingTabIds) + 1;
    }

    // Finds out if the item will be placed inside a navbarMenu
    // (dropdown). If so, returns the dropdown tabset (ul tag)
    // and the dropdown tabsetid (to be used to fix the tab ID)
    function getDropdown() {
      if (message.menuName !== null) {
        // menuName is only provided if the user wants to prepend
        // or append an item inside a navbarMenu (dropdown)
        var $dropdownATag = $("a.dropdown-toggle[data-value='" +
                              $escape(message.menuName) + "']");
        if ($dropdownATag.length === 0) {
          throw "There is no navbarMenu with menuName equal to '" +
                message.menuName + "'";
        }
        var $dropdownTabset = $dropdownATag.find("+ ul.dropdown-menu");
        var dropdownId = $dropdownTabset.attr("data-tabsetid");
        return { $tabset: $dropdownTabset, id: dropdownId };

      } else if (message.target !== null) {
        // if our item is to be placed next to a tab that is inside
        // a navbarMenu, our item will also be inside
        var $uncleTabset = $targetLiTag.parent("ul");
        if ($uncleTabset.hasClass("dropdown-menu")) {
          var uncleId = $uncleTabset.attr("data-tabsetid");
          return { $tabset: $uncleTabset, id: uncleId };
        }
      }
      return null;
    }
  });

  // If the given tabset has no active tabs, select the first one
  function ensureTabsetHasVisibleTab($tabset) {
    if ($tabset.find("li.active").not(".dropdown").length === 0) {
      // Note: destTabValue may be null. We still want to proceed
      // through the below logic and setValue so that the input
      // value for the tabset gets updated (i.e. input$tabsetId
      // should be null if there are no tabs).
      let destTabValue = getFirstTab($tabset);
      let inputBinding = $tabset.data('shiny-input-binding');
      let evt = jQuery.Event('shiny:updateinput');
      evt.binding = inputBinding;
      $tabset.trigger(evt);
      inputBinding.setValue($tabset[0], destTabValue);
    }
  }

  // Given a tabset ul jquery object, return the value of the first tab
  // (in document order) that's visible and able to be selected.
  function getFirstTab($ul) {
    return $ul.find("li:visible a[data-toggle='tab']")
      .first()
      .attr("data-value") || null;
  }

  function tabApplyFunction(target, func, liTags = false) {
    $.each(target, function(key, el) {
      if (key === "$liTag") {
        // $liTag is always just one jQuery element
        func(el);
      }
      else if (key === "$divTags") {
        // $divTags is always an array (even if length = 1)
        $.each(el, function(i, div) { func(div); });

      } else if (liTags && key === "$liTags") {
        // $liTags is always an array (even if length = 0)
        $.each(el, function(i, div) { func(div); });
      }
    });
  }

  addMessageHandler("shiny-remove-tab", function(message) {
    var $tabset = getTabset(message.inputId);
    var $tabContent = getTabContent($tabset);
    var target = getTargetTabs($tabset, $tabContent, message.target);

    tabApplyFunction(target, removeEl);

    ensureTabsetHasVisibleTab($tabset);

    function removeEl($el) {
      exports.unbindAll($el, true);
      $el.remove();
    }
  });

  addMessageHandler("shiny-change-tab-visibility", function(message) {
    var $tabset = getTabset(message.inputId);
    var $tabContent = getTabContent($tabset);
    var target = getTargetTabs($tabset, $tabContent, message.target);

    tabApplyFunction(target, changeVisibility, true);

    ensureTabsetHasVisibleTab($tabset);

    function changeVisibility($el) {
      if (message.type === "show") $el.css("display", "");
      else if (message.type === "hide") {
        $el.hide();
        $el.removeClass("active");
      }
    }
  });

  addMessageHandler('updateQueryString', function(message) {

    // leave the bookmarking code intact
    if (message.mode === "replace") {
      window.history.replaceState(null, null, message.queryString);
      return;
    }

    var what = null;
    if (message.queryString.charAt(0) === "#") what = "hash";
    else if (message.queryString.charAt(0) === "?") what = "query";
    else throw "The 'query' string must start with either '?' " +
               "(to update the query string) or with '#' (to " +
               "update the hash).";

    var path = window.location.pathname;
    var oldQS = window.location.search;
    var oldHash = window.location.hash;

    /* Barbara -- December 2016
    Note: we could check if the new QS and/or hash are different
    from the old one(s) and, if not, we could choose not to push
    a new state (whether or not we would replace it is moot/
    inconsequential). However, I think that it is better to
    interpret each call to `updateQueryString` as representing
    new state (even if the message.queryString is the same), so
    that check isn't even performed as of right now.
    */

    var relURL = path;
    if (what === "query") relURL += message.queryString;
    else relURL += oldQS + message.queryString; // leave old QS if it exists
    window.history.pushState(null, null, relURL);

    // for the case when message.queryString has both a query string
    // and a hash (`what = "hash"` allows us to trigger the
    // hashchange event)
    if (message.queryString.indexOf("#") !== -1) what = "hash";

    // for the case when there was a hash before, but there isn't
    // any hash now (e.g. for when only the query string is updated)
    if (window.location.hash !== oldHash) what = "hash";

    // This event needs to be triggered manually because pushState() never
    // causes a hashchange event to be fired,
    if (what === "hash") $(document).trigger("hashchange");
  });

  addMessageHandler("resetBrush", function(message) {
    exports.resetBrush(message.brushId);
  });

  // Progress reporting ====================================================

  var progressHandlers = {
    // Progress for a particular object
    binding: function(message) {
      var key = message.id;
      var binding = this.$bindings[key];
      if (binding) {
        $(binding.el).trigger({
          type: 'shiny:outputinvalidated',
          binding: binding,
          name: key
        });
        if (binding.showProgress) binding.showProgress(true);
      }
    },

    // Open a page-level progress bar
    open: function(message) {
      if (message.style === "notification") {
        // For new-style (starting in Shiny 0.14) progress indicators that use
        // the notification API.

        // Progress bar starts hidden; will be made visible if a value is provided
        // during updates.
        exports.notifications.show({
          html:
            `<div id="shiny-progress-${message.id}" class="shiny-progress-notification">` +
              '<div class="progress progress-striped active" style="display: none;"><div class="progress-bar"></div></div>' +
              '<div class="progress-text">' +
                '<span class="progress-message">message</span> ' +
                '<span class="progress-detail"></span>' +
              '</div>' +
            '</div>',
          id: message.id,
          duration: null
        });

      } else if (message.style === "old") {
        // For old-style (Shiny <=0.13.2) progress indicators.

        // Add progress container (for all progress items) if not already present
        var $container = $('.shiny-progress-container');
        if ($container.length === 0) {
          $container = $('<div class="shiny-progress-container"></div>');
          $(document.body).append($container);
        }

        // Add div for just this progress ID
        var depth = $('.shiny-progress.open').length;
        // The 'bar' class is needed for backward compatibility with Bootstrap 2.
        var $progress = $('<div class="shiny-progress open">' +
          '<div class="progress progress-striped active"><div class="progress-bar bar"></div></div>' +
          '<div class="progress-text">' +
            '<span class="progress-message">message</span>' +
            '<span class="progress-detail"></span>' +
          '</div>' +
          '</div>'
        );

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
      }

    },

    // Update page-level progress bar
    update: function(message) {
      if (message.style === "notification") {
        // For new-style (starting in Shiny 0.14) progress indicators that use
        // the notification API.
        var $progress = $('#shiny-progress-' + message.id);

        if ($progress.length === 0)
          return;

        if (typeof(message.message) !== 'undefined') {
          $progress.find('.progress-message').text(message.message);
        }
        if (typeof(message.detail) !== 'undefined') {
          $progress.find('.progress-detail').text(message.detail);
        }
        if (typeof(message.value) !== 'undefined' && message.value !== null) {
          $progress.find('.progress').show();
          $progress.find('.progress-bar').width((message.value*100) + '%');
        }

      } else if (message.style === "old") {
        // For old-style (Shiny <=0.13.2) progress indicators.

        var $progress = $('#' + message.id + '.shiny-progress');
        if (typeof(message.message) !== 'undefined') {
          $progress.find('.progress-message').text(message.message);
        }
        if (typeof(message.detail) !== 'undefined') {
          $progress.find('.progress-detail').text(message.detail);
        }
        if (typeof(message.value) !== 'undefined' && message.value !== null) {
          $progress.find('.progress').show();
          $progress.find('.bar').width((message.value*100) + '%');
        }

        $progress.fadeIn();
      }

    },

    // Close page-level progress bar
    close: function(message) {
      if (message.style === "notification") {
        exports.notifications.remove(message.id);

      } else if (message.style === "old") {
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
      }
    }
  };

  exports.progressHandlers = progressHandlers;

  // Returns a URL which can be queried to get values from inside the server
  // function. This is enabled with `options(shiny.testmode=TRUE)`.
  this.getTestSnapshotBaseUrl = function({ fullUrl = true } = {})
  {
    const loc = window.location;
    let url = "";

    if (fullUrl) {
      // Strip off everything after last slash in path, like dirname() in R
      url = loc.origin + loc.pathname.replace(/\/[^/]*$/, "");
    }
    url += "/session/" +
      encodeURIComponent(this.config.sessionId) +
      "/dataobj/shinytest?w=" +
      encodeURIComponent(this.config.workerId) +
      "&nonce=" + randomId();

    return url;
  };

}).call(ShinyApp.prototype);



exports.showReconnectDialog = (function() {
  var reconnectTime = null;

  function updateTime() {
    var $time = $("#shiny-reconnect-time");
    // If the time has been removed, exit and don't reschedule this function.
    if ($time.length === 0) return;

    var seconds = Math.floor((reconnectTime - new Date().getTime()) / 1000);
    if (seconds > 0) {
      $time.text(" in " + seconds + "s");
    } else {
      $time.text("...");
    }

    // Reschedule this function after 1 second
    setTimeout(updateTime, 1000);
  }


  return function(delay) {
    reconnectTime = new Date().getTime() + delay;

    // If there's already a reconnect dialog, don't add another
    if ($('#shiny-reconnect-text').length > 0)
      return;

    var html = '<span id="shiny-reconnect-text">Attempting to reconnect</span>' +
               '<span id="shiny-reconnect-time"></span>';
    var action = '<a id="shiny-reconnect-now" href="#" onclick="Shiny.shinyapp.reconnect();">Try now</a>';

    exports.notifications.show({
      id: "reconnect",
      html: html,
      action: action,
      duration: null,
      closeButton: false,
      type: 'warning'
    });

    updateTime();
  };
})();

exports.hideReconnectDialog = function() {
  exports.notifications.remove("reconnect");
};
