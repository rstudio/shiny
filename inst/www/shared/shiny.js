'use strict';

var _typeof = typeof Symbol === "function" && typeof Symbol.iterator === "symbol" ? function (obj) { return typeof obj; } : function (obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol ? "symbol" : typeof obj; };

//---------------------------------------------------------------------
// Source file: ../srcjs/_start.js

(function () {
  var $ = jQuery;

  var exports = window.Shiny = window.Shiny || {};

  $(document).on('submit', 'form:not([action])', function (e) {
    e.preventDefault();
  });

  //---------------------------------------------------------------------
  // Source file: ../srcjs/utils.js

  function escapeHTML(str) {
    return str.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/"/g, "&quot;").replace(/'/g, "&#039;").replace(/\//g, "&#x2F;");
  }

  function randomId() {
    return Math.floor(0x100000000 + Math.random() * 0xF00000000).toString(16);
  }

  function strToBool(str) {
    if (!str || !str.toLowerCase) return undefined;

    switch (str.toLowerCase()) {
      case 'true':
        return true;
      case 'false':
        return false;
      default:
        return undefined;
    }
  }

  // A wrapper for getComputedStyle that is compatible with older browsers.
  // This is significantly faster than jQuery's .css() function.
  function getStyle(el, styleProp) {
    var x;
    if (el.currentStyle) x = el.currentStyle[styleProp];else if (window.getComputedStyle) {
      // getComputedStyle can return null when we're inside a hidden iframe on
      // Firefox; don't attempt to retrieve style props in this case.
      // https://bugzilla.mozilla.org/show_bug.cgi?id=548397
      var style = document.defaultView.getComputedStyle(el, null);
      if (style) x = style.getPropertyValue(styleProp);
    }
    return x;
  }

  // Convert a number to a string with leading zeros
  function padZeros(n, digits) {
    var str = n.toString();
    while (str.length < digits) {
      str = "0" + str;
    }return str;
  }

  // Take a string with format "YYYY-MM-DD" and return a Date object.
  // IE8 and QTWebKit don't support YYYY-MM-DD, but they support YYYY/MM/DD
  function parseDate(dateString) {
    var date = new Date(dateString);
    if (isNaN(date)) date = new Date(dateString.replace(/-/g, "/"));
    return date;
  }

  // Given a Date object, return a string in yyyy-mm-dd format, using the
  // UTC date. This may be a day off from the date in the local time zone.
  function formatDateUTC(date) {
    if (date instanceof Date) {
      return date.getUTCFullYear() + '-' + padZeros(date.getUTCMonth() + 1, 2) + '-' + padZeros(date.getUTCDate(), 2);
    } else {
      return null;
    }
  }

  // Given an element and a function(width, height), returns a function(). When
  // the output function is called, it calls the input function with the offset
  // width and height of the input element--but only if the size of the element
  // is non-zero and the size is different than the last time the output
  // function was called.
  //
  // Basically we are trying to filter out extraneous calls to func, so that
  // when the window size changes or whatever, we don't run resize logic for
  // elements that haven't actually changed size or aren't visible anyway.
  function makeResizeFilter(el, func) {
    var lastSize = {};
    return function () {
      var size = { w: el.offsetWidth, h: el.offsetHeight };
      if (size.w === 0 && size.h === 0) return;
      if (size.w === lastSize.w && size.h === lastSize.h) return;
      lastSize = size;
      func(size.w, size.h);
    };
  }

  var _BlobBuilder = window.BlobBuilder || window.WebKitBlobBuilder || window.MozBlobBuilder || window.MSBlobBuilder;

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
    } catch (e) {
      var blobBuilder = new _BlobBuilder();
      $.each(parts, function (i, part) {
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
    return function (scope) {
      return func.call(scope);
    };
  }

  function asArray(value) {
    if (value === null || value === undefined) return [];
    if ($.isArray(value)) return value;
    return [value];
  }

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
        } else {
          sorted.push(b[ib++]);
        }
      }
      while (ia < a.length) {
        sorted.push(a[ia++]);
      }while (ib < b.length) {
        sorted.push(b[ib++]);
      }return sorted;
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

  // Escape jQuery selector metacharacters: !"#$%&'()*+,./:;<=>?@[\]^`{|}~
  var $escape = exports.$escape = function (val) {
    return val.replace(/([!"#$%&'()*+,.\/:;<=>?@\[\\\]^`{|}~])/g, '\\$1');
  };

  //---------------------------------------------------------------------
  // Source file: ../srcjs/browser.js

  var browser = function () {

    var isQt = false;
    // For easy handling of Qt quirks using CSS
    if (/\bQt\//.test(window.navigator.userAgent)) {
      $(document.documentElement).addClass('qt');
      isQt = true;
    }

    // Enable special treatment for Qt 5 quirks on Linux
    if (/\bQt\/5/.test(window.navigator.userAgent) && /Linux/.test(window.navigator.userAgent)) {
      $(document.documentElement).addClass('qt5');
    }

    // Detect IE information
    var isIE = navigator.appName === 'Microsoft Internet Explorer';

    function getIEVersion() {
      var rv = -1;
      if (isIE) {
        var ua = navigator.userAgent;
        var re = new RegExp("MSIE ([0-9]{1,}[\\.0-9]{0,})");
        if (re.exec(ua) !== null) rv = parseFloat(RegExp.$1);
      }
      return rv;
    }

    return {
      isQt: isQt,
      isIE: isIE,
      IEVersion: getIEVersion()
    };
  }();

  //---------------------------------------------------------------------
  // Source file: ../srcjs/input_rate.js

  var Invoker = function Invoker(target, func) {
    this.target = target;
    this.func = func;
  };

  (function () {
    this.normalCall = this.immediateCall = function () {
      this.func.apply(this.target, arguments);
    };
  }).call(Invoker.prototype);

  var Debouncer = function Debouncer(target, func, delayMs) {
    this.target = target;
    this.func = func;
    this.delayMs = delayMs;

    this.timerId = null;
    this.args = null;
  };

  (function () {
    this.normalCall = function () {
      var self = this;

      this.$clearTimer();
      this.args = arguments;

      this.timerId = setTimeout(function () {
        // IE8 doesn't reliably clear timeout, so this additional
        // check is needed
        if (self.timerId === null) return;
        self.$clearTimer();
        self.$invoke();
      }, this.delayMs);
    };
    this.immediateCall = function () {
      this.$clearTimer();
      this.args = arguments;
      this.$invoke();
    };
    this.isPending = function () {
      return this.timerId !== null;
    };
    this.$clearTimer = function () {
      if (this.timerId !== null) {
        clearTimeout(this.timerId);
        this.timerId = null;
      }
    };
    this.$invoke = function () {
      this.func.apply(this.target, this.args);
      this.args = null;
    };
  }).call(Debouncer.prototype);

  var Throttler = function Throttler(target, func, delayMs) {
    this.target = target;
    this.func = func;
    this.delayMs = delayMs;

    this.timerId = null;
    this.args = null;
  };

  (function () {
    this.normalCall = function () {
      var self = this;

      this.args = arguments;
      if (this.timerId === null) {
        this.$invoke();
        this.timerId = setTimeout(function () {
          // IE8 doesn't reliably clear timeout, so this additional
          // check is needed
          if (self.timerId === null) return;
          self.$clearTimer();
          if (self.args) self.normalCall.apply(self, self.args);
        }, this.delayMs);
      }
    };
    this.immediateCall = function () {
      this.$clearTimer();
      this.args = arguments;
      this.$invoke();
    };
    this.isPending = function () {
      return this.timerId !== null;
    };
    this.$clearTimer = function () {
      if (this.timerId !== null) {
        clearTimeout(this.timerId);
        this.timerId = null;
      }
    };
    this.$invoke = function () {
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
    return function () {
      self = this;
      args = arguments;
      if (timerId !== null) {
        clearTimeout(timerId);
        timerId = null;
      }
      timerId = setTimeout(function () {
        // IE8 doesn't reliably clear timeout, so this additional
        // check is needed
        if (timerId === null) return;
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
        timerId = setTimeout(function () {
          // When time expires, clear the timer; and if
          // there has been a call in the meantime, repeat.
          timerId = null;
          if (executionPending) {
            executionPending = false;
            throttled.apply(self, args);
          }
        }, threshold);
        func.apply(this, arguments);
      } else {
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
  var InputBatchSender = function InputBatchSender(shinyapp) {
    this.shinyapp = shinyapp;
    this.timerId = null;
    this.pendingData = {};
    this.reentrant = false;
    this.lastChanceCallback = [];
  };
  (function () {
    this.setInput = function (name, value) {
      var self = this;

      this.pendingData[name] = value;
      if (!this.timerId && !this.reentrant) {
        this.timerId = setTimeout(function () {
          self.reentrant = true;
          try {
            $.each(self.lastChanceCallback, function (i, callback) {
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

  var InputNoResendDecorator = function InputNoResendDecorator(target, initialValues) {
    this.target = target;
    this.lastSentValues = initialValues || {};
  };
  (function () {
    this.setInput = function (name, value) {
      var jsonValue = JSON.stringify(value);
      if (this.lastSentValues[name] === jsonValue) return;
      this.lastSentValues[name] = jsonValue;
      this.target.setInput(name, value);
    };
    this.reset = function (values) {
      values = values || {};
      var strValues = {};
      $.each(values, function (key, value) {
        strValues[key] = JSON.stringify(value);
      });
      this.lastSentValues = strValues;
    };
  }).call(InputNoResendDecorator.prototype);

  var InputDeferDecorator = function InputDeferDecorator(target) {
    this.target = target;
    this.pendingInput = {};
  };
  (function () {
    this.setInput = function (name, value) {
      if (/^\./.test(name)) this.target.setInput(name, value);else this.pendingInput[name] = value;
    };
    this.submit = function () {
      for (var name in this.pendingInput) {
        if (this.pendingInput.hasOwnProperty(name)) this.target.setInput(name, this.pendingInput[name]);
      }
    };
  }).call(InputDeferDecorator.prototype);

  var InputEventDecorator = function InputEventDecorator(target) {
    this.target = target;
  };
  (function () {
    this.setInput = function (name, value, immediate) {
      var evt = jQuery.Event("shiny:inputchanged");
      var name2 = name.split(':');
      evt.name = name2[0];
      evt.inputType = name2.length > 1 ? name2[1] : '';
      evt.value = value;
      $(document).trigger(evt);
      if (!evt.isDefaultPrevented()) {
        name = evt.name;
        if (evt.inputType !== '') name += ':' + evt.inputType;
        this.target.setInput(name, evt.value, immediate);
      }
    };
  }).call(InputEventDecorator.prototype);

  var InputRateDecorator = function InputRateDecorator(target) {
    this.target = target;
    this.inputRatePolicies = {};
  };
  (function () {
    this.setInput = function (name, value, immediate) {
      this.$ensureInit(name);
      if (immediate) this.inputRatePolicies[name].immediateCall(name, value, immediate);else this.inputRatePolicies[name].normalCall(name, value, immediate);
    };
    this.setRatePolicy = function (name, mode, millis) {
      if (mode === 'direct') {
        this.inputRatePolicies[name] = new Invoker(this, this.$doSetInput);
      } else if (mode === 'debounce') {
        this.inputRatePolicies[name] = new Debouncer(this, this.$doSetInput, millis);
      } else if (mode === 'throttle') {
        this.inputRatePolicies[name] = new Throttler(this, this.$doSetInput, millis);
      }
    };
    this.$ensureInit = function (name) {
      if (!(name in this.inputRatePolicies)) this.setRatePolicy(name, 'direct');
    };
    this.$doSetInput = function (name, value) {
      this.target.setInput(name, value);
    };
  }).call(InputRateDecorator.prototype);

  //---------------------------------------------------------------------
  // Source file: ../srcjs/shinyapp.js

  var ShinyApp = function ShinyApp() {
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

    this.$allowReconnect = false;
  };

  (function () {

    this.connect = function (initialInput) {
      if (this.$socket) throw "Connect was already called on this application object";

      $.extend(initialInput, {
        // IE8 and IE9 have some limitations with data URIs
        ".clientdata_allowDataUriScheme": typeof WebSocket !== 'undefined'
      });

      this.$socket = this.createSocket();
      this.$initialInput = initialInput;
      $.extend(this.$inputValues, initialInput);

      this.$updateConditionals();
    };

    this.isConnected = function () {
      return !!this.$socket;
    };

    var scheduledReconnect = null;
    this.reconnect = function () {
      // This function can be invoked directly even if there's a scheduled
      // reconnect, so be sure to clear any such scheduled reconnects.
      clearTimeout(scheduledReconnect);

      if (this.isConnected()) throw "Attempted to reconnect, but already connected.";

      this.$socket = this.createSocket();
      this.$initialInput = $.extend({}, this.$inputValues);
      this.$updateConditionals();
    };

    this.createSocket = function () {
      var self = this;

      var createSocketFunc = exports.createSocket || function () {
        var protocol = 'ws:';
        if (window.location.protocol === 'https:') protocol = 'wss:';

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
        if (!/\/$/.test(defaultPath)) defaultPath += '/';
        defaultPath += 'websocket/';

        var ws = new WebSocket(protocol + '//' + window.location.host + defaultPath);
        ws.binaryType = 'arraybuffer';

        return ws;
      };

      var socket = createSocketFunc();
      var hasOpened = false;
      socket.onopen = function () {
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
      socket.onmessage = function (e) {
        self.dispatchMessage(e.data);
      };
      // Called when a successfully-opened websocket is closed, or when an
      // attempt to open a connection fails.
      socket.onclose = function () {
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

    this.sendInput = function (values) {
      var msg = JSON.stringify({
        method: 'update',
        data: values
      });

      this.$sendMsg(msg);

      $.extend(this.$inputValues, values);
      this.$updateConditionals();
    };

    this.$notifyDisconnected = function () {

      // function to normalize hostnames
      var normalize = function normalize(hostname) {
        if (hostname == "127.0.0.1") return "localhost";else return hostname;
      };

      // Send a 'disconnected' message to parent if we are on the same domin
      var parentUrl = parent !== window ? document.referrer : null;
      if (parentUrl) {
        // parse the parent href
        var a = document.createElement('a');
        a.href = parentUrl;

        // post the disconnected message if the hostnames are the same
        if (normalize(a.hostname) == normalize(window.location.hostname)) {
          var protocol = a.protocol.replace(':', ''); // browser compatability
          var origin = protocol + '://' + a.hostname;
          if (a.port) origin = origin + ':' + a.port;
          parent.postMessage('disconnected', origin);
        }
      }
    };

    this.$removeSocket = function () {
      this.$socket = null;
    };

    this.$scheduleReconnect = function (delay) {
      var self = this;
      scheduledReconnect = setTimeout(function () {
        self.reconnect();
      }, delay);
    };

    // How long should we wait before trying the next reconnection?
    // The delay will increase with subsequent attempts.
    // .next: Return the time to wait for next connection, and increment counter.
    // .reset: Reset the attempt counter.
    var reconnectDelay = function () {
      var attempts = 0;
      // Time to wait before each reconnection attempt. If we go through all of
      // these values, repeated use the last one. Add 500ms to each one so that
      // in the last 0.5s, it shows "..."
      var delays = [1500, 1500, 2500, 2500, 5500, 5500, 10500];

      return {
        next: function next() {
          var i = attempts;
          // Instead of going off the end, use the last one
          if (i >= delays.length) {
            i = delays.length - 1;
          }

          attempts++;
          return delays[i];
        },
        reset: function reset() {
          attempts = 0;
        }
      };
    }();

    this.onDisconnected = function () {
      // Add gray-out overlay, if not already present
      var $overlay = $('#shiny-disconnected-overlay');
      if ($overlay.length === 0) {
        $(document.body).append('<div id="shiny-disconnected-overlay"></div>');
      }

      // To try a reconnect, both the app (this.$allowReconnect) and the
      // server (this.$socket.allowReconnect) must allow reconnections, or
      // session$allowReconnect("force") was called. The "force" option should
      // only be used for testing.
      if (this.$allowReconnect === true && this.$socket.allowReconnect === true || this.$allowReconnect === "force") {
        var delay = reconnectDelay.next();
        exports.showReconnectDialog(delay);
        this.$scheduleReconnect(delay);
      }
    };

    this.onConnected = function () {
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
    this.makeRequest = function (method, args, onSuccess, onError, blobs) {
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

        var uint32_to_buf = function uint32_to_buf(val) {
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

    this.$sendMsg = function (msg) {
      if (!this.$socket.readyState) {
        this.$pendingMessages.push(msg);
      } else {
        this.$socket.send(msg);
      }
    };

    this.receiveError = function (name, error) {
      if (this.$errors[name] === error) return;

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

    this.receiveOutput = function (name, value) {
      if (this.$values[name] === value) return undefined;

      this.$values[name] = value;
      delete this.$errors[name];

      var binding = this.$bindings[name];
      var evt = jQuery.Event('shiny:value');
      evt.name = name;
      evt.value = value;
      evt.binding = binding;
      $(binding ? binding.el : document).trigger(evt);
      if (!evt.isDefaultPrevented() && binding) {
        binding.onValueChange(evt.value);
      }

      return value;
    };

    this.bindOutput = function (id, binding) {
      if (!id) throw "Can't bind an element with no ID";
      if (this.$bindings[id]) throw "Duplicate binding for ID " + id;
      this.$bindings[id] = binding;

      if (this.$values[id] !== undefined) binding.onValueChange(this.$values[id]);else if (this.$errors[id] !== undefined) binding.onValueError(this.$errors[id]);

      return binding;
    };

    this.unbindOutput = function (id, binding) {
      if (this.$bindings[id] === binding) {
        delete this.$bindings[id];
        return true;
      } else {
        return false;
      }
    };

    this.$updateConditionals = function () {
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

      var scope = { input: inputs, output: this.$values };

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
          } else {
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
        throw 'handler for message of type "' + type + '" already added.';
      }
      if (typeof handler !== 'function') {
        throw 'handler must be a function.';
      }
      if (handler.length !== 1) {
        throw 'handler must be a function that takes one argument.';
      }
      messageHandlerOrder.push(type);
      messageHandlers[type] = handler;
    }

    // Adds custom message handler - this one is exposed to the user
    function addCustomMessageHandler(type, handler) {
      if (customMessageHandlers[type]) {
        throw 'handler for message of type "' + type + '" already added.';
      }
      if (typeof handler !== 'function') {
        throw 'handler must be a function.';
      }
      if (handler.length !== 1) {
        throw 'handler must be a function that takes one argument.';
      }

      customMessageHandlerOrder.push(type);
      customMessageHandlers[type] = handler;
    }

    exports.addCustomMessageHandler = addCustomMessageHandler;

    this.dispatchMessage = function (data) {
      var msgObj = {};
      if (typeof data === "string") {
        msgObj = JSON.parse(data);
      } else {
        // data is arraybuffer
        var len = new DataView(data, 0, 1).getUint8(0);
        var typedv = new DataView(data, 1, len);
        var typebuf = [];
        for (var i = 0; i < len; i++) {
          typebuf.push(String.fromCharCode(typedv.getUint8(i)));
        }
        var type = typebuf.join("");
        data = data.slice(len + 1);
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
    this._sendMessagesToHandlers = function (msgObj, handlers, handlerOrder) {
      // Dispatch messages to handlers, if handler is present
      for (var i = 0; i < handlerOrder.length; i++) {
        var msgType = handlerOrder[i];
        if (msgObj.hasOwnProperty(msgType)) {
          // Execute each handler with 'this' referring to the present value of
          // 'this'
          handlers[msgType].call(this, msgObj[msgType]);
        }
      }
    };

    // Message handlers =====================================================

    addMessageHandler('values', function (message) {
      for (var name in this.$bindings) {
        if (this.$bindings.hasOwnProperty(name)) this.$bindings[name].showProgress(false);
      }

      for (var key in message) {
        if (message.hasOwnProperty(key)) this.receiveOutput(key, message[key]);
      }
    });

    addMessageHandler('errors', function (message) {
      for (var key in message) {
        if (message.hasOwnProperty(key)) this.receiveError(key, message[key]);
      }
    });

    addMessageHandler('inputMessages', function (message) {
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
          if (!evt.isDefaultPrevented()) inputBinding.receiveMessage(el, evt.message);
        }
      }
    });

    addMessageHandler('javascript', function (message) {
      /*jshint evil: true */
      eval(message);
    });

    addMessageHandler('console', function (message) {
      for (var i = 0; i < message.length; i++) {
        if (console.log) console.log(message[i]);
      }
    });

    addMessageHandler('progress', function (message) {
      if (message.type && message.message) {
        var handler = progressHandlers[message.type];
        if (handler) handler.call(this, message.message);
      }
    });

    addMessageHandler('notification', function (message) {
      if (message.type === 'show') exports.notifications.show(message.message);else if (message.type === 'remove') exports.notifications.remove(message.message);else throw 'Unkown notification type: ' + message.type;
    });

    addMessageHandler('modal', function (message) {
      if (message.type === 'show') exports.modal.show(message.message);else if (message.type === 'remove') exports.modal.remove(); // For 'remove', message content isn't used
      else throw 'Unkown modal type: ' + message.type;
    });

    addMessageHandler('response', function (message) {
      var requestId = message.tag;
      var request = this.$activeRequests[requestId];
      if (request) {
        delete this.$activeRequests[requestId];
        if ('value' in message) request.onSuccess(message.value);else request.onError(message.error);
      }
    });

    addMessageHandler('allowReconnect', function (message) {
      if (message === true || message === false || message === "force") {
        this.$allowReconnect = message;
      } else {
        throw "Invalid value for allowReconnect: " + message;
      }
    });

    addMessageHandler('custom', function (message) {
      // For old-style custom messages - should deprecate and migrate to new
      // method
      if (exports.oncustommessage) {
        exports.oncustommessage(message);
      }

      // Send messages.foo and messages.bar to appropriate handlers
      this._sendMessagesToHandlers(message, customMessageHandlers, customMessageHandlerOrder);
    });

    addMessageHandler('config', function (message) {
      this.config = message;
    });

    addMessageHandler('busy', function (message) {
      if (message === 'busy') {
        $(document.documentElement).addClass('shiny-busy');
        $(document).trigger('shiny:busy');
      } else if (message === 'idle') {
        $(document.documentElement).removeClass('shiny-busy');
        $(document).trigger('shiny:idle');
      }
    });

    addMessageHandler('recalculating', function (message) {
      if (message.hasOwnProperty('name') && message.hasOwnProperty('status')) {
        var binding = this.$bindings[message.name];
        $(binding ? binding.el : null).trigger({
          type: 'shiny:' + message.status
        });
      }
    });

    addMessageHandler('reload', function (message) {
      window.location.reload();
    });

    addMessageHandler('shiny-insert-ui', function (message) {
      var targets = $(message.selector);
      if (targets.length === 0) {
        // render the HTML and deps to a null target, so
        // the side-effect of rendering the deps, singletons,
        // and <head> still occur
        exports.renderHtml($([]), message.content.html, message.content.deps);
      } else {
        targets.each(function (i, target) {
          exports.renderContent(target, message.content, message.where);
          return message.multiple;
        });
      }
    });

    addMessageHandler('shiny-remove-ui', function (message) {
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

    addMessageHandler('updateQueryString', function (message) {
      window.history.replaceState(null, null, message.queryString);
    });

    addMessageHandler("resetBrush", function (message) {
      exports.resetBrush(message.brushId);
    });

    // Progress reporting ====================================================

    var progressHandlers = {
      // Progress for a particular object
      binding: function binding(message) {
        var key = message.id;
        var binding = this.$bindings[key];
        if (binding && binding.showProgress) {
          binding.showProgress(true);
        }
      },

      // Open a page-level progress bar
      open: function open(message) {
        if (message.style === "notification") {
          // For new-style (starting in Shiny 0.14) progress indicators that use
          // the notification API.

          // Progress bar starts hidden; will be made visible if a value is provided
          // during updates.
          exports.notifications.show({
            html: '<div id="shiny-progress-' + message.id + '" class="shiny-progress-notification">' + '<div class="progress progress-striped active" style="display: none;"><div class="progress-bar"></div></div>' + '<div class="progress-text">' + '<span class="progress-message">message</span> ' + '<span class="progress-detail"></span>' + '</div>' + '</div>',
            id: message.id,
            duration: null
          });
        } else if (message.style === "old") {
          // For old-style (Shiny <=0.13.2) progress indicators.

          // Add progress container (for all progress items) if not already present
          var $container = $('.shiny-progress-container');
          if ($container.length === 0) {
            $container = $('<div class="shiny-progress-container"></div>');
            $('body').append($container);
          }

          // Add div for just this progress ID
          var depth = $('.shiny-progress.open').length;
          // The 'bar' class is needed for backward compatibility with Bootstrap 2.
          var $progress = $('<div class="shiny-progress open">' + '<div class="progress progress-striped active"><div class="progress-bar bar"></div></div>' + '<div class="progress-text">' + '<span class="progress-message">message</span>' + '<span class="progress-detail"></span>' + '</div>' + '</div>');

          $progress.attr('id', message.id);
          $container.append($progress);

          // Stack bars
          var $progressBar = $progress.find('.progress');
          $progressBar.css('top', depth * $progressBar.height() + 'px');

          // Stack text objects
          var $progressText = $progress.find('.progress-text');
          $progressText.css('top', 3 * $progressBar.height() + depth * $progressText.outerHeight() + 'px');

          $progress.hide();
        }
      },

      // Update page-level progress bar
      update: function update(message) {
        if (message.style === "notification") {
          // For new-style (starting in Shiny 0.14) progress indicators that use
          // the notification API.
          var $progress = $('#shiny-progress-' + message.id);

          if ($progress.length === 0) return;

          if (typeof message.message !== 'undefined') {
            $progress.find('.progress-message').text(message.message);
          }
          if (typeof message.detail !== 'undefined') {
            $progress.find('.progress-detail').text(message.detail);
          }
          if (typeof message.value !== 'undefined') {
            if (message.value !== null) {
              $progress.find('.progress').show();
              $progress.find('.progress-bar').width(message.value * 100 + '%');
            } else {
              $progress.find('.progress').hide();
            }
          }
        } else if (message.style === "old") {
          // For old-style (Shiny <=0.13.2) progress indicators.

          var $progress = $('#' + message.id + '.shiny-progress');
          if (typeof message.message !== 'undefined') {
            $progress.find('.progress-message').text(message.message);
          }
          if (typeof message.detail !== 'undefined') {
            $progress.find('.progress-detail').text(message.detail);
          }
          if (typeof message.value !== 'undefined') {
            if (message.value !== null) {
              $progress.find('.progress').show();
              $progress.find('.bar').width(message.value * 100 + '%');
            } else {
              $progress.find('.progress').hide();
            }
          }

          $progress.fadeIn();
        }
      },

      // Close page-level progress bar
      close: function close(message) {
        if (message.style === "notification") {
          exports.notifications.remove(message.id);
        } else if (message.style === "old") {
          var $progress = $('#' + message.id + '.shiny-progress');
          $progress.removeClass('open');

          $progress.fadeOut({
            complete: function complete() {
              $progress.remove();

              // If this was the last shiny-progress, remove container
              if ($('.shiny-progress').length === 0) $('.shiny-progress-container').remove();
            }
          });
        }
      }
    };

    exports.progressHandlers = progressHandlers;
  }).call(ShinyApp.prototype);

  exports.showReconnectDialog = function () {
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

    return function (delay) {
      reconnectTime = new Date().getTime() + delay;

      // If there's already a reconnect dialog, don't add another
      if ($('#shiny-reconnect-text').length > 0) return;

      var html = '<span id="shiny-reconnect-text">Attempting to reconnect</span>' + '<span id="shiny-reconnect-time"></span>';
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
  }();

  exports.hideReconnectDialog = function () {
    exports.notifications.remove("reconnect");
  };

  //---------------------------------------------------------------------
  // Source file: ../srcjs/notifications.js

  exports.notifications = function () {

    // Milliseconds to fade in or out
    var fadeDuration = 250;

    function show() {
      var _ref = arguments.length <= 0 || arguments[0] === undefined ? {} : arguments[0];

      var _ref$html = _ref.html;
      var html = _ref$html === undefined ? '' : _ref$html;
      var _ref$action = _ref.action;
      var action = _ref$action === undefined ? '' : _ref$action;
      var _ref$deps = _ref.deps;
      var deps = _ref$deps === undefined ? [] : _ref$deps;
      var _ref$duration = _ref.duration;
      var duration = _ref$duration === undefined ? 5000 : _ref$duration;
      var _ref$id = _ref.id;
      var id = _ref$id === undefined ? null : _ref$id;
      var _ref$closeButton = _ref.closeButton;
      var closeButton = _ref$closeButton === undefined ? true : _ref$closeButton;
      var _ref$type = _ref.type;
      var type = _ref$type === undefined ? null : _ref$type;

      if (!id) id = randomId();

      // Create panel if necessary
      _createPanel();

      // Get existing DOM element for this ID, or create if needed.
      var $notification = _get(id);
      if ($notification.length === 0) $notification = _create(id);

      // Render html and dependencies
      var newHtml = '<div class="shiny-notification-content-text">' + html + '</div>' + ('<div class="shiny-notification-content-action">' + action + '</div>');
      var $content = $notification.find('.shiny-notification-content');
      exports.renderContent($content, { html: newHtml, deps: deps });

      // Remove any existing classes of the form 'shiny-notification-xxxx'.
      // The xxxx would be strings like 'warning'.
      var classes = $notification.attr('class').split(/\s+/).filter(function (cls) {
        return cls.match(/^shiny-notification-/);
      }).join(' ');
      $notification.removeClass(classes);

      // Add class. 'default' means no additional CSS class.
      if (type && type !== 'default') $notification.addClass('shiny-notification-' + type);

      // Make sure that the presence/absence of close button matches with value
      // of `closeButton`.
      var $close = $notification.find('.shiny-notification-close');
      if (closeButton && $close.length === 0) {
        $notification.append('<div class="shiny-notification-close">&times;</div>');
      } else if (!closeButton && $close.length !== 0) {
        $close.remove();
      }

      // If duration was provided, schedule removal. If not, clear existing
      // removal callback (this happens if a message was first added with
      // a duration, and then updated with no duration).
      if (duration) _addRemovalCallback(id, duration);else _clearRemovalCallback(id);

      return id;
    }

    function remove(id) {
      _get(id).fadeOut(fadeDuration, function () {

        exports.unbindAll(this);
        $(this).remove();

        // If no more notifications, remove the panel from the DOM.
        if (_ids().length === 0) {
          _getPanel().remove();
        }
      });
    }

    // Returns an individual notification DOM object (wrapped in jQuery).
    function _get(id) {
      if (!id) return null;
      return _getPanel().find('#shiny-notification-' + $escape(id));
    }

    // Return array of all notification IDs
    function _ids() {
      return _getPanel().find('.shiny-notification').map(function () {
        return this.id.replace(/shiny-notification-/, '');
      }).get();
    }

    // Returns the notification panel DOM object (wrapped in jQuery).
    function _getPanel() {
      return $('#shiny-notification-panel');
    }

    // Create notifications panel and return the jQuery object. If the DOM
    // element already exists, just return it.
    function _createPanel() {
      var $panel = _getPanel();

      if ($panel.length > 0) return $panel;

      $('body').append('<div id="shiny-notification-panel">');

      return $panel;
    }

    // Create a notification DOM element and return the jQuery object. If the
    // DOM element already exists for the ID, just return it without creating.
    function _create(id) {
      var $notification = _get(id);

      if ($notification.length === 0) {
        $notification = $('<div id="shiny-notification-' + id + '" class="shiny-notification">' + '<div class="shiny-notification-close">&times;</div>' + '<div class="shiny-notification-content"></div>' + '</div>');

        $notification.find('.shiny-notification-close').on('click', function (e) {
          e.preventDefault();
          e.stopPropagation();
          remove(id);
        });

        _getPanel().append($notification);
      }

      return $notification;
    }

    // Add a callback to remove a notification after a delay in ms.
    function _addRemovalCallback(id, delay) {
      // If there's an existing removalCallback, clear it before adding the new
      // one.
      _clearRemovalCallback(id);

      // Attach new removal callback
      var removalCallback = setTimeout(function () {
        remove(id);
      }, delay);
      _get(id).data('removalCallback', removalCallback);
    }

    // Clear a removal callback from a notification, if present.
    function _clearRemovalCallback(id) {
      var $notification = _get(id);
      var oldRemovalCallback = $notification.data('removalCallback');
      if (oldRemovalCallback) {
        clearTimeout(oldRemovalCallback);
      }
    }

    return {
      show: show,
      remove: remove
    };
  }();

  //---------------------------------------------------------------------
  // Source file: ../srcjs/modal.js

  exports.modal = {

    // Show a modal dialog. This is meant to handle two types of cases: one is
    // that the content is a Bootstrap modal dialog, and the other is that the
    // content is non-Bootstrap. Bootstrap modals require some special handling,
    // which is coded in here.
    show: function show() {
      var _ref2 = arguments.length <= 0 || arguments[0] === undefined ? {} : arguments[0];

      var _ref2$html = _ref2.html;
      var html = _ref2$html === undefined ? '' : _ref2$html;
      var _ref2$deps = _ref2.deps;
      var deps = _ref2$deps === undefined ? [] : _ref2$deps;


      // If there was an existing Bootstrap modal, then there will be a modal-
      // backdrop div that was added outside of the modal wrapper, and it must be 
      // removed; otherwise there can be multiple of these divs.
      $('.modal-backdrop').remove();

      // Get existing wrapper DOM element, or create if needed.
      var $modal = $('#shiny-modal-wrapper');
      if ($modal.length === 0) {
        $modal = $('<div id="shiny-modal-wrapper"></div>');
        $('body').append($modal);

        // If the wrapper's content is a Bootstrap modal, then when the inner
        // modal is hidden, remove the entire thing, including wrapper.
        $modal.on('hidden.bs.modal', function () {
          exports.unbindAll($modal);
          $modal.remove();
        });
      }

      // Set/replace contents of wrapper with html.
      exports.renderContent($modal, { html: html, deps: deps });
    },

    remove: function remove() {
      var $modal = $('#shiny-modal-wrapper');

      // Look for a Bootstrap modal and if present, trigger hide event. This will
      // trigger the hidden.bs.modal callback that we set in show(), which unbinds
      // and removes the element.
      if ($modal.find('.modal').length > 0) {
        $modal.find('.modal').modal('hide');
      } else {
        // If not a Bootstrap modal dialog, simply unbind and remove it.
        exports.unbindAll($modal);
        $modal.remove();
      }
    }
  };

  //---------------------------------------------------------------------
  // Source file: ../srcjs/file_processor.js

  // Generic driver class for doing chunk-wise asynchronous processing of a
  // FileList object. Subclass/clone it and override the `on*` functions to
  // make it do something useful.
  var FileProcessor = function FileProcessor(files) {
    this.files = files;
    this.fileIndex = -1;
    // Currently need to use small chunk size because R-Websockets can't
    // handle continuation frames
    this.aborted = false;
    this.completed = false;

    // TODO: Register error/abort callbacks

    this.$run();
  };
  (function () {
    // Begin callbacks. Subclassers/cloners may override any or all of these.
    this.onBegin = function (files, cont) {
      setTimeout(cont, 0);
    };
    this.onFile = function (file, cont) {
      setTimeout(cont, 0);
    };
    this.onComplete = function () {};
    this.onAbort = function () {};
    // End callbacks

    // Aborts processing, unless it's already completed
    this.abort = function () {
      if (this.completed || this.aborted) return;

      this.aborted = true;
      this.onAbort();
    };

    // Returns a bound function that will call this.$run one time.
    this.$getRun = function () {
      var self = this;
      var called = false;
      return function () {
        if (called) return;
        called = true;
        self.$run();
      };
    };

    // This function will be called multiple times to advance the process.
    // It relies on the state of the object's fields to know what to do next.
    this.$run = function () {

      if (this.aborted || this.completed) return;

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

  //---------------------------------------------------------------------
  // Source file: ../srcjs/binding_registry.js

  var BindingRegistry = function BindingRegistry() {
    this.bindings = [];
    this.bindingNames = {};
  };
  (function () {
    this.register = function (binding, bindingName, priority) {
      var bindingObj = { binding: binding, priority: priority || 0 };
      this.bindings.unshift(bindingObj);
      if (bindingName) {
        this.bindingNames[bindingName] = bindingObj;
        binding.name = bindingName;
      }
    };
    this.setPriority = function (bindingName, priority) {
      var bindingObj = this.bindingNames[bindingName];
      if (!bindingObj) throw "Tried to set priority on unknown binding " + bindingName;
      bindingObj.priority = priority || 0;
    };
    this.getPriority = function (bindingName) {
      var bindingObj = this.bindingNames[bindingName];
      if (!bindingObj) return false;
      return bindingObj.priority;
    };
    this.getBindings = function () {
      // Sort the bindings. The ones with higher priority are consulted
      // first; ties are broken by most-recently-registered.
      return mergeSort(this.bindings, function (a, b) {
        return b.priority - a.priority;
      });
    };
  }).call(BindingRegistry.prototype);

  var inputBindings = exports.inputBindings = new BindingRegistry();
  var outputBindings = exports.outputBindings = new BindingRegistry();

  //---------------------------------------------------------------------
  // Source file: ../srcjs/output_binding.js

  var OutputBinding = exports.OutputBinding = function () {};
  (function () {
    // Returns a jQuery object or element array that contains the
    // descendants of scope that match this binding
    this.find = function (scope) {
      throw "Not implemented";
    };

    this.getId = function (el) {
      return el['data-input-id'] || el.id;
    };

    this.onValueChange = function (el, data) {
      this.clearError(el);
      this.renderValue(el, data);
    };
    this.onValueError = function (el, err) {
      this.renderError(el, err);
    };
    this.renderError = function (el, err) {
      this.clearError(el);
      if (err.message === '') {
        // not really error, but we just need to wait (e.g. action buttons)
        $(el).empty();
        return;
      }
      var errClass = 'shiny-output-error';
      if (err.type !== null) {
        // use the classes of the error condition as CSS class names
        errClass = errClass + ' ' + $.map(asArray(err.type), function (type) {
          return errClass + '-' + type;
        }).join(' ');
      }
      $(el).addClass(errClass).text(err.message);
    };
    this.clearError = function (el) {
      $(el).attr('class', function (i, c) {
        return c.replace(/(^|\s)shiny-output-error\S*/g, '');
      });
    };
    this.showProgress = function (el, show) {
      var RECALC_CLASS = 'recalculating';
      if (show) $(el).addClass(RECALC_CLASS);else $(el).removeClass(RECALC_CLASS);
    };
  }).call(OutputBinding.prototype);

  //---------------------------------------------------------------------
  // Source file: ../srcjs/output_binding_text.js

  var textOutputBinding = new OutputBinding();
  $.extend(textOutputBinding, {
    find: function find(scope) {
      return $(scope).find('.shiny-text-output');
    },
    renderValue: function renderValue(el, data) {
      $(el).text(data);
    }
  });
  outputBindings.register(textOutputBinding, 'shiny.textOutput');

  //---------------------------------------------------------------------
  // Source file: ../srcjs/output_binding_image.js

  var imageOutputBinding = new OutputBinding();
  $.extend(imageOutputBinding, {
    find: function find(scope) {
      return $(scope).find('.shiny-image-output, .shiny-plot-output');
    },
    renderValue: function renderValue(el, data) {
      // The overall strategy:
      // * Clear out existing image and event handlers.
      // * Create new image.
      // * Create various event handlers.
      // * Bind those event handlers to events.
      // * Insert the new image.

      var outputId = this.getId(el);

      var $el = $(el);
      var img;

      // Remove event handlers that were added in previous renderValue()
      $el.off('.image_output');

      // Get existing img element if present.
      var $img = $el.find('img');

      if ($img.length === 0) {
        // If a img element is not already present, that means this is either
        // the first time renderValue() has been called, or this is after an
        // error.
        img = document.createElement('img');
        $el.append(img);
        $img = $(img);
      } else {
        // Trigger custom 'reset' event for any existing images in the div
        img = $img[0];
        $img.trigger('reset');
      }

      if (!data) {
        $el.empty();
        return;
      }

      // If value is undefined, return alternate. Sort of like ||, except it won't
      // return alternate for other falsy values (0, false, null).
      function OR(value, alternate) {
        if (value === undefined) return alternate;
        return value;
      }

      var opts = {
        clickId: $el.data('click-id'),
        clickClip: OR(strToBool($el.data('click-clip')), true),

        dblclickId: $el.data('dblclick-id'),
        dblclickClip: OR(strToBool($el.data('dblclick-clip')), true),
        dblclickDelay: OR($el.data('dblclick-delay'), 400),

        hoverId: $el.data('hover-id'),
        hoverClip: OR(strToBool($el.data('hover-clip')), true),
        hoverDelayType: OR($el.data('hover-delay-type'), 'debounce'),
        hoverDelay: OR($el.data('hover-delay'), 300),
        hoverNullOutside: OR(strToBool($el.data('hover-null-outside')), false),

        brushId: $el.data('brush-id'),
        brushClip: OR(strToBool($el.data('brush-clip')), true),
        brushDelayType: OR($el.data('brush-delay-type'), 'debounce'),
        brushDelay: OR($el.data('brush-delay'), 300),
        brushFill: OR($el.data('brush-fill'), '#666'),
        brushStroke: OR($el.data('brush-stroke'), '#000'),
        brushOpacity: OR($el.data('brush-opacity'), 0.3),
        brushDirection: OR($el.data('brush-direction'), 'xy'),
        brushResetOnNew: OR(strToBool($el.data('brush-reset-on-new')), false),

        coordmap: data.coordmap
      };

      // Copy items from data to img. Don't set the coordmap as an attribute.
      $.each(data, function (key, value) {
        if (value === null || key === 'coordmap') {
          return;
        }
        img.setAttribute(key, value);
      });

      // Unset any attributes in the current img that were not provided in the
      // new data.
      for (var i = 0; i < img.attributes.length; i++) {
        var attrib = img.attributes[i];
        // Need to check attrib.specified on IE because img.attributes contains
        // all possible attributes on IE.
        if (attrib.specified && !data.hasOwnProperty(attrib.name)) {
          img.removeAttribute(attrib.name);
        }
      }

      if (!opts.coordmap) opts.coordmap = [];

      imageutils.initCoordmap($el, opts.coordmap);

      // This object listens for mousedowns, and triggers mousedown2 and dblclick2
      // events as appropriate.
      var clickInfo = imageutils.createClickInfo($el, opts.dblclickId, opts.dblclickDelay);

      $el.on('mousedown.image_output', clickInfo.mousedown);

      if (browser.isIE && browser.IEVersion === 8) {
        $el.on('dblclick.image_output', clickInfo.dblclickIE8);
      }

      // ----------------------------------------------------------
      // Register the various event handlers
      // ----------------------------------------------------------
      if (opts.clickId) {
        var clickHandler = imageutils.createClickHandler(opts.clickId, opts.clickClip, opts.coordmap);
        $el.on('mousedown2.image_output', clickHandler.mousedown);

        // When img is reset, do housekeeping: clear $el's mouse listener and
        // call the handler's onResetImg callback.
        $img.on('reset', clickHandler.onResetImg);
      }

      if (opts.dblclickId) {
        // We'll use the clickHandler's mousedown function, but register it to
        // our custom 'dblclick2' event.
        var dblclickHandler = imageutils.createClickHandler(opts.dblclickId, opts.clickClip, opts.coordmap);
        $el.on('dblclick2.image_output', dblclickHandler.mousedown);

        $img.on('reset', dblclickHandler.onResetImg);
      }

      if (opts.hoverId) {
        var hoverHandler = imageutils.createHoverHandler(opts.hoverId, opts.hoverDelay, opts.hoverDelayType, opts.hoverClip, opts.hoverNullOutside, opts.coordmap);
        $el.on('mousemove.image_output', hoverHandler.mousemove);
        $el.on('mouseout.image_output', hoverHandler.mouseout);

        $img.on('reset', hoverHandler.onResetImg);
      }

      if (opts.brushId) {
        // Make image non-draggable (Chrome, Safari)
        $img.css('-webkit-user-drag', 'none');
        // Firefox, IE<=10
        $img.on('dragstart', function () {
          return false;
        });

        // Disable selection of image and text when dragging in IE<=10
        $el.on('selectstart.image_output', function () {
          return false;
        });

        var brushHandler = imageutils.createBrushHandler(opts.brushId, $el, opts, opts.coordmap, outputId);
        $el.on('mousedown.image_output', brushHandler.mousedown);
        $el.on('mousemove.image_output', brushHandler.mousemove);

        $img.on('reset', brushHandler.onResetImg);
      }

      if (opts.clickId || opts.dblclickId || opts.hoverId || opts.brushId) {
        $el.addClass('crosshair');
      }

      if (data.error) console.log('Error on server extracting coordmap: ' + data.error);
    },

    renderError: function renderError(el, err) {
      $(el).find('img').trigger('reset');
      OutputBinding.prototype.renderError.call(this, el, err);
    },

    clearError: function clearError(el) {
      // Remove all elements except img and the brush; this is usually just
      // error messages.
      $(el).contents().filter(function () {
        return this.tagName !== "IMG" && this.id !== el.id + '_brush';
      }).remove();

      OutputBinding.prototype.clearError.call(this, el);
    }
  });
  outputBindings.register(imageOutputBinding, 'shiny.imageOutput');

  var imageutils = {};

  // Modifies the panel objects in a coordmap, adding scale(), scaleInv(),
  // and clip() functions to each one.
  imageutils.initPanelScales = function (coordmap) {
    // Map a value x from a domain to a range. If clip is true, clip it to the
    // range.
    function mapLinear(x, domainMin, domainMax, rangeMin, rangeMax, clip) {
      // By default, clip to range
      clip = clip || true;

      var factor = (rangeMax - rangeMin) / (domainMax - domainMin);
      var val = x - domainMin;
      var newval = val * factor + rangeMin;

      if (clip) {
        var max = Math.max(rangeMax, rangeMin);
        var min = Math.min(rangeMax, rangeMin);
        if (newval > max) newval = max;else if (newval < min) newval = min;
      }
      return newval;
    }

    // Create scale and inverse-scale functions for a single direction (x or y).
    function scaler1D(domainMin, domainMax, rangeMin, rangeMax, logbase) {
      return {
        scale: function scale(val, clip) {
          if (logbase) val = Math.log(val) / Math.log(logbase);
          return mapLinear(val, domainMin, domainMax, rangeMin, rangeMax, clip);
        },

        scaleInv: function scaleInv(val, clip) {
          var res = mapLinear(val, rangeMin, rangeMax, domainMin, domainMax, clip);
          if (logbase) res = Math.pow(logbase, res);
          return res;
        }
      };
    }

    // Modify panel, adding scale and inverse-scale functions that take objects
    // like {x:1, y:3}, and also add clip function.
    function addScaleFuns(panel) {
      var d = panel.domain;
      var r = panel.range;
      var xlog = panel.log && panel.log.x ? panel.log.x : null;
      var ylog = panel.log && panel.log.y ? panel.log.y : null;
      var xscaler = scaler1D(d.left, d.right, r.left, r.right, xlog);
      var yscaler = scaler1D(d.bottom, d.top, r.bottom, r.top, ylog);

      panel.scale = function (val, clip) {
        return {
          x: xscaler.scale(val.x, clip),
          y: yscaler.scale(val.y, clip)
        };
      };

      panel.scaleInv = function (val, clip) {
        return {
          x: xscaler.scaleInv(val.x, clip),
          y: yscaler.scaleInv(val.y, clip)
        };
      };

      // Given a scaled offset (in pixels), clip it to the nearest panel region.
      panel.clip = function (offset) {
        var newOffset = {
          x: offset.x,
          y: offset.y
        };

        var bounds = panel.range;

        if (offset.x > bounds.right) newOffset.x = bounds.right;else if (offset.x < bounds.left) newOffset.x = bounds.left;

        if (offset.y > bounds.bottom) newOffset.y = bounds.bottom;else if (offset.y < bounds.top) newOffset.y = bounds.top;

        return newOffset;
      };
    }

    // Add the functions to each panel object.
    for (var i = 0; i < coordmap.length; i++) {
      var panel = coordmap[i];
      addScaleFuns(panel);
    }
  };

  // This adds functions to the coordmap object to handle various
  // coordinate-mapping tasks, and send information to the server.
  // The input coordmap is an array of objects, each of which represents a panel.
  // coordmap must be an array, even if empty, so that it can be modified in
  // place; when empty, we add a dummy panel to the array.
  // It also calls initPanelScales, which modifies each panel object to have
  // scale, scaleInv, and clip functions.
  imageutils.initCoordmap = function ($el, coordmap) {
    var el = $el[0];

    // If we didn't get any panels, create a dummy one where the domain and range
    // are simply the pixel dimensions.
    // that we modify.
    if (coordmap.length === 0) {
      var bounds = {
        top: 0,
        left: 0,
        right: el.clientWidth - 1,
        bottom: el.clientHeight - 1
      };

      coordmap[0] = {
        domain: bounds,
        range: bounds,
        mapping: {}
      };
    }

    // Add scaling functions to each panel
    imageutils.initPanelScales(coordmap);

    // Firefox doesn't have offsetX/Y, so we need to use an alternate
    // method of calculation for it. Even though other browsers do have
    // offsetX/Y, we need to calculate relative to $el, because sometimes the
    // mouse event can come with offset relative to other elements on the
    // page. This happens when the event listener is bound to, say, window.
    coordmap.mouseOffset = function (mouseEvent) {
      var offset = $el.offset();
      return {
        x: mouseEvent.pageX - offset.left,
        y: mouseEvent.pageY - offset.top
      };
    };

    // Given two sets of x/y coordinates, return an object representing the
    // min and max x and y values. (This could be generalized to any number
    // of points).
    coordmap.findBox = function (offset1, offset2) {
      return {
        xmin: Math.min(offset1.x, offset2.x),
        xmax: Math.max(offset1.x, offset2.x),
        ymin: Math.min(offset1.y, offset2.y),
        ymax: Math.max(offset1.y, offset2.y)
      };
    };

    // Shift an array of values so that they are within a min and max.
    // The vals will be shifted so that they maintain the same spacing
    // internally. If the range in vals is larger than the range of
    // min and max, the result might not make sense.
    coordmap.shiftToRange = function (vals, min, max) {
      if (!(vals instanceof Array)) vals = [vals];

      var maxval = Math.max.apply(null, vals);
      var minval = Math.min.apply(null, vals);
      var shiftAmount = 0;
      if (maxval > max) {
        shiftAmount = max - maxval;
      } else if (minval < min) {
        shiftAmount = min - minval;
      }

      var newvals = [];
      for (var i = 0; i < vals.length; i++) {
        newvals[i] = vals[i] + shiftAmount;
      }
      return newvals;
    };

    // Given an offset, return an object representing which panel it's in. The
    // `expand` argument tells it to expand the panel area by that many pixels.
    // It's possible for an offset to be within more than one panel, because
    // of the `expand` value. If that's the case, find the nearest panel.
    coordmap.getPanel = function (offset, expand) {
      expand = expand || 0;

      var x = offset.x;
      var y = offset.y;

      var matches = []; // Panels that match
      var dists = []; // Distance of offset to each matching panel
      var b;
      for (var i = 0; i < coordmap.length; i++) {
        b = coordmap[i].range;

        if (x <= b.right + expand && x >= b.left - expand && y <= b.bottom + expand && y >= b.top - expand) {
          matches.push(coordmap[i]);

          // Find distance from edges for x and y
          var xdist = 0;
          var ydist = 0;
          if (x > b.right && x <= b.right + expand) {
            xdist = x - b.right;
          } else if (x < b.left && x >= b.left - expand) {
            xdist = x - b.left;
          }
          if (y > b.bottom && y <= b.bottom + expand) {
            ydist = y - b.bottom;
          } else if (y < b.top && y >= b.top - expand) {
            ydist = y - b.top;
          }

          // Cartesian distance
          dists.push(Math.sqrt(Math.pow(xdist, 2) + Math.pow(ydist, 2)));
        }
      }

      if (matches.length) {
        // Find shortest distance
        var min_dist = Math.min.apply(null, dists);
        for (i = 0; i < matches.length; i++) {
          if (dists[i] === min_dist) {
            return matches[i];
          }
        }
      }

      return null;
    };

    // Is an offset in a panel? If supplied, `expand` tells us to expand the
    // panels by that many pixels in all directions.
    coordmap.isInPanel = function (offset, expand) {
      expand = expand || 0;

      if (coordmap.getPanel(offset, expand)) return true;

      return false;
    };

    // Returns a function that sends mouse coordinates, scaled to data space.
    // If that function is passed a null event, it will send null.
    coordmap.mouseCoordinateSender = function (inputId, clip, nullOutside) {
      if (clip === undefined) clip = true;
      if (nullOutside === undefined) nullOutside = false;

      return function (e) {
        if (e === null) {
          exports.onInputChange(inputId, null);
          return;
        }

        var offset = coordmap.mouseOffset(e);
        // If outside of plotting region
        if (!coordmap.isInPanel(offset)) {
          if (nullOutside) {
            exports.onInputChange(inputId, null);
            return;
          }
          if (clip) return;
        }
        if (clip && !coordmap.isInPanel(offset)) return;

        var panel = coordmap.getPanel(offset);
        var coords = panel.scaleInv(offset);

        // Add the panel (facet) variables, if present
        $.extend(coords, panel.panel_vars);

        // Add variable name mappings
        coords.mapping = panel.mapping;

        // Add scaling information
        coords.domain = panel.domain;
        coords.range = panel.range;
        coords.log = panel.log;

        coords[".nonce"] = Math.random();
        exports.onInputChange(inputId, coords);
      };
    };
  };

  // This object provides two public event listeners: mousedown, and
  // dblclickIE8.
  // We need to make sure that, when the image is listening for double-
  // clicks, that a double-click doesn't trigger two click events. We'll
  // trigger custom mousedown2 and dblclick2 events with this mousedown
  // listener.
  imageutils.createClickInfo = function ($el, dblclickId, dblclickDelay) {
    var clickTimer = null;
    var pending_e = null; // A pending mousedown2 event

    // Create a new event of type eventType (like 'mousedown2'), and trigger
    // it with the information stored in this.e.
    function triggerEvent(newEventType, e) {
      // Extract important info from e and construct a new event with type
      // eventType.
      var e2 = $.Event(newEventType, {
        which: e.which,
        pageX: e.pageX,
        pageY: e.pageY,
        offsetX: e.offsetX,
        offsetY: e.offsetY
      });

      $el.trigger(e2);
    }

    function triggerPendingMousedown2() {
      // It's possible that between the scheduling of a mousedown2 and the
      // time this callback is executed, someone else triggers a
      // mousedown2, so check for that.
      if (pending_e) {
        triggerEvent('mousedown2', pending_e);
        pending_e = null;
      }
    }

    // Set a timer to trigger a mousedown2 event, using information from the
    // last recorded mousdown event.
    function scheduleMousedown2(e) {
      pending_e = e;

      clickTimer = setTimeout(function () {
        triggerPendingMousedown2();
      }, dblclickDelay);
    }

    function mousedown(e) {
      // Listen for left mouse button only
      if (e.which !== 1) return;

      // If no dblclick listener, immediately trigger a mousedown2 event.
      if (!dblclickId) {
        triggerEvent('mousedown2', e);
        return;
      }

      // If there's a dblclick listener, make sure not to count this as a
      // click on the first mousedown; we need to wait for the dblclick
      // delay before we can be sure this click was a single-click.
      if (pending_e === null) {
        scheduleMousedown2(e);
      } else {
        clearTimeout(clickTimer);

        // If second click is too far away, it doesn't count as a double
        // click. Instead, immediately trigger a mousedown2 for the previous
        // click, and set this click as a new first click.
        if (pending_e && Math.abs(pending_e.offsetX - e.offsetX) > 2 || Math.abs(pending_e.offsetY - e.offsetY) > 2) {

          triggerPendingMousedown2();
          scheduleMousedown2(e);
        } else {
          // The second click was close to the first one. If it happened
          // within specified delay, trigger our custom 'dblclick2' event.
          pending_e = null;
          triggerEvent('dblclick2', e);
        }
      }
    }

    // IE8 needs a special hack because when you do a double-click it doesn't
    // trigger the click event twice - it directly triggers dblclick.
    function dblclickIE8(e) {
      e.which = 1; // In IE8, e.which is 0 instead of 1. ???
      triggerEvent('dblclick2', e);
    }

    return {
      mousedown: mousedown,
      dblclickIE8: dblclickIE8
    };
  };

  // ----------------------------------------------------------
  // Handler creators for click, hover, brush.
  // Each of these returns an object with a few public members. These public
  // members are callbacks that are meant to be bound to events on $el with
  // the same name (like 'mousedown').
  // ----------------------------------------------------------

  imageutils.createClickHandler = function (inputId, clip, coordmap) {
    var clickInfoSender = coordmap.mouseCoordinateSender(inputId, clip);

    return {
      mousedown: function mousedown(e) {
        // Listen for left mouse button only
        if (e.which !== 1) return;
        clickInfoSender(e);
      },
      onResetImg: function onResetImg() {
        clickInfoSender(null);
      }
    };
  };

  imageutils.createHoverHandler = function (inputId, delay, delayType, clip, nullOutside, coordmap) {
    var sendHoverInfo = coordmap.mouseCoordinateSender(inputId, clip, nullOutside);

    var hoverInfoSender;
    if (delayType === 'throttle') hoverInfoSender = new Throttler(null, sendHoverInfo, delay);else hoverInfoSender = new Debouncer(null, sendHoverInfo, delay);

    // What to do when mouse exits the image
    var mouseout;
    if (nullOutside) mouseout = function mouseout() {
      hoverInfoSender.normalCall(null);
    };else mouseout = function mouseout() {};

    return {
      mousemove: function mousemove(e) {
        hoverInfoSender.normalCall(e);
      },
      mouseout: mouseout,
      onResetImg: function onResetImg() {
        hoverInfoSender.immediateCall(null);
      }
    };
  };

  // Returns a brush handler object. This has three public functions:
  // mousedown, mousemove, and onResetImg.
  imageutils.createBrushHandler = function (inputId, $el, opts, coordmap, outputId) {
    // Parameter: expand the area in which a brush can be started, by this
    // many pixels in all directions. (This should probably be a brush option)
    var expandPixels = 20;

    // Represents the state of the brush
    var brush = imageutils.createBrush($el, opts, coordmap, expandPixels);

    // Brush IDs can span multiple image/plot outputs. When an output is brushed,
    // if a brush with the same ID is active on a different image/plot, it must
    // be dismissed (but without sending any data to the server). We implement
    // this by sending the shiny-internal:brushed event to all plots, and letting
    // each plot decide for itself what to do.
    //
    // The decision to have the event sent to each plot (as opposed to a single
    // event triggered on, say, the document) was made to make cleanup easier;
    // listening on an event on the document would prevent garbage collection
    // of plot outputs that are removed from the document.
    $el.on("shiny-internal:brushed.image_output", function (e, coords) {
      // If the new brush shares our ID but not our output element ID, we
      // need to clear our brush (if any).
      if (coords.brushId === inputId && coords.outputId !== outputId) {
        $el.data("mostRecentBrush", false);
        brush.reset();
      }
    });

    // Set cursor to one of 7 styles. We need to set the cursor on the whole
    // el instead of the brush div, because the brush div has
    // 'pointer-events:none' so that it won't intercept pointer events.
    // If `style` is null, don't add a cursor style.
    function setCursorStyle(style) {
      $el.removeClass('crosshair grabbable grabbing ns-resize ew-resize nesw-resize nwse-resize');

      if (style) $el.addClass(style);
    }

    function sendBrushInfo() {
      var coords = brush.boundsData();

      // We're in a new or reset state
      if (isNaN(coords.xmin)) {
        exports.onInputChange(inputId, null);
        // Must tell other brushes to clear.
        imageOutputBinding.find(document).trigger("shiny-internal:brushed", {
          brushId: inputId, outputId: null
        });
        return;
      }

      var panel = brush.getPanel();

      // Add the panel (facet) variables, if present
      $.extend(coords, panel.panel_vars);

      // Add variable name mappings
      coords.mapping = panel.mapping;

      // Add scaling information
      coords.domain = panel.domain;
      coords.range = panel.range;
      coords.log = panel.log;

      coords.direction = opts.brushDirection;

      coords.brushId = inputId;
      coords.outputId = outputId;

      // Send data to server
      exports.onInputChange(inputId, coords);

      $el.data("mostRecentBrush", true);
      imageOutputBinding.find(document).trigger("shiny-internal:brushed", coords);
    }

    var brushInfoSender;
    if (opts.brushDelayType === 'throttle') {
      brushInfoSender = new Throttler(null, sendBrushInfo, opts.brushDelay);
    } else {
      brushInfoSender = new Debouncer(null, sendBrushInfo, opts.brushDelay);
    }

    function mousedown(e) {
      // This can happen when mousedown inside the graphic, then mouseup
      // outside, then mousedown inside. Just ignore the second
      // mousedown.
      if (brush.isBrushing() || brush.isDragging() || brush.isResizing()) return;

      // Listen for left mouse button only
      if (e.which !== 1) return;

      var offset = coordmap.mouseOffset(e);

      // Ignore mousedown events outside of plotting region, expanded by
      // a number of pixels specified in expandPixels.
      if (opts.brushClip && !coordmap.isInPanel(offset, expandPixels)) return;

      brush.up({ x: NaN, y: NaN });
      brush.down(offset);

      if (brush.isInResizeArea(offset)) {
        brush.startResizing(offset);

        // Attach the move and up handlers to the window so that they respond
        // even when the mouse is moved outside of the image.
        $(document).on('mousemove.image_brush', mousemoveResizing).on('mouseup.image_brush', mouseupResizing);
      } else if (brush.isInsideBrush(offset)) {
        brush.startDragging(offset);
        setCursorStyle('grabbing');

        // Attach the move and up handlers to the window so that they respond
        // even when the mouse is moved outside of the image.
        $(document).on('mousemove.image_brush', mousemoveDragging).on('mouseup.image_brush', mouseupDragging);
      } else {
        var panel = coordmap.getPanel(offset, expandPixels);
        brush.startBrushing(panel.clip(offset));

        // Attach the move and up handlers to the window so that they respond
        // even when the mouse is moved outside of the image.
        $(document).on('mousemove.image_brush', mousemoveBrushing).on('mouseup.image_brush', mouseupBrushing);
      }
    }

    // This sets the cursor style when it's in the el
    function mousemove(e) {
      var offset = coordmap.mouseOffset(e);

      if (!(brush.isBrushing() || brush.isDragging() || brush.isResizing())) {
        // Set the cursor depending on where it is
        if (brush.isInResizeArea(offset)) {
          var r = brush.whichResizeSides(offset);

          if (r.left && r.top || r.right && r.bottom) {
            setCursorStyle('nwse-resize');
          } else if (r.left && r.bottom || r.right && r.top) {
            setCursorStyle('nesw-resize');
          } else if (r.left || r.right) {
            setCursorStyle('ew-resize');
          } else if (r.top || r.bottom) {
            setCursorStyle('ns-resize');
          }
        } else if (brush.isInsideBrush(offset)) {
          setCursorStyle('grabbable');
        } else if (coordmap.isInPanel(offset, expandPixels)) {
          setCursorStyle('crosshair');
        } else {
          setCursorStyle(null);
        }
      }
    }

    // mousemove handlers while brushing or dragging
    function mousemoveBrushing(e) {
      brush.brushTo(coordmap.mouseOffset(e));
      brushInfoSender.normalCall();
    }

    function mousemoveDragging(e) {
      brush.dragTo(coordmap.mouseOffset(e));
      brushInfoSender.normalCall();
    }

    function mousemoveResizing(e) {
      brush.resizeTo(coordmap.mouseOffset(e));
      brushInfoSender.normalCall();
    }

    // mouseup handlers while brushing or dragging
    function mouseupBrushing(e) {
      // Listen for left mouse button only
      if (e.which !== 1) return;

      $(document).off('mousemove.image_brush').off('mouseup.image_brush');

      brush.up(coordmap.mouseOffset(e));

      brush.stopBrushing();
      setCursorStyle('crosshair');

      // If the brush didn't go anywhere, hide the brush, clear value,
      // and return.
      if (brush.down().x === brush.up().x && brush.down().y === brush.up().y) {
        brush.reset();
        brushInfoSender.immediateCall();
        return;
      }

      // Send info immediately on mouseup, but only if needed. If we don't
      // do the pending check, we might send the same data twice (with
      // with difference nonce).
      if (brushInfoSender.isPending()) brushInfoSender.immediateCall();
    }

    function mouseupDragging(e) {
      // Listen for left mouse button only
      if (e.which !== 1) return;

      $(document).off('mousemove.image_brush').off('mouseup.image_brush');

      brush.up(coordmap.mouseOffset(e));

      brush.stopDragging();
      setCursorStyle('grabbable');

      if (brushInfoSender.isPending()) brushInfoSender.immediateCall();
    }

    function mouseupResizing(e) {
      // Listen for left mouse button only
      if (e.which !== 1) return;

      $(document).off('mousemove.image_brush').off('mouseup.image_brush');

      brush.up(coordmap.mouseOffset(e));
      brush.stopResizing();

      if (brushInfoSender.isPending()) brushInfoSender.immediateCall();
    }

    // Brush maintenance: When an image is re-rendered, the brush must either
    // be removed (if brushResetOnNew) or imported (if !brushResetOnNew). The
    // "mostRecentBrush" bit is to ensure that when multiple outputs share the
    // same brush ID, inactive brushes don't send null values up to the server.

    // This should be called when the img (not the el) is reset
    function onResetImg() {
      if (opts.brushResetOnNew) {
        if ($el.data("mostRecentBrush")) {
          brush.reset();
          brushInfoSender.immediateCall();
        }
      }
    }

    if (!opts.brushResetOnNew) {
      if ($el.data("mostRecentBrush")) {
        brush.importOldBrush();
        brushInfoSender.immediateCall();
      }
    }

    return {
      mousedown: mousedown,
      mousemove: mousemove,
      onResetImg: onResetImg
    };
  };

  // Returns an object that represents the state of the brush. This gets wrapped
  // in a brushHandler, which provides various event listeners.
  imageutils.createBrush = function ($el, opts, coordmap, expandPixels) {
    // Number of pixels outside of brush to allow start resizing
    var resizeExpand = 10;

    var el = $el[0];
    var $div = null; // The div representing the brush

    var state = {};
    reset();

    function reset() {
      // Current brushing/dragging/resizing state
      state.brushing = false;
      state.dragging = false;
      state.resizing = false;

      // Offset of last mouse down and up events
      state.down = { x: NaN, y: NaN };
      state.up = { x: NaN, y: NaN };

      // Which side(s) we're currently resizing
      state.resizeSides = {
        left: false,
        right: false,
        top: false,
        bottom: false
      };

      // Bounding rectangle of the brush, in pixel and data dimensions. We need to
      // record data dimensions along with pixel dimensions so that when a new
      // plot is sent, we can re-draw the brush div with the appropriate coords.
      state.boundsPx = {
        xmin: NaN,
        xmax: NaN,
        ymin: NaN,
        ymax: NaN
      };
      state.boundsData = {
        xmin: NaN,
        xmax: NaN,
        ymin: NaN,
        ymax: NaN
      };

      // Panel object that the brush is in
      state.panel = null;

      // The bounds at the start of a drag/resize
      state.changeStartBounds = {
        xmin: NaN,
        xmax: NaN,
        ymin: NaN,
        ymax: NaN
      };

      if ($div) $div.remove();
    }

    // If there's an existing brush div, use that div to set the new brush's
    // settings, provided that the x, y, and panel variables have the same names,
    // and there's a panel with matching panel variable values.
    function importOldBrush() {
      var oldDiv = $el.find('#' + el.id + '_brush');
      if (oldDiv.length === 0) return;

      var oldBoundsData = oldDiv.data('bounds-data');
      var oldPanel = oldDiv.data('panel');

      if (!oldBoundsData || !oldPanel) return;

      // Compare two objects. This checks that objects a and b have the same est
      // of keys, and that each key has the same value. This function isn't
      // perfect, but it's good enough for comparing variable mappings, below.
      function isEquivalent(a, b) {
        if (a === undefined) {
          if (b === undefined) return true;else return false;
        }
        if (a === null) {
          if (b === null) return true;else return false;
        }

        var aProps = Object.getOwnPropertyNames(a);
        var bProps = Object.getOwnPropertyNames(b);

        if (aProps.length != bProps.length) return false;

        for (var i = 0; i < aProps.length; i++) {
          var propName = aProps[i];
          if (a[propName] !== b[propName]) {
            return false;
          }
        }
        return true;
      }

      // Find a panel that has matching vars; if none found, we can't restore.
      // The oldPanel and new panel must match on their mapping vars, and the
      // values.
      for (var i = 0; i < coordmap.length; i++) {
        var curPanel = coordmap[i];

        if (isEquivalent(oldPanel.mapping, curPanel.mapping) && isEquivalent(oldPanel.panel_vars, curPanel.panel_vars)) {
          // We've found a matching panel
          state.panel = coordmap[i];
          break;
        }
      }

      // If we didn't find a matching panel, remove the old div and return
      if (state.panel === null) {
        oldDiv.remove();
        return;
      }

      $div = oldDiv;

      boundsData(oldBoundsData);
      updateDiv();
    }

    // Return true if the offset is inside min/max coords
    function isInsideBrush(offset) {
      var bounds = state.boundsPx;
      return offset.x <= bounds.xmax && offset.x >= bounds.xmin && offset.y <= bounds.ymax && offset.y >= bounds.ymin;
    }

    // Return true if offset is inside a region to start a resize
    function isInResizeArea(offset) {
      var sides = whichResizeSides(offset);
      return sides.left || sides.right || sides.top || sides.bottom;
    }

    // Return an object representing which resize region(s) the cursor is in.
    function whichResizeSides(offset) {
      var b = state.boundsPx;
      // Bounds with expansion
      var e = {
        xmin: b.xmin - resizeExpand,
        xmax: b.xmax + resizeExpand,
        ymin: b.ymin - resizeExpand,
        ymax: b.ymax + resizeExpand
      };
      var res = {
        left: false,
        right: false,
        top: false,
        bottom: false
      };

      if ((opts.brushDirection === 'xy' || opts.brushDirection === 'x') && offset.y <= e.ymax && offset.y >= e.ymin) {
        if (offset.x < b.xmin && offset.x >= e.xmin) res.left = true;else if (offset.x > b.xmax && offset.x <= e.xmax) res.right = true;
      }

      if ((opts.brushDirection === 'xy' || opts.brushDirection === 'y') && offset.x <= e.xmax && offset.x >= e.xmin) {
        if (offset.y < b.ymin && offset.y >= e.ymin) res.top = true;else if (offset.y > b.ymax && offset.y <= e.ymax) res.bottom = true;
      }

      return res;
    }

    // Sets the bounds of the brush, given a box and optional panel. This
    // will fit the box bounds into the panel, so we don't brush outside of it.
    // This knows whether we're brushing in the x, y, or xy directions, and sets
    // bounds accordingly.
    // If no box is passed in, just return current bounds.
    function boundsPx(box) {
      if (box === undefined) return state.boundsPx;

      var min = { x: box.xmin, y: box.ymin };
      var max = { x: box.xmax, y: box.ymax };

      var panel = state.panel;
      var panelBounds = panel.range;

      if (opts.brushClip) {
        min = panel.clip(min);
        max = panel.clip(max);
      }

      if (opts.brushDirection === 'xy') {
        // No change

      } else if (opts.brushDirection === 'x') {
        // Extend top and bottom of plotting area
        min.y = panelBounds.top;
        max.y = panelBounds.bottom;
      } else if (opts.brushDirection === 'y') {
        min.x = panelBounds.left;
        max.x = panelBounds.right;
      }

      state.boundsPx = {
        xmin: min.x,
        xmax: max.x,
        ymin: min.y,
        ymax: max.y
      };

      // Positions in data space
      var minData = state.panel.scaleInv(min);
      var maxData = state.panel.scaleInv(max);
      // For reversed scales, the min and max can be reversed, so use findBox
      // to ensure correct order.
      state.boundsData = coordmap.findBox(minData, maxData);

      // We also need to attach the data bounds and panel as data attributes, so
      // that if the image is re-sent, we can grab the data bounds to create a new
      // brush. This should be fast because it doesn't actually modify the DOM.
      $div.data('bounds-data', state.boundsData);
      $div.data('panel', state.panel);
      return undefined;
    }

    // Get or set the bounds of the brush using coordinates in the data space.
    function boundsData(box) {
      if (box === undefined) {
        return state.boundsData;
      }

      var min = { x: box.xmin, y: box.ymin };
      var max = { x: box.xmax, y: box.ymax };

      var minPx = state.panel.scale(min);
      var maxPx = state.panel.scale(max);

      // The scaling function can reverse the direction of the axes, so we need to
      // find the min and max again.
      boundsPx({
        xmin: Math.min(minPx.x, maxPx.x),
        xmax: Math.max(minPx.x, maxPx.x),
        ymin: Math.min(minPx.y, maxPx.y),
        ymax: Math.max(minPx.y, maxPx.y)
      });
      return undefined;
    }

    function getPanel() {
      return state.panel;
    }

    // Add a new div representing the brush.
    function addDiv() {
      if ($div) $div.remove();

      // Start hidden; we'll show it when movement occurs
      $div = $(document.createElement('div')).attr('id', el.id + '_brush').css({
        'background-color': opts.brushFill,
        'opacity': opts.brushOpacity,
        'pointer-events': 'none',
        'position': 'absolute'
      }).hide();

      var borderStyle = '1px solid ' + opts.brushStroke;
      if (opts.brushDirection === 'xy') {
        $div.css({
          'border': borderStyle
        });
      } else if (opts.brushDirection === 'x') {
        $div.css({
          'border-left': borderStyle,
          'border-right': borderStyle
        });
      } else if (opts.brushDirection === 'y') {
        $div.css({
          'border-top': borderStyle,
          'border-bottom': borderStyle
        });
      }

      $el.append($div);
      $div.offset({ x: 0, y: 0 }).width(0).outerHeight(0);
    }

    // Update the brush div to reflect the current brush bounds.
    function updateDiv() {
      // Need parent offset relative to page to calculate mouse offset
      // relative to page.
      var imgOffset = $el.offset();
      var b = state.boundsPx;
      $div.offset({
        top: imgOffset.top + b.ymin,
        left: imgOffset.left + b.xmin
      }).outerWidth(b.xmax - b.xmin + 1).outerHeight(b.ymax - b.ymin + 1);
    }

    function down(offset) {
      if (offset === undefined) return state.down;

      state.down = offset;
      return undefined;
    }

    function up(offset) {
      if (offset === undefined) return state.up;

      state.up = offset;
      return undefined;
    }

    function isBrushing() {
      return state.brushing;
    }

    function startBrushing() {
      state.brushing = true;
      addDiv();
      state.panel = coordmap.getPanel(state.down, expandPixels);

      boundsPx(coordmap.findBox(state.down, state.down));
      updateDiv();
    }

    function brushTo(offset) {
      boundsPx(coordmap.findBox(state.down, offset));
      $div.show();
      updateDiv();
    }

    function stopBrushing() {
      state.brushing = false;

      // Save the final bounding box of the brush
      boundsPx(coordmap.findBox(state.down, state.up));
    }

    function isDragging() {
      return state.dragging;
    }

    function startDragging() {
      state.dragging = true;
      state.changeStartBounds = $.extend({}, state.boundsPx);
    }

    function dragTo(offset) {
      // How far the brush was dragged
      var dx = offset.x - state.down.x;
      var dy = offset.y - state.down.y;

      // Calculate what new positions would be, before clipping.
      var start = state.changeStartBounds;
      var newBounds = {
        xmin: start.xmin + dx,
        xmax: start.xmax + dx,
        ymin: start.ymin + dy,
        ymax: start.ymax + dy
      };

      // Clip to the plotting area
      if (opts.brushClip) {
        var panelBounds = state.panel.range;

        // Convert to format for shiftToRange
        var xvals = [newBounds.xmin, newBounds.xmax];
        var yvals = [newBounds.ymin, newBounds.ymax];

        xvals = coordmap.shiftToRange(xvals, panelBounds.left, panelBounds.right);
        yvals = coordmap.shiftToRange(yvals, panelBounds.top, panelBounds.bottom);

        // Convert back to bounds format
        newBounds = {
          xmin: xvals[0],
          xmax: xvals[1],
          ymin: yvals[0],
          ymax: yvals[1]
        };
      }

      boundsPx(newBounds);
      updateDiv();
    }

    function stopDragging() {
      state.dragging = false;
    }

    function isResizing() {
      return state.resizing;
    }

    function startResizing() {
      state.resizing = true;
      state.changeStartBounds = $.extend({}, state.boundsPx);
      state.resizeSides = whichResizeSides(state.down);
    }

    function resizeTo(offset) {
      // How far the brush was dragged
      var dx = offset.x - state.down.x;
      var dy = offset.y - state.down.y;

      // Calculate what new positions would be, before clipping.
      var b = $.extend({}, state.changeStartBounds);
      var panelBounds = state.panel.range;

      if (state.resizeSides.left) {
        b.xmin = coordmap.shiftToRange([b.xmin + dx], panelBounds.left, b.xmax)[0];
      } else if (state.resizeSides.right) {
        b.xmax = coordmap.shiftToRange([b.xmax + dx], b.xmin, panelBounds.right)[0];
      }

      if (state.resizeSides.top) {
        b.ymin = coordmap.shiftToRange([b.ymin + dy], panelBounds.top, b.ymax)[0];
      } else if (state.resizeSides.bottom) {
        b.ymax = coordmap.shiftToRange([b.ymax + dy], b.ymin, panelBounds.bottom)[0];
      }

      boundsPx(b);
      updateDiv();
    }

    function stopResizing() {
      state.resizing = false;
    }

    return {
      reset: reset,

      importOldBrush: importOldBrush,
      isInsideBrush: isInsideBrush,
      isInResizeArea: isInResizeArea,
      whichResizeSides: whichResizeSides,

      boundsPx: boundsPx,
      boundsData: boundsData,
      getPanel: getPanel,

      down: down,
      up: up,

      isBrushing: isBrushing,
      startBrushing: startBrushing,
      brushTo: brushTo,
      stopBrushing: stopBrushing,

      isDragging: isDragging,
      startDragging: startDragging,
      dragTo: dragTo,
      stopDragging: stopDragging,

      isResizing: isResizing,
      startResizing: startResizing,
      resizeTo: resizeTo,
      stopResizing: stopResizing
    };
  };

  exports.resetBrush = function (brushId) {
    exports.onInputChange(brushId, null);
    imageOutputBinding.find(document).trigger("shiny-internal:brushed", {
      brushId: brushId, outputId: null
    });
  };

  //---------------------------------------------------------------------
  // Source file: ../srcjs/output_binding_html.js

  var htmlOutputBinding = new OutputBinding();
  $.extend(htmlOutputBinding, {
    find: function find(scope) {
      return $(scope).find('.shiny-html-output');
    },
    onValueError: function onValueError(el, err) {
      exports.unbindAll(el);
      this.renderError(el, err);
    },
    renderValue: function renderValue(el, data) {
      exports.renderContent(el, data);
    }
  });
  outputBindings.register(htmlOutputBinding, 'shiny.htmlOutput');

  var renderDependencies = exports.renderDependencies = function (dependencies) {
    if (dependencies) {
      $.each(dependencies, function (i, dep) {
        renderDependency(dep);
      });
    }
  };

  // Render HTML in a DOM element, add dependencies, and bind Shiny
  // inputs/outputs. `content` can be null, a string, or an object with
  // properties 'html' and 'deps'.
  exports.renderContent = function (el, content) {
    var where = arguments.length <= 2 || arguments[2] === undefined ? "replace" : arguments[2];

    exports.unbindAll(el);

    var html;
    var dependencies = [];
    if (content === null) {
      html = '';
    } else if (typeof content === 'string') {
      html = content;
    } else if ((typeof content === 'undefined' ? 'undefined' : _typeof(content)) === 'object') {
      html = content.html;
      dependencies = content.deps || [];
    }

    exports.renderHtml(html, el, dependencies, where);

    var scope = el;
    if (where === "replace") {
      exports.initializeInputs(el);
      exports.bindAll(el);
    } else {
      var $parent = $(el).parent();
      if ($parent.length > 0) {
        scope = $parent;
        if (where === "beforeBegin" || where === "afterEnd") {
          var $grandparent = $parent.parent();
          if ($grandparent.length > 0) scope = $grandparent;
        }
      }
      exports.initializeInputs(scope);
      exports.bindAll(scope);
    }
  };

  // Render HTML in a DOM element, inserting singletons into head as needed
  exports.renderHtml = function (html, el, dependencies) {
    var where = arguments.length <= 3 || arguments[3] === undefined ? 'replace' : arguments[3];

    renderDependencies(dependencies);
    return singletons.renderHtml(html, el, where);
  };

  var htmlDependencies = {};
  function registerDependency(name, version) {
    htmlDependencies[name] = version;
  }

  // Client-side dependency resolution and rendering
  function renderDependency(dep) {
    if (htmlDependencies.hasOwnProperty(dep.name)) return false;

    registerDependency(dep.name, dep.version);

    var href = dep.src.href;

    var $head = $("head").first();

    if (dep.meta) {
      var metas = $.map(asArray(dep.meta), function (content, name) {
        return $("<meta>").attr("name", name).attr("content", content);
      });
      $head.append(metas);
    }

    if (dep.stylesheet) {
      var stylesheets = $.map(asArray(dep.stylesheet), function (stylesheet) {
        return $("<link rel='stylesheet' type='text/css'>").attr("href", href + "/" + encodeURI(stylesheet));
      });
      $head.append(stylesheets);
    }

    if (dep.script) {
      var scripts = $.map(asArray(dep.script), function (scriptName) {
        return $("<script>").attr("src", href + "/" + encodeURI(scriptName));
      });
      $head.append(scripts);
    }

    if (dep.attachment) {
      // dep.attachment might be a single string, an array, or an object.
      var attachments = dep.attachment;
      if (typeof attachments === "string") attachments = [attachments];
      if ($.isArray(attachments)) {
        // The contract for attachments is that arrays of attachments are
        // addressed using 1-based indexes. Convert this array to an object.
        var tmp = {};
        $.each(attachments, function (index, attachment) {
          tmp[index + 1 + ""] = attachment;
        });
        attachments = tmp;
      }

      var attach = $.map(attachments, function (attachment, key) {
        return $("<link rel='attachment'>").attr("id", dep.name + "-" + key + "-attachment").attr("href", href + "/" + encodeURI(attachment));
      });
      $head.append(attach);
    }

    if (dep.head) {
      var $newHead = $("<head></head>");
      $newHead.html(dep.head);
      $head.append($newHead.children());
    }
    return true;
  }

  var singletons = {
    knownSingletons: {},
    renderHtml: function renderHtml(html, el, where) {
      var processed = this._processHtml(html);
      this._addToHead(processed.head);
      this.register(processed.singletons);
      if (where === "replace") {
        $(el).html(processed.html);
      } else {
        el.insertAdjacentHTML(where, processed.html);
      }
      return processed;
    },
    // Take an object where keys are names of singletons, and merges it into
    // knownSingletons
    register: function register(s) {
      $.extend(this.knownSingletons, s);
    },
    // Takes a string or array of strings and adds them to knownSingletons
    registerNames: function registerNames(s) {
      if (typeof s === 'string') {
        this.knownSingletons[s] = true;
      } else if (s instanceof Array) {
        for (var i = 0; i < s.length; i++) {
          this.knownSingletons[s[i]] = true;
        }
      }
    },
    // Inserts new content into document head
    _addToHead: function _addToHead(head) {
      if (head.length > 0) {
        var tempDiv = $("<div>" + head + "</div>")[0];
        var $head = $('head');
        while (tempDiv.hasChildNodes()) {
          $head.append(tempDiv.firstChild);
        }
      }
    },
    // Reads HTML and returns an object with info about singletons
    _processHtml: function _processHtml(val) {
      var self = this;
      var newSingletons = {};
      var newVal;

      var findNewPayload = function findNewPayload(match, p1, sig, payload) {
        if (self.knownSingletons[sig] || newSingletons[sig]) return "";
        newSingletons[sig] = true;
        return payload;
      };
      while (true) {
        newVal = val.replace(self._reSingleton, findNewPayload);
        if (val.length === newVal.length) break;
        val = newVal;
      }

      var heads = [];
      var headAddPayload = function headAddPayload(match, payload) {
        heads.push(payload);
        return "";
      };
      while (true) {
        newVal = val.replace(self._reHead, headAddPayload);
        if (val.length === newVal.length) break;
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

  //---------------------------------------------------------------------
  // Source file: ../srcjs/output_binding_downloadlink.js

  var downloadLinkOutputBinding = new OutputBinding();
  $.extend(downloadLinkOutputBinding, {
    find: function find(scope) {
      return $(scope).find('a.shiny-download-link');
    },
    renderValue: function renderValue(el, data) {
      $(el).attr('href', data);
    }
  });
  outputBindings.register(downloadLinkOutputBinding, 'shiny.downloadLink');

  //---------------------------------------------------------------------
  // Source file: ../srcjs/output_binding_datatable.js

  var datatableOutputBinding = new OutputBinding();
  $.extend(datatableOutputBinding, {
    find: function find(scope) {
      return $(scope).find('.shiny-datatable-output');
    },
    onValueError: function onValueError(el, err) {
      exports.unbindAll(el);
      this.renderError(el, err);
    },
    renderValue: function renderValue(el, _data) {
      var $el = $(el).empty();
      if (!_data || !_data.colnames) return;

      var colnames = $.makeArray(_data.colnames);
      var header = $.map(colnames, function (x) {
        return '<th>' + x + '</th>';
      }).join('');
      header = '<thead><tr>' + header + '</tr></thead>';
      var footer = '';
      if (_data.options === null || _data.options.searching !== false) {
        footer = $.map(colnames, function (x) {
          // placeholder needs to be escaped (and HTML tags are stripped off)
          return '<th><input type="text" placeholder="' + escapeHTML(x.replace(/(<([^>]+)>)/ig, '')) + '" /></th>';
        }).join('');
        footer = '<tfoot>' + footer + '</tfoot>';
      }
      var content = '<table class="table table-striped table-hover">' + header + footer + '</table>';
      $el.append(content);

      // options that should be eval()ed
      if (_data.evalOptions) $.each(_data.evalOptions, function (i, x) {
        /*jshint evil: true */
        _data.options[x] = eval('(' + _data.options[x] + ')');
      });

      // caseInsensitive searching? default true
      var searchCI = _data.options === null || typeof _data.options.search === 'undefined' || _data.options.search.caseInsensitive !== false;
      var oTable = $(el).children("table").DataTable($.extend({
        "processing": true,
        "serverSide": true,
        "order": [],
        "orderClasses": false,
        "pageLength": 25,
        "ajax": {
          "url": _data.action,
          "type": "POST",
          "data": function data(d) {
            d.search.caseInsensitive = searchCI;
            d.escape = _data.escape;
          }
        }
      }, _data.options));
      // the table object may need post-processing
      if (typeof _data.callback === 'string') {
        /*jshint evil: true */
        var callback = eval('(' + _data.callback + ')');
        if (typeof callback === 'function') callback(oTable);
      }

      // use debouncing for searching boxes
      $el.find('label input').first().unbind('keyup').keyup(debounce(_data.searchDelay, function () {
        oTable.search(this.value).draw();
      }));
      var searchInputs = $el.find("tfoot input");
      if (searchInputs.length > 0) {
        // this is a little weird: aoColumns/bSearchable are still in DT 1.10
        // https://github.com/DataTables/DataTables/issues/388
        $.each(oTable.settings()[0].aoColumns, function (i, x) {
          // hide the text box if not searchable
          if (!x.bSearchable) searchInputs.eq(i).hide();
        });
        searchInputs.keyup(debounce(_data.searchDelay, function () {
          oTable.column(searchInputs.index(this)).search(this.value).draw();
        }));
      }
      // FIXME: ugly scrollbars in tab panels b/c Bootstrap uses 'visible: auto'
      $el.parents('.tab-content').css('overflow', 'visible');
    }
  });
  outputBindings.register(datatableOutputBinding, 'shiny.datatableOutput');

  //---------------------------------------------------------------------
  // Source file: ../srcjs/output_binding_adapter.js

  var OutputBindingAdapter = function OutputBindingAdapter(el, binding) {
    this.el = el;
    this.binding = binding;

    // If the binding actually has a resize method, override the prototype of
    // onResize with a version that does a makeResizeFilter on the element.
    if (binding.resize) {
      this.onResize = makeResizeFilter(el, function (width, height) {
        binding.resize(el, width, height);
      });
    }
  };
  (function () {
    this.getId = function () {
      return this.binding.getId(this.el);
    };
    this.onValueChange = function (data) {
      this.binding.onValueChange(this.el, data);
    };
    this.onValueError = function (err) {
      this.binding.onValueError(this.el, err);
    };
    this.showProgress = function (show) {
      this.binding.showProgress(this.el, show);
    };
    this.onResize = function () {
      // Intentionally left blank; see constructor
    };
  }).call(OutputBindingAdapter.prototype);

  //---------------------------------------------------------------------
  // Source file: ../srcjs/input_binding.js

  var InputBinding = exports.InputBinding = function () {};

  (function () {

    // Returns a jQuery object or element array that contains the
    // descendants of scope that match this binding
    this.find = function (scope) {
      throw "Not implemented";
    };

    this.getId = function (el) {
      return el['data-input-id'] || el.id;
    };

    // Gives the input a type in case the server needs to know it
    // to deserialize the JSON correctly
    this.getType = function () {
      return false;
    };
    this.getValue = function (el) {
      throw "Not implemented";
    };
    this.subscribe = function (el, callback) {};
    this.unsubscribe = function (el) {};

    // This is used for receiving messages that tell the input object to do
    // things, such as setting values (including min, max, and others).
    // 'data' should be an object with elements corresponding to value, min,
    // max, etc., as appropriate for the type of input object. It also should
    // trigger a change event.
    this.receiveMessage = function (el, data) {
      throw "Not implemented";
    };
    this.getState = function (el, data) {
      throw "Not implemented";
    };

    this.getRatePolicy = function () {
      return null;
    };

    // Some input objects need initialization before being bound. This is
    // called when the document is ready (for statically-added input objects),
    // and when new input objects are added to the document with
    // htmlOutputBinding.renderValue() (for dynamically-added input objects).
    // This is called before the input is bound.
    this.initialize = function (el) {};

    // This is called after unbinding the output.
    this.dispose = function (el) {};
  }).call(InputBinding.prototype);

  //---------------------------------------------------------------------
  // Source file: ../srcjs/input_binding_text.js

  var textInputBinding = new InputBinding();
  $.extend(textInputBinding, {
    find: function find(scope) {
      return $(scope).find('input[type="text"], input[type="search"], input[type="url"], input[type="email"]');
    },
    getId: function getId(el) {
      return InputBinding.prototype.getId.call(this, el) || el.name;
    },
    getValue: function getValue(el) {
      return el.value;
    },
    setValue: function setValue(el, value) {
      el.value = value;
    },
    subscribe: function subscribe(el, callback) {
      $(el).on('keyup.textInputBinding input.textInputBinding', function (event) {
        callback(true);
      });
      $(el).on('change.textInputBinding', function (event) {
        callback(false);
      });
    },
    unsubscribe: function unsubscribe(el) {
      $(el).off('.textInputBinding');
    },
    receiveMessage: function receiveMessage(el, data) {
      if (data.hasOwnProperty('value')) this.setValue(el, data.value);

      if (data.hasOwnProperty('label')) $(el).parent().find('label[for="' + $escape(el.id) + '"]').text(data.label);

      $(el).trigger('change');
    },
    getState: function getState(el) {
      return {
        label: $(el).parent().find('label[for="' + $escape(el.id) + '"]').text(),
        value: el.value
      };
    },
    getRatePolicy: function getRatePolicy() {
      return {
        policy: 'debounce',
        delay: 250
      };
    }
  });
  inputBindings.register(textInputBinding, 'shiny.textInput');

  //---------------------------------------------------------------------
  // Source file: ../srcjs/input_binding_textarea.js

  var textareaInputBinding = {};
  $.extend(textareaInputBinding, textInputBinding, {
    find: function find(scope) {
      return $(scope).find('textarea');
    }
  });
  inputBindings.register(textareaInputBinding, 'shiny.textareaInput');

  //---------------------------------------------------------------------
  // Source file: ../srcjs/input_binding_password.js

  var passwordInputBinding = {};
  $.extend(passwordInputBinding, textInputBinding, {
    find: function find(scope) {
      return $(scope).find('input[type="password"]');
    },
    getType: function getType(el) {
      return "shiny.password";
    }
  });
  inputBindings.register(passwordInputBinding, 'shiny.passwordInput');

  //---------------------------------------------------------------------
  // Source file: ../srcjs/input_binding_number.js

  var numberInputBinding = {};
  $.extend(numberInputBinding, textInputBinding, {
    find: function find(scope) {
      return $(scope).find('input[type="number"]');
    },
    getValue: function getValue(el) {
      var numberVal = $(el).val();
      if (/^\s*$/.test(numberVal)) // Return null if all whitespace
        return null;else if (!isNaN(numberVal)) // If valid Javascript number string, coerce to number
        return +numberVal;else return numberVal; // If other string like "1e6", send it unchanged
    },
    setValue: function setValue(el, value) {
      el.value = value;
    },
    getType: function getType(el) {
      return "shiny.number";
    },
    receiveMessage: function receiveMessage(el, data) {
      if (data.hasOwnProperty('value')) el.value = data.value;
      if (data.hasOwnProperty('min')) el.min = data.min;
      if (data.hasOwnProperty('max')) el.max = data.max;
      if (data.hasOwnProperty('step')) el.step = data.step;

      if (data.hasOwnProperty('label')) $(el).parent().find('label[for="' + $escape(el.id) + '"]').text(data.label);

      $(el).trigger('change');
    },
    getState: function getState(el) {
      return { label: $(el).parent().find('label[for="' + $escape(el.id) + '"]').text(),
        value: this.getValue(el),
        min: Number(el.min),
        max: Number(el.max),
        step: Number(el.step) };
    }
  });
  inputBindings.register(numberInputBinding, 'shiny.numberInput');

  //---------------------------------------------------------------------
  // Source file: ../srcjs/input_binding_checkbox.js

  var checkboxInputBinding = new InputBinding();
  $.extend(checkboxInputBinding, {
    find: function find(scope) {
      return $(scope).find('input[type="checkbox"]');
    },
    getValue: function getValue(el) {
      return el.checked;
    },
    setValue: function setValue(el, value) {
      el.checked = value;
    },
    subscribe: function subscribe(el, callback) {
      $(el).on('change.checkboxInputBinding', function (event) {
        callback(true);
      });
    },
    unsubscribe: function unsubscribe(el) {
      $(el).off('.checkboxInputBinding');
    },
    getState: function getState(el) {
      return {
        label: $(el).parent().find('span').text(),
        value: el.checked
      };
    },
    receiveMessage: function receiveMessage(el, data) {
      if (data.hasOwnProperty('value')) el.checked = data.value;

      if (data.hasOwnProperty('label')) $(el).parent().find('span').text(data.label);

      $(el).trigger('change');
    }
  });
  inputBindings.register(checkboxInputBinding, 'shiny.checkboxInput');

  //---------------------------------------------------------------------
  // Source file: ../srcjs/input_binding_slider.js

  // Necessary to get hidden sliders to send their updated values
  function forceIonSliderUpdate(slider) {
    if (slider.$cache && slider.$cache.input) slider.$cache.input.trigger('change');else console.log("Couldn't force ion slider to update");
  }

  var sliderInputBinding = {};
  $.extend(sliderInputBinding, textInputBinding, {
    find: function find(scope) {
      // Check if ionRangeSlider plugin is loaded
      if (!$.fn.ionRangeSlider) return [];

      return $(scope).find('input.js-range-slider');
    },
    getType: function getType(el) {
      var dataType = $(el).data('data-type');
      if (dataType === 'date') return 'shiny.date';else if (dataType === 'datetime') return 'shiny.datetime';else return false;
    },
    getValue: function getValue(el) {
      var $el = $(el);
      var result = $(el).data('ionRangeSlider').result;

      // Function for converting numeric value from slider to appropriate type.
      var convert;
      var dataType = $el.data('data-type');
      if (dataType === 'date') {
        convert = function convert(val) {
          return formatDateUTC(new Date(+val));
        };
      } else if (dataType === 'datetime') {
        convert = function convert(val) {
          // Convert ms to s
          return +val / 1000;
        };
      } else {
        convert = function convert(val) {
          return +val;
        };
      }

      if (this._numValues(el) == 2) {
        return [convert(result.from), convert(result.to)];
      } else {
        return convert(result.from);
      }
    },
    setValue: function setValue(el, value) {
      var slider = $(el).data('ionRangeSlider');

      if (this._numValues(el) == 2 && value instanceof Array) {
        slider.update({ from: value[0], to: value[1] });
      } else {
        slider.update({ from: value });
      }
      forceIonSliderUpdate(slider);
    },
    subscribe: function subscribe(el, callback) {
      $(el).on('change.sliderInputBinding', function (event) {
        callback(!$(el).data('updating') && !$(el).data('animating'));
      });
    },
    unsubscribe: function unsubscribe(el) {
      $(el).off('.sliderInputBinding');
    },
    receiveMessage: function receiveMessage(el, data) {
      var $el = $(el);
      var slider = $el.data('ionRangeSlider');
      var msg = {};

      if (data.hasOwnProperty('value')) {
        if (this._numValues(el) == 2 && data.value instanceof Array) {
          msg.from = data.value[0];
          msg.to = data.value[1];
        } else {
          msg.from = data.value;
        }
      }
      if (data.hasOwnProperty('min')) msg.min = data.min;
      if (data.hasOwnProperty('max')) msg.max = data.max;
      if (data.hasOwnProperty('step')) msg.step = data.step;

      if (data.hasOwnProperty('label')) $el.parent().find('label[for="' + $escape(el.id) + '"]').text(data.label);

      $el.data('updating', true);
      try {
        slider.update(msg);
        forceIonSliderUpdate(slider);
      } finally {
        $el.data('updating', false);
      }
    },
    getRatePolicy: function getRatePolicy() {
      return {
        policy: 'debounce',
        delay: 250
      };
    },
    getState: function getState(el) {},
    initialize: function initialize(el) {
      var opts = {};
      var $el = $(el);
      var dataType = $el.data('data-type');
      var timeFormat = $el.data('time-format');
      var timeFormatter;

      // Set up formatting functions
      if (dataType === 'date') {
        timeFormatter = strftime.utc();
        opts.prettify = function (num) {
          return timeFormatter(timeFormat, new Date(num));
        };
      } else if (dataType === 'datetime') {
        var timezone = $el.data('timezone');
        if (timezone) timeFormatter = strftime.timezone(timezone);else timeFormatter = strftime;

        opts.prettify = function (num) {
          return timeFormatter(timeFormat, new Date(num));
        };
      }

      $el.ionRangeSlider(opts);
    },

    // Number of values; 1 for single slider, 2 for range slider
    _numValues: function _numValues(el) {
      if ($(el).data('ionRangeSlider').options.type === 'double') return 2;else return 1;
    }
  });
  inputBindings.register(sliderInputBinding, 'shiny.sliderInput');

  $(document).on('click', '.slider-animate-button', function (evt) {
    evt.preventDefault();
    var self = $(this);
    var target = $('#' + $escape(self.attr('data-target-id')));
    var startLabel = 'Play';
    var stopLabel = 'Pause';
    var loop = self.attr('data-loop') !== undefined && !/^\s*false\s*$/i.test(self.attr('data-loop'));
    var animInterval = self.attr('data-interval');
    if (isNaN(animInterval)) animInterval = 1500;else animInterval = +animInterval;

    if (!target.data('animTimer')) {
      var slider;
      var timer;

      // Separate code paths:
      // Backward compatible code for old-style jsliders (Shiny <= 0.10.2.2),
      // and new-style ionsliders.
      if (target.hasClass('jslider')) {
        slider = target.slider();

        // If we're currently at the end, restart
        if (!slider.canStepNext()) slider.resetToStart();

        timer = setInterval(function () {
          if (loop && !slider.canStepNext()) {
            slider.resetToStart();
          } else {
            slider.stepNext();
            if (!loop && !slider.canStepNext()) {
              self.click(); // stop the animation
            }
          }
        }, animInterval);
      } else {
        slider = target.data('ionRangeSlider');
        // Single sliders have slider.options.type == "single", and only the
        // `from` value is used. Double sliders have type == "double", and also
        // use the `to` value for the right handle.
        var sliderCanStep = function sliderCanStep() {
          if (slider.options.type === "double") return slider.result.to < slider.result.max;else return slider.result.from < slider.result.max;
        };
        var sliderReset = function sliderReset() {
          var val = { from: slider.result.min };
          // Preserve the current spacing for double sliders
          if (slider.options.type === "double") val.to = val.from + (slider.result.to - slider.result.from);

          slider.update(val);
          forceIonSliderUpdate(slider);
        };
        var sliderStep = function sliderStep() {
          // Don't overshoot the end
          var val = {
            from: Math.min(slider.result.max, slider.result.from + slider.options.step)
          };
          if (slider.options.type === "double") val.to = Math.min(slider.result.max, slider.result.to + slider.options.step);

          slider.update(val);
          forceIonSliderUpdate(slider);
        };

        // If we're currently at the end, restart
        if (!sliderCanStep()) sliderReset();

        timer = setInterval(function () {
          if (loop && !sliderCanStep()) {
            sliderReset();
          } else {
            sliderStep();
            if (!loop && !sliderCanStep()) {
              self.click(); // stop the animation
            }
          }
        }, animInterval);
      }

      target.data('animTimer', timer);
      self.attr('title', stopLabel);
      self.addClass('playing');
      target.data('animating', true);
    } else {
      clearTimeout(target.data('animTimer'));
      target.removeData('animTimer');
      self.attr('title', startLabel);
      self.removeClass('playing');
      target.removeData('animating');
    }
  });

  //---------------------------------------------------------------------
  // Source file: ../srcjs/input_binding_date.js

  var dateInputBinding = new InputBinding();
  $.extend(dateInputBinding, {
    find: function find(scope) {
      return $(scope).find('.shiny-date-input');
    },
    getType: function getType(el) {
      return "shiny.date";
    },
    // Return the date in an unambiguous format, yyyy-mm-dd (as opposed to a
    // format like mm/dd/yyyy)
    getValue: function getValue(el) {
      var date = $(el).find('input').data('datepicker').getUTCDate();
      return formatDateUTC(date);
    },
    // value must be an unambiguous string like '2001-01-01', or a Date object.
    setValue: function setValue(el, value) {
      // R's NA, which is null here will remove current value
      if (value === null) {
        $(el).find('input').val('').datepicker('update');
        return;
      }

      var date = this._newDate(value);
      // If date is invalid, do nothing
      if (isNaN(date)) return;

      $(el).find('input').datepicker('update', date);
    },
    getState: function getState(el) {
      var $el = $(el);
      var $input = $el.find('input');

      var min = $input.data('datepicker').startDate;
      var max = $input.data('datepicker').endDate;

      // Stringify min and max. If min and max aren't set, they will be
      // -Infinity and Infinity; replace these with null.
      min = min === -Infinity ? null : formatDateUTC(min);
      max = max === Infinity ? null : formatDateUTC(max);

      // startViewMode is stored as a number; convert to string
      var startview = $input.data('datepicker').startViewMode;
      if (startview === 2) startview = 'decade';else if (startview === 1) startview = 'year';else if (startview === 0) startview = 'month';

      return {
        label: $el.find('label[for="' + $escape(el.id) + '"]').text(),
        value: this.getValue(el),
        valueString: $input.val(),
        min: min,
        max: max,
        language: $input.data('datepicker').language,
        weekstart: $input.data('datepicker').weekStart,
        format: this._formatToString($input.data('datepicker').format),
        startview: startview
      };
    },
    receiveMessage: function receiveMessage(el, data) {
      var $input = $(el).find('input');

      if (data.hasOwnProperty('value')) this.setValue(el, data.value);

      if (data.hasOwnProperty('label')) $(el).find('label[for="' + $escape(el.id) + '"]').text(data.label);

      if (data.hasOwnProperty('min')) this._setMin($input[0], data.min);

      if (data.hasOwnProperty('max')) this._setMax($input[0], data.max);

      $(el).trigger('change');
    },
    subscribe: function subscribe(el, callback) {
      $(el).on('keyup.dateInputBinding input.dateInputBinding', function (event) {
        // Use normal debouncing policy when typing
        callback(true);
      });
      $(el).on('changeDate.dateInputBinding change.dateInputBinding', function (event) {
        // Send immediately when clicked
        callback(false);
      });
    },
    unsubscribe: function unsubscribe(el) {
      $(el).off('.dateInputBinding');
    },
    getRatePolicy: function getRatePolicy() {
      return {
        policy: 'debounce',
        delay: 250
      };
    },
    initialize: function initialize(el) {
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
    // Given a format object from a date picker, return a string
    _formatToString: function _formatToString(format) {
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
    _setMin: function _setMin(el, date) {
      if (date === null) {
        $(el).datepicker('setStartDate', null);
      } else {
        date = this._newDate(date);
        if (!isNaN(date)) $(el).datepicker('setStartDate', date);
      }
    },
    // Given an unambiguous date string or a Date object, set the max (end) date
    // null will unset.
    _setMax: function _setMax(el, date) {
      if (date === null) {
        $(el).datepicker('setEndDate', null);
      } else {
        date = this._newDate(date);
        if (!isNaN(date)) $(el).datepicker('setEndDate', date);
      }
    },
    // Given a date string of format yyyy-mm-dd, return a Date object with
    // that date at 12AM UTC.
    // If date is a Date object, return it unchanged.
    _newDate: function _newDate(date) {
      if (date instanceof Date) return date;
      if (!date) return null;

      // Get Date object - this will be at 12AM in UTC, but may print
      // differently at the Javascript console.
      var d = parseDate(date);

      // If invalid date, return null
      if (isNaN(d)) return null;

      return new Date(d.getTime());
    },
    // Given a Date object, return a Date object which has the same "clock time"
    // in UTC. For example, if input date is 2013-02-01 23:00:00 GMT-0600 (CST),
    // output will be 2013-02-01 23:00:00 UTC. Note that the JS console may
    // print this in local time, as "Sat Feb 02 2013 05:00:00 GMT-0600 (CST)".
    _dateAsUTC: function _dateAsUTC(date) {
      return new Date(date.getTime() - date.getTimezoneOffset() * 60000);
    }
  });
  inputBindings.register(dateInputBinding, 'shiny.dateInput');

  //---------------------------------------------------------------------
  // Source file: ../srcjs/input_binding_daterange.js

  var dateRangeInputBinding = {};
  $.extend(dateRangeInputBinding, dateInputBinding, {
    find: function find(scope) {
      return $(scope).find('.shiny-date-range-input');
    },
    // Return the date in an unambiguous format, yyyy-mm-dd (as opposed to a
    // format like mm/dd/yyyy)
    getValue: function getValue(el) {
      var $inputs = $(el).find('input');
      var start = $inputs.eq(0).data('datepicker').getUTCDate();
      var end = $inputs.eq(1).data('datepicker').getUTCDate();

      return [formatDateUTC(start), formatDateUTC(end)];
    },
    // value must be an array of unambiguous strings like '2001-01-01', or
    // Date objects.
    setValue: function setValue(el, value) {
      if (!(value instanceof Object)) {
        return;
      }

      // Get the start and end input objects
      var $inputs = $(el).find('input');

      // If value is undefined, don't try to set
      // null will remove the current value
      if (value.start !== undefined) {
        if (value.start === null) {
          $inputs.eq(0).val('').datepicker('update');
        } else {
          var start = this._newDate(value.start);
          $inputs.eq(0).datepicker('update', start);
        }
      }
      if (value.end !== undefined) {
        if (value.end === null) {
          $inputs.eq(1).val('').datepicker('update');
        } else {
          var end = this._newDate(value.end);
          $inputs.eq(1).datepicker('update', end);
        }
      }
    },
    getState: function getState(el) {
      var $el = $(el);
      var $inputs = $el.find('input');
      var $startinput = $inputs.eq(0);
      var $endinput = $inputs.eq(1);

      // For many of the properties, assume start and end have the same values
      var min = $startinput.data('datepicker').startDate;
      var max = $startinput.data('datepicker').endDate;

      // Stringify min and max. If min and max aren't set, they will be
      // -Infinity and Infinity; replace these with null.
      min = min === -Infinity ? null : formatDateUTC(min);
      max = max === Infinity ? null : formatDateUTC(max);

      // startViewMode is stored as a number; convert to string
      var startview = $startinput.data('datepicker').startViewMode;
      if (startview === 2) startview = 'decade';else if (startview === 1) startview = 'year';else if (startview === 0) startview = 'month';

      return {
        label: $el.find('label[for="' + $escape(el.id) + '"]').text(),
        value: this.getValue(el),
        valueString: [$startinput.val(), $endinput.val()],
        min: min,
        max: max,
        weekstart: $startinput.data('datepicker').weekStart,
        format: this._formatToString($startinput.data('datepicker').format),
        language: $startinput.data('datepicker').language,
        startview: startview
      };
    },
    receiveMessage: function receiveMessage(el, data) {
      var $el = $(el);
      var $inputs = $el.find('input');
      var $startinput = $inputs.eq(0);
      var $endinput = $inputs.eq(1);

      if (data.hasOwnProperty('value')) this.setValue(el, data.value);

      if (data.hasOwnProperty('label')) $el.find('label[for="' + $escape(el.id) + '"]').text(data.label);

      if (data.hasOwnProperty('min')) {
        this._setMin($startinput[0], data.min);
        this._setMin($endinput[0], data.min);
      }

      if (data.hasOwnProperty('max')) {
        this._setMax($startinput[0], data.max);
        this._setMax($endinput[0], data.max);
      }

      $el.trigger('change');
    },
    initialize: function initialize(el) {
      var $el = $(el);
      var $inputs = $el.find('input');
      var $startinput = $inputs.eq(0);
      var $endinput = $inputs.eq(1);

      var start = $startinput.data('initial-date');
      var end = $endinput.data('initial-date');

      // If empty/null, use local date, but as UTC
      if (start === undefined || start === null) start = this._dateAsUTC(new Date());

      if (end === undefined || end === null) end = this._dateAsUTC(new Date());

      this.setValue(el, { "start": start, "end": end });

      // // Set the start and end dates, from min-date and max-date. These always
      // // use yyyy-mm-dd format, instead of bootstrap-datepicker's built-in
      // // support for date-startdate and data-enddate, which use the current
      // // date format.
      this._setMin($startinput[0], $startinput.data('min-date'));
      this._setMin($endinput[0], $startinput.data('min-date'));
      this._setMax($startinput[0], $endinput.data('max-date'));
      this._setMax($endinput[0], $endinput.data('max-date'));
    },
    subscribe: function subscribe(el, callback) {
      $(el).on('keyup.dateRangeInputBinding input.dateRangeInputBinding', function (event) {
        // Use normal debouncing policy when typing
        callback(true);
      });
      $(el).on('changeDate.dateRangeInputBinding change.dateRangeInputBinding', function (event) {
        // Send immediately when clicked
        callback(false);
      });
    },
    unsubscribe: function unsubscribe(el) {
      $(el).off('.dateRangeInputBinding');
    }
  });
  inputBindings.register(dateRangeInputBinding, 'shiny.dateRangeInput');

  //---------------------------------------------------------------------
  // Source file: ../srcjs/input_binding_select.js

  var selectInputBinding = new InputBinding();
  $.extend(selectInputBinding, {
    find: function find(scope) {
      return $(scope).find('select');
    },
    getId: function getId(el) {
      return InputBinding.prototype.getId.call(this, el) || el.name;
    },
    getValue: function getValue(el) {
      return $(el).val();
    },
    setValue: function setValue(el, value) {
      var selectize = this._selectize(el);
      if (typeof selectize !== 'undefined') {
        selectize.setValue(value);
      } else $(el).val(value);
    },
    getState: function getState(el) {
      // Store options in an array of objects, each with with value and label
      var options = new Array(el.length);
      for (var i = 0; i < el.length; i++) {
        options[i] = { value: el[i].value,
          label: el[i].label };
      }

      return {
        label: $(el).parent().find('label[for="' + $escape(el.id) + '"]').text(),
        value: this.getValue(el),
        options: options
      };
    },
    receiveMessage: function receiveMessage(el, data) {
      var $el = $(el),
          selectize;

      // This will replace all the options
      if (data.hasOwnProperty('options')) {
        selectize = this._selectize(el);
        // Must destroy selectize before appending new options, otherwise
        // selectize will restore the original select
        if (selectize) selectize.destroy();
        // Clear existing options and add each new one
        $el.empty().append(data.options);
        this._selectize(el);
      }

      // re-initialize selectize
      if (data.hasOwnProperty('config')) {
        $el.parent().find('script[data-for="' + $escape(el.id) + '"]').replaceWith(data.config);
        this._selectize(el, true);
      }

      // use server-side processing for selectize
      if (data.hasOwnProperty('url')) {
        selectize = this._selectize(el);
        selectize.clearOptions();
        var thiz = this,
            loaded = false;
        selectize.settings.load = function (query, callback) {
          var settings = selectize.settings;
          $.ajax({
            url: data.url,
            data: {
              query: query,
              field: JSON.stringify([settings.searchField]),
              value: settings.valueField,
              conju: settings.searchConjunction,
              maxop: settings.maxOptions
            },
            type: 'GET',
            error: function error() {
              callback();
            },
            success: function success(res) {
              callback(res);
              if (!loaded && data.hasOwnProperty('value')) thiz.setValue(el, data.value);
              loaded = true;
            }
          });
        };
        // perform an empty search after changing the `load` function
        selectize.load(function (callback) {
          selectize.settings.load.apply(selectize, ['', callback]);
        });
      } else if (data.hasOwnProperty('value')) {
        this.setValue(el, data.value);
      }

      if (data.hasOwnProperty('label')) $(el).parent().parent().find('label[for="' + $escape(el.id) + '"]').text(data.label);

      $(el).trigger('change');
    },
    subscribe: function subscribe(el, callback) {
      $(el).on('change.selectInputBinding', function (event) {
        callback();
      });
    },
    unsubscribe: function unsubscribe(el) {
      $(el).off('.selectInputBinding');
    },
    initialize: function initialize(el) {
      this._selectize(el);
    },
    _selectize: function _selectize(el, update) {
      if (!$.fn.selectize) return undefined;
      var $el = $(el);
      var config = $el.parent().find('script[data-for="' + $escape(el.id) + '"]');
      if (config.length === 0) return undefined;
      var options = $.extend({
        labelField: 'label',
        valueField: 'value',
        searchField: ['label']
      }, JSON.parse(config.html()));
      // selectize created from selectInput()
      if (typeof config.data('nonempty') !== 'undefined') {
        options = $.extend(options, {
          onItemRemove: function onItemRemove(value) {
            if (this.getValue() === "") $("select#" + $escape(el.id)).empty().append($("<option/>", {
              "value": value,
              "selected": true
            })).trigger("change");
          },
          onDropdownClose: function onDropdownClose($dropdown) {
            if (this.getValue() === "") this.setValue($("select#" + $escape(el.id)).val());
          }
        });
      }
      // options that should be eval()ed
      if (config.data('eval') instanceof Array) $.each(config.data('eval'), function (i, x) {
        /*jshint evil: true*/
        options[x] = eval('(' + options[x] + ')');
      });
      var control = $el.selectize(options)[0].selectize;
      // .selectize() does not really update settings; must destroy and rebuild
      if (update) {
        var settings = $.extend(control.settings, options);
        control.destroy();
        control = $el.selectize(settings)[0].selectize;
      }
      return control;
    }
  });
  inputBindings.register(selectInputBinding, 'shiny.selectInput');

  //---------------------------------------------------------------------
  // Source file: ../srcjs/input_binding_radio.js

  var radioInputBinding = new InputBinding();
  $.extend(radioInputBinding, {
    find: function find(scope) {
      return $(scope).find('.shiny-input-radiogroup');
    },
    getValue: function getValue(el) {
      // Select the radio objects that have name equal to the grouping div's id
      return $('input:radio[name="' + $escape(el.id) + '"]:checked').val();
    },
    setValue: function setValue(el, value) {
      $('input:radio[name="' + $escape(el.id) + '"][value="' + $escape(value) + '"]').prop('checked', true);
    },
    getState: function getState(el) {
      var $objs = $('input:radio[name="' + $escape(el.id) + '"]');

      // Store options in an array of objects, each with with value and label
      var options = new Array($objs.length);
      for (var i = 0; i < options.length; i++) {
        options[i] = { value: $objs[i].value,
          label: this._getLabel($objs[i]) };
      }

      return {
        label: $(el).parent().find('label[for="' + $escape(el.id) + '"]').text(),
        value: this.getValue(el),
        options: options
      };
    },
    receiveMessage: function receiveMessage(el, data) {
      var $el = $(el);

      // This will replace all the options
      if (data.hasOwnProperty('options')) {
        // Clear existing options and add each new one
        $el.find('div.shiny-options-group').remove();
        // Backward compatibility: for HTML generated by shinybootstrap2 package
        $el.find('label.radio').remove();
        $el.append(data.options);
      }

      if (data.hasOwnProperty('value')) this.setValue(el, data.value);

      if (data.hasOwnProperty('label')) $(el).parent().find('label[for="' + $escape(el.id) + '"]').text(data.label);

      $(el).trigger('change');
    },
    subscribe: function subscribe(el, callback) {
      $(el).on('change.radioInputBinding', function (event) {
        callback();
      });
    },
    unsubscribe: function unsubscribe(el) {
      $(el).off('.radioInputBinding');
    },
    // Given an input DOM object, get the associated label. Handles labels
    // that wrap the input as well as labels associated with 'for' attribute.
    _getLabel: function _getLabel(obj) {
      // If <label><input /><span>label text</span></label>
      if (obj.parentNode.tagName === "LABEL") {
        return $.trim($(obj.parentNode).find('span').text());
      }

      return null;
    },
    // Given an input DOM object, set the associated label. Handles labels
    // that wrap the input as well as labels associated with 'for' attribute.
    _setLabel: function _setLabel(obj, value) {
      // If <label><input /><span>label text</span></label>
      if (obj.parentNode.tagName === "LABEL") {
        $(obj.parentNode).find('span').text(value);
      }

      return null;
    }

  });
  inputBindings.register(radioInputBinding, 'shiny.radioInput');

  //---------------------------------------------------------------------
  // Source file: ../srcjs/input_binding_checkboxgroup.js

  var checkboxGroupInputBinding = new InputBinding();
  $.extend(checkboxGroupInputBinding, {
    find: function find(scope) {
      return $(scope).find('.shiny-input-checkboxgroup');
    },
    getValue: function getValue(el) {
      // Select the checkbox objects that have name equal to the grouping div's id
      var $objs = $('input:checkbox[name="' + $escape(el.id) + '"]:checked');
      var values = new Array($objs.length);
      for (var i = 0; i < $objs.length; i++) {
        values[i] = $objs[i].value;
      }
      return values;
    },
    setValue: function setValue(el, value) {
      // Clear all checkboxes
      $('input:checkbox[name="' + $escape(el.id) + '"]').prop('checked', false);

      // Accept array
      if (value instanceof Array) {
        for (var i = 0; i < value.length; i++) {
          $('input:checkbox[name="' + $escape(el.id) + '"][value="' + $escape(value[i]) + '"]').prop('checked', true);
        }
        // Else assume it's a single value
      } else {
        $('input:checkbox[name="' + $escape(el.id) + '"][value="' + $escape(value) + '"]').prop('checked', true);
      }
    },
    getState: function getState(el) {
      var $objs = $('input:checkbox[name="' + $escape(el.id) + '"]');

      // Store options in an array of objects, each with with value and label
      var options = new Array($objs.length);
      for (var i = 0; i < options.length; i++) {
        options[i] = { value: $objs[i].value,
          label: this._getLabel($objs[i]) };
      }

      return { label: $(el).find('label[for="' + $escape(el.id) + '"]').text(),
        value: this.getValue(el),
        options: options
      };
    },
    receiveMessage: function receiveMessage(el, data) {
      var $el = $(el);

      // This will replace all the options
      if (data.hasOwnProperty('options')) {
        // Clear existing options and add each new one
        $el.find('div.shiny-options-group').remove();
        // Backward compatibility: for HTML generated by shinybootstrap2 package
        $el.find('label.checkbox').remove();
        $el.append(data.options);
      }

      if (data.hasOwnProperty('value')) this.setValue(el, data.value);

      if (data.hasOwnProperty('label')) $el.find('label[for="' + $escape(el.id) + '"]').text(data.label);

      $(el).trigger('change');
    },
    subscribe: function subscribe(el, callback) {
      $(el).on('change.checkboxGroupInputBinding', function (event) {
        callback();
      });
    },
    unsubscribe: function unsubscribe(el) {
      $(el).off('.checkboxGroupInputBinding');
    },
    // Given an input DOM object, get the associated label. Handles labels
    // that wrap the input as well as labels associated with 'for' attribute.
    _getLabel: function _getLabel(obj) {
      // If <label><input /><span>label text</span></label>
      if (obj.parentNode.tagName === "LABEL") {
        return $.trim($(obj.parentNode).find('span').text());
      }

      return null;
    },
    // Given an input DOM object, set the associated label. Handles labels
    // that wrap the input as well as labels associated with 'for' attribute.
    _setLabel: function _setLabel(obj, value) {
      // If <label><input /><span>label text</span></label>
      if (obj.parentNode.tagName === "LABEL") {
        $(obj.parentNode).find('span').text(value);
      }

      return null;
    }

  });
  inputBindings.register(checkboxGroupInputBinding, 'shiny.checkboxGroupInput');

  //---------------------------------------------------------------------
  // Source file: ../srcjs/input_binding_actionbutton.js

  var actionButtonInputBinding = new InputBinding();
  $.extend(actionButtonInputBinding, {
    find: function find(scope) {
      return $(scope).find(".action-button");
    },
    getValue: function getValue(el) {
      return $(el).data('val') || 0;
    },
    setValue: function setValue(el, value) {
      $(el).data('val', value);
    },
    getType: function getType(el) {
      return 'shiny.action';
    },
    subscribe: function subscribe(el, callback) {
      $(el).on("click.actionButtonInputBinding", function (e) {
        var $el = $(this);
        var val = $el.data('val') || 0;
        $el.data('val', val + 1);

        callback();
      });
    },
    getState: function getState(el) {
      return { value: this.getValue(el) };
    },
    receiveMessage: function receiveMessage(el, data) {
      var $el = $(el);

      // retrieve current label and icon
      var label = $el.text();
      var icon = '';

      // to check (and store) the previous icon, we look for a $el child
      // object that has an i tag, and some (any) class (this prevents
      // italicized text - which has an i tag but, usually, no class -
      // from being mistakenly selected)
      if ($el.find('i[class]').length > 0) {
        var icon_html = $el.find('i[class]')[0];
        if (icon_html == $el.children()[0]) {
          // another check for robustness
          icon = $(icon_html).prop('outerHTML');
        }
      }

      // update the requested properties
      if (data.hasOwnProperty('label')) label = data.label;
      if (data.hasOwnProperty('icon')) {
        icon = data.icon;
        // if the user entered icon=character(0), remove the icon
        if (icon.length === 0) icon = '';
      }

      // produce new html
      $el.html(icon + ' ' + label);
    },
    unsubscribe: function unsubscribe(el) {
      $(el).off(".actionButtonInputBinding");
    }
  });
  inputBindings.register(actionButtonInputBinding, 'shiny.actionButtonInput');

  $(document).on('click', 'a.action-button', function (e) {
    e.preventDefault();
  });

  //---------------------------------------------------------------------
  // Source file: ../srcjs/input_binding_tabinput.js

  var bootstrapTabInputBinding = new InputBinding();
  $.extend(bootstrapTabInputBinding, {
    find: function find(scope) {
      return $(scope).find('ul.nav.shiny-tab-input');
    },
    getValue: function getValue(el) {
      var anchor = $(el).find('li:not(.dropdown).active').children('a');
      if (anchor.length === 1) return this._getTabName(anchor);

      return null;
    },
    setValue: function setValue(el, value) {
      var self = this;
      var anchors = $(el).find('li:not(.dropdown)').children('a');
      anchors.each(function () {
        if (self._getTabName($(this)) === value) {
          $(this).tab('show');
          return false; // Break out of each()
        }
        return true;
      });
    },
    getState: function getState(el) {
      return { value: this.getValue(el) };
    },
    receiveMessage: function receiveMessage(el, data) {
      if (data.hasOwnProperty('value')) this.setValue(el, data.value);
    },
    subscribe: function subscribe(el, callback) {
      $(el).on('shown.bootstrapTabInputBinding shown.bs.tab.bootstrapTabInputBinding', function (event) {
        callback();
      });
    },
    unsubscribe: function unsubscribe(el) {
      $(el).off('.bootstrapTabInputBinding');
    },
    _getTabName: function _getTabName(anchor) {
      return anchor.attr('data-value') || anchor.text();
    }
  });
  inputBindings.register(bootstrapTabInputBinding, 'shiny.bootstrapTabInput');

  //---------------------------------------------------------------------
  // Source file: ../srcjs/input_binding_fileinput.js

  var IE8FileUploader = function IE8FileUploader(shinyapp, id, fileEl) {
    this.shinyapp = shinyapp;
    this.id = id;
    this.fileEl = fileEl;
    this.beginUpload();
  };
  (function () {
    this.beginUpload = function () {
      var self = this;
      // Create invisible frame
      var iframeId = 'shinyupload_iframe_' + this.id;
      this.iframe = document.createElement('iframe');
      this.iframe.id = iframeId;
      this.iframe.name = iframeId;
      this.iframe.setAttribute('style', 'position: fixed; top: 0; left: 0; width: 0; height: 0; border: none');
      $('body').append(this.iframe);
      var iframeDestroy = function iframeDestroy() {
        // Forces Shiny to flushReact, flush outputs, etc. Without this we get
        // invalidated reactives, but observers don't actually execute.
        self.shinyapp.makeRequest('uploadieFinish', [], function () {}, function () {});
        $(self.iframe).remove();
      };
      if (this.iframe.attachEvent) {
        this.iframe.attachEvent('onload', iframeDestroy);
      } else {
        this.iframe.onload = iframeDestroy;
      }

      this.form = document.createElement('form');
      this.form.method = 'POST';
      this.form.setAttribute('enctype', 'multipart/form-data');
      this.form.action = "session/" + encodeURI(this.shinyapp.config.sessionId) + "/uploadie/" + encodeURI(this.id);
      this.form.id = 'shinyupload_form_' + this.id;
      this.form.target = iframeId;
      $(this.form).insertAfter(this.fileEl).append(this.fileEl);
      this.form.submit();
    };
  }).call(IE8FileUploader.prototype);

  var FileUploader = function FileUploader(shinyapp, id, files) {
    this.shinyapp = shinyapp;
    this.id = id;
    FileProcessor.call(this, files);
  };
  $.extend(FileUploader.prototype, FileProcessor.prototype);
  (function () {
    this.makeRequest = function (method, args, onSuccess, onFailure, blobs) {
      this.shinyapp.makeRequest(method, args, onSuccess, onFailure, blobs);
    };
    this.onBegin = function (files, cont) {
      var self = this;

      // Reset progress bar
      this.$setError(null);
      this.$setActive(true);
      this.$setVisible(true);
      this.onProgress(null, 0);

      this.totalBytes = 0;
      this.progressBytes = 0;
      $.each(files, function (i, file) {
        self.totalBytes += file.size;
      });

      var fileInfo = $.map(files, function (file, i) {
        return {
          name: file.name,
          size: file.size,
          type: file.type
        };
      });

      this.makeRequest('uploadInit', [fileInfo], function (response) {
        self.jobId = response.jobId;
        self.uploadUrl = response.uploadUrl;
        cont();
      }, function (error) {
        self.onError(error);
      });
    };
    this.onFile = function (file, cont) {
      var self = this;
      this.onProgress(file, 0);

      $.ajax(this.uploadUrl, {
        type: 'POST',
        cache: false,
        xhr: function xhr() {
          var xhrVal = $.ajaxSettings.xhr();
          if (xhrVal.upload) {
            xhrVal.upload.onprogress = function (e) {
              if (e.lengthComputable) {
                self.onProgress(file, (self.progressBytes + e.loaded) / self.totalBytes);
              }
            };
          }
          return xhrVal;
        },
        data: file,
        processData: false,
        success: function success() {
          self.progressBytes += file.size;
          cont();
        },
        error: function error(jqXHR, textStatus, errorThrown) {
          self.onError(jqXHR.responseText || textStatus);
        }
      });
    };
    this.onComplete = function () {
      var self = this;
      this.makeRequest('uploadEnd', [this.jobId, this.id], function (response) {
        self.$setActive(false);
        self.onProgress(null, 1);
        self.$bar().text('Upload complete');
      }, function (error) {
        self.onError(error);
      });
      this.$bar().text('Finishing upload');
    };
    this.onError = function (message) {
      this.$setError(message || '');
      this.$setActive(false);
    };
    this.onAbort = function () {
      this.$setVisible(false);
    };
    this.onProgress = function (file, completed) {
      this.$bar().width(Math.round(completed * 100) + '%');
      this.$bar().text(file ? file.name : '');
    };
    this.$container = function () {
      return $('#' + $escape(this.id) + '_progress.shiny-file-input-progress');
    };
    this.$bar = function () {
      return $('#' + $escape(this.id) + '_progress.shiny-file-input-progress .progress-bar');
    };
    this.$setVisible = function (visible) {
      this.$container().css('visibility', visible ? 'visible' : 'hidden');
    };
    this.$setError = function (error) {
      this.$bar().toggleClass('bar-danger', error !== null);
      if (error !== null) {
        this.onProgress(null, 1);
        this.$bar().text(error);
      }
    };
    this.$setActive = function (active) {
      this.$container().toggleClass('active', !!active);
    };
  }).call(FileUploader.prototype);

  function uploadFiles(evt) {
    // If previously selected files are uploading, abort that.
    var $el = $(evt.target);
    var uploader = $el.data('currentUploader');
    if (uploader) uploader.abort();

    var files = evt.target.files;
    // IE8 here does not necessarily mean literally IE8; it indicates if the web
    // browser supports the FileList object (IE8/9 do not support it)
    var IE8 = typeof files === 'undefined';
    var id = fileInputBinding.getId(evt.target);

    if (!IE8 && files.length === 0) return;

    // Clear data-restore attribute if present.
    $el.removeAttr('data-restore');

    // Set the label in the text box
    var $fileText = $el.closest('div.input-group').find('input[type=text]');
    if (IE8) {
      // If we're using IE8/9, just use this placeholder
      $fileText.val("[Uploaded file]");
    } else if (files.length === 1) {
      $fileText.val(files[0].name);
    } else {
      $fileText.val(files.length + " files");
    }

    // Start the new upload and put the uploader in 'currentUploader'.
    if (IE8) {
      /*jshint nonew:false */
      new IE8FileUploader(exports.shinyapp, id, evt.target);
    } else {
      $el.data('currentUploader', new FileUploader(exports.shinyapp, id, files));
    }
  }

  var fileInputBinding = new InputBinding();
  $.extend(fileInputBinding, {
    find: function find(scope) {
      return $(scope).find('input[type="file"]');
    },
    getId: function getId(el) {
      return InputBinding.prototype.getId.call(this, el) || el.name;
    },
    getValue: function getValue(el) {
      // This returns a non-undefined value only when there's a 'data-restore'
      // attribute, which is set only when restoring Shiny state. If a file is
      // uploaded through the browser, 'data-restore' gets cleared.
      var data = $(el).attr('data-restore');
      if (data) {
        data = JSON.parse(data);

        // Set the label in the text box
        var $fileText = $(el).closest('div.input-group').find('input[type=text]');
        if (data.name.length === 1) {
          $fileText.val(data.name[0]);
        } else {
          $fileText.val(data.name.length + " files");
        }

        // Manually set up progress bar. A bit inelegant because it duplicates
        // code from FileUploader, but duplication is less bad than alternatives.
        var $progress = $(el).closest('div.form-group').find('.progress');
        var $bar = $progress.find('.progress-bar');
        $progress.removeClass('active');
        $bar.width('100%');
        $bar.css('visibility', 'visible');

        return data;
      } else {
        return null;
      }
    },
    setValue: function setValue(el, value) {
      // Not implemented
    },
    getType: function getType(el) {
      // This will be used only when restoring a file from a saved state.
      return 'shiny.file';
    },
    subscribe: function subscribe(el, callback) {
      $(el).on('change.fileInputBinding', uploadFiles);
    },
    unsubscribe: function unsubscribe(el) {
      $(el).off('.fileInputBinding');
    }
  });
  inputBindings.register(fileInputBinding, 'shiny.fileInputBinding');

  //---------------------------------------------------------------------
  // Source file: ../srcjs/init_shiny.js

  function initShiny() {

    var shinyapp = exports.shinyapp = new ShinyApp();

    function bindOutputs() {
      var scope = arguments.length <= 0 || arguments[0] === undefined ? document : arguments[0];

      scope = $(scope);

      var bindings = outputBindings.getBindings();

      for (var i = 0; i < bindings.length; i++) {
        var binding = bindings[i].binding;
        var matches = binding.find(scope) || [];
        for (var j = 0; j < matches.length; j++) {
          var el = matches[j];
          var id = binding.getId(el);

          // Check if ID is falsy
          if (!id) continue;

          var $el = $(el);
          if ($el.hasClass('shiny-bound-output')) {
            // Already bound; can happen with nested uiOutput (bindAll
            // gets called on two ancestors)
            continue;
          }

          var bindingAdapter = new OutputBindingAdapter(el, binding);
          shinyapp.bindOutput(id, bindingAdapter);
          $el.data('shiny-output-binding', bindingAdapter);
          $el.addClass('shiny-bound-output');
          $el.trigger({
            type: 'shiny:bound',
            binding: binding,
            bindingType: 'output'
          });
        }
      }

      // Send later in case DOM layout isn't final yet.
      setTimeout(sendImageSize, 0);
      setTimeout(sendOutputHiddenState, 0);
    }

    function unbindOutputs() {
      var scope = arguments.length <= 0 || arguments[0] === undefined ? document : arguments[0];
      var includeSelf = arguments.length <= 1 || arguments[1] === undefined ? false : arguments[1];

      var outputs = $(scope).find('.shiny-bound-output');

      if (includeSelf && $(scope).hasClass('shiny-bound-output')) {
        outputs.push(scope);
      }

      for (var i = 0; i < outputs.length; i++) {
        var $el = $(outputs[i]);
        var bindingAdapter = $el.data('shiny-output-binding');
        if (!bindingAdapter) continue;
        var id = bindingAdapter.binding.getId(outputs[i]);
        shinyapp.unbindOutput(id, bindingAdapter);
        $el.removeClass('shiny-bound-output');
        $el.removeData('shiny-output-binding');
        $el.trigger({
          type: 'shiny:unbound',
          binding: bindingAdapter.binding,
          bindingType: 'output'
        });
      }

      // Send later in case DOM layout isn't final yet.
      setTimeout(sendImageSize, 0);
      setTimeout(sendOutputHiddenState, 0);
    }

    var inputBatchSender = new InputBatchSender(shinyapp);
    var inputsNoResend = new InputNoResendDecorator(inputBatchSender);
    var inputsEvent = new InputEventDecorator(inputsNoResend);
    var inputsRate = new InputRateDecorator(inputsEvent);
    var inputsDefer = new InputDeferDecorator(inputsEvent);

    // By default, use rate decorator
    var inputs = inputsRate;
    $('input[type="submit"], button[type="submit"]').each(function () {
      // If there is a submit button on the page, use defer decorator
      inputs = inputsDefer;
      $(this).click(function (event) {
        event.preventDefault();
        inputsDefer.submit();
      });
    });

    exports.onInputChange = function (name, value) {
      inputs.setInput(name, value);
    };

    var boundInputs = {};

    function valueChangeCallback(binding, el, allowDeferred) {
      var id = binding.getId(el);
      if (id) {
        var value = binding.getValue(el);
        var type = binding.getType(el);
        if (type) id = id + ":" + type;
        inputs.setInput(id, value, !allowDeferred);
      }
    }

    function bindInputs() {
      var scope = arguments.length <= 0 || arguments[0] === undefined ? document : arguments[0];

      var bindings = inputBindings.getBindings();

      var currentValues = {};

      for (var i = 0; i < bindings.length; i++) {
        var binding = bindings[i].binding;
        var matches = binding.find(scope) || [];
        for (var j = 0; j < matches.length; j++) {
          var el = matches[j];
          var id = binding.getId(el);

          // Check if ID is falsy, or if already bound
          if (!id || boundInputs[id]) continue;

          var type = binding.getType(el);
          var effectiveId = type ? id + ":" + type : id;
          currentValues[effectiveId] = binding.getValue(el);

          /*jshint loopfunc:true*/
          var thisCallback = function () {
            var thisBinding = binding;
            var thisEl = el;
            return function (allowDeferred) {
              valueChangeCallback(thisBinding, thisEl, allowDeferred);
            };
          }();

          binding.subscribe(el, thisCallback);
          $(el).data('shiny-input-binding', binding);
          $(el).addClass('shiny-bound-input');
          var ratePolicy = binding.getRatePolicy(el);
          if (ratePolicy !== null) {
            inputsRate.setRatePolicy(effectiveId, ratePolicy.policy, ratePolicy.delay);
          }

          boundInputs[id] = {
            binding: binding,
            node: el
          };

          $(el).trigger({
            type: 'shiny:bound',
            binding: binding,
            bindingType: 'input'
          });

          if (shinyapp.isConnected()) {
            valueChangeCallback(binding, el, false);
          }
        }
      }

      return currentValues;
    }

    function unbindInputs() {
      var scope = arguments.length <= 0 || arguments[0] === undefined ? document : arguments[0];
      var includeSelf = arguments.length <= 1 || arguments[1] === undefined ? false : arguments[1];

      var inputs = $(scope).find('.shiny-bound-input');

      if (includeSelf && $(scope).hasClass('shiny-bound-input')) {
        inputs.push(scope);
      }

      for (var i = 0; i < inputs.length; i++) {
        var el = inputs[i];
        var binding = $(el).data('shiny-input-binding');
        if (!binding) continue;
        var id = binding.getId(el);
        $(el).removeClass('shiny-bound-input');
        delete boundInputs[id];
        binding.unsubscribe(el);
        $(el).trigger({
          type: 'shiny:unbound',
          binding: binding,
          bindingType: 'input'
        });
      }
    }

    function _bindAll(scope) {
      bindOutputs(scope);
      return bindInputs(scope);
    }
    function unbindAll(scope) {
      var includeSelf = arguments.length <= 1 || arguments[1] === undefined ? false : arguments[1];

      unbindInputs(scope, includeSelf);
      unbindOutputs(scope, includeSelf);
    }
    exports.bindAll = function (scope) {
      // _bindAll alone returns initial values, it doesn't send them to the
      // server. export.bindAll needs to send the values to the server, so we
      // wrap _bindAll in a closure that does that.
      var currentValues = _bindAll(scope);
      $.each(currentValues, function (name, value) {
        inputs.setInput(name, value);
      });

      // Not sure if the iframe stuff is an intrinsic part of bindAll, but bindAll
      // is a convenient place to hang it. bindAll will be called anytime new HTML
      // appears that might contain inputs/outputs; it's reasonable to assume that
      // any such HTML may contain iframes as well.
      initDeferredIframes();
    };
    exports.unbindAll = unbindAll;

    // Calls .initialize() for all of the input objects in all input bindings,
    // in the given scope.
    function initializeInputs() {
      var scope = arguments.length <= 0 || arguments[0] === undefined ? document : arguments[0];

      var bindings = inputBindings.getBindings();

      // Iterate over all bindings
      for (var i = 0; i < bindings.length; i++) {
        var binding = bindings[i].binding;
        var inputObjects = binding.find(scope) || [];

        // Iterate over all input objects for this binding
        for (var j = 0; j < inputObjects.length; j++) {
          if (!inputObjects[j]._shiny_initialized) {
            inputObjects[j]._shiny_initialized = true;
            binding.initialize(inputObjects[j]);
          }
        }
      }
    }
    exports.initializeInputs = initializeInputs;

    function getIdFromEl(el) {
      var $el = $(el);
      var bindingAdapter = $el.data("shiny-output-binding");
      if (!bindingAdapter) return null;else return bindingAdapter.getId();
    }

    // Initialize all input objects in the document, before binding
    initializeInputs(document);

    var initialValues = _bindAll(document);

    // The server needs to know the size of each image and plot output element,
    // in case it is auto-sizing
    $('.shiny-image-output, .shiny-plot-output').each(function () {
      var id = getIdFromEl(this);
      if (this.offsetWidth !== 0 || this.offsetHeight !== 0) {
        initialValues['.clientdata_output_' + id + '_width'] = this.offsetWidth;
        initialValues['.clientdata_output_' + id + '_height'] = this.offsetHeight;
      }
    });
    function doSendImageSize() {
      $('.shiny-image-output, .shiny-plot-output').each(function () {
        var id = getIdFromEl(this);
        if (this.offsetWidth !== 0 || this.offsetHeight !== 0) {
          inputs.setInput('.clientdata_output_' + id + '_width', this.offsetWidth);
          inputs.setInput('.clientdata_output_' + id + '_height', this.offsetHeight);
        }
      });
      $('.shiny-bound-output').each(function () {
        var $this = $(this),
            binding = $this.data('shiny-output-binding');
        $this.trigger({
          type: 'shiny:visualchange',
          visible: !isHidden(this),
          binding: binding
        });
        binding.onResize();
      });
    }
    var sendImageSizeDebouncer = new Debouncer(null, doSendImageSize, 0);
    function sendImageSize() {
      sendImageSizeDebouncer.normalCall();
    }
    // Make sure sendImageSize actually gets called before the inputBatchSender
    // sends data to the server.
    inputBatchSender.lastChanceCallback.push(function () {
      if (sendImageSizeDebouncer.isPending()) sendImageSizeDebouncer.immediateCall();
    });

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
        return isHidden(obj.parentNode);
      }
    }
    var lastKnownVisibleOutputs = {};
    // Set initial state of outputs to hidden, if needed
    $('.shiny-bound-output').each(function () {
      var id = getIdFromEl(this);
      if (isHidden(this)) {
        initialValues['.clientdata_output_' + id + '_hidden'] = true;
      } else {
        lastKnownVisibleOutputs[id] = true;
        initialValues['.clientdata_output_' + id + '_hidden'] = false;
      }
    });
    // Send update when hidden state changes
    function doSendOutputHiddenState() {
      var visibleOutputs = {};
      $('.shiny-bound-output').each(function () {
        var id = getIdFromEl(this);
        delete lastKnownVisibleOutputs[id];
        // Assume that the object is hidden when width and height are 0
        var hidden = isHidden(this),
            evt = {
          type: 'shiny:visualchange',
          visible: !hidden
        };
        if (hidden) {
          inputs.setInput('.clientdata_output_' + id + '_hidden', true);
        } else {
          visibleOutputs[id] = true;
          inputs.setInput('.clientdata_output_' + id + '_hidden', false);
        }
        var $this = $(this);
        evt.binding = $this.data('shiny-output-binding');
        $this.trigger(evt);
      });
      // Anything left in lastKnownVisibleOutputs is orphaned
      for (var name in lastKnownVisibleOutputs) {
        if (lastKnownVisibleOutputs.hasOwnProperty(name)) inputs.setInput('.clientdata_output_' + name + '_hidden', true);
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
    inputBatchSender.lastChanceCallback.push(function () {
      if (sendOutputHiddenStateDebouncer.isPending()) sendOutputHiddenStateDebouncer.immediateCall();
    });

    // Given a namespace and a handler function, return a function that invokes
    // the handler only when e's namespace matches. For example, if the
    // namespace is "bs", it would match when e.namespace is "bs" or "bs.tab".
    // If the namespace is "bs.tab", it would match for "bs.tab", but not "bs".
    function filterEventsByNamespace(namespace, handler) {
      namespace = namespace.split(".");

      return function (e) {
        var eventNamespace = e.namespace.split(".");

        // If any of the namespace strings aren't present in this event, quit.
        for (var i = 0; i < namespace.length; i++) {
          if (eventNamespace.indexOf(namespace[i]) === -1) return;
        }

        handler.apply(this, arguments);
      };
    }

    // The size of each image may change either because the browser window was
    // resized, or because a tab was shown/hidden (hidden elements report size
    // of 0x0). It's OK to over-report sizes because the input pipeline will
    // filter out values that haven't changed.
    $(window).resize(debounce(500, sendImageSize));
    // Need to register callbacks for each Bootstrap 3 class.
    var bs3classes = ['modal', 'dropdown', 'tab', 'tooltip', 'popover', 'collapse'];
    $.each(bs3classes, function (idx, classname) {
      $('body').on('shown.bs.' + classname + '.sendImageSize', '*', filterEventsByNamespace('bs', sendImageSize));
      $('body').on('shown.bs.' + classname + '.sendOutputHiddenState ' + 'hidden.bs.' + classname + '.sendOutputHiddenState', '*', filterEventsByNamespace('bs', sendOutputHiddenState));
    });

    // This is needed for Bootstrap 2 compatibility and for non-Bootstrap
    // related shown/hidden events (like conditionalPanel)
    $('body').on('shown.sendImageSize', '*', sendImageSize);
    $('body').on('shown.sendOutputHiddenState hidden.sendOutputHiddenState', '*', sendOutputHiddenState);

    // Send initial pixel ratio, and update it if it changes
    initialValues['.clientdata_pixelratio'] = pixelRatio();
    $(window).resize(function () {
      inputs.setInput('.clientdata_pixelratio', pixelRatio());
    });

    // Send initial URL
    initialValues['.clientdata_url_protocol'] = window.location.protocol;
    initialValues['.clientdata_url_hostname'] = window.location.hostname;
    initialValues['.clientdata_url_port'] = window.location.port;
    initialValues['.clientdata_url_pathname'] = window.location.pathname;
    initialValues['.clientdata_url_search'] = window.location.search;
    // This is only the initial value of the hash. The hash can change, but
    // a reactive version of this isn't sent because w atching for changes can
    // require polling on some browsers. The JQuery hashchange plugin can be
    // used if this capability is important.
    initialValues['.clientdata_url_hash_initial'] = window.location.hash;

    // The server needs to know what singletons were rendered as part of
    // the page loading
    var singletonText = initialValues['.clientdata_singletons'] = $('script[type="application/shiny-singletons"]').text();
    singletons.registerNames(singletonText.split(/,/));

    var dependencyText = $('script[type="application/html-dependencies"]').text();
    $.each(dependencyText.split(/;/), function (i, depStr) {
      var match = /\s*^(.+)\[(.+)\]\s*$/.exec(depStr);
      if (match) {
        registerDependency(match[1], match[2]);
      }
    });

    // We've collected all the initial values--start the server process!
    inputsNoResend.reset(initialValues);
    shinyapp.connect(initialValues);
    $(document).one("shiny:connected", function () {
      initDeferredIframes();
    });
  } // function initShiny()


  // Give any deferred iframes a chance to load.
  function initDeferredIframes() {
    if (!window.Shiny || !window.Shiny.shinyapp || !window.Shiny.shinyapp.isConnected()) {
      // If somehow we accidentally call this before the server connection is
      // established, just ignore the call. At the time of this writing it
      // doesn't happen, but it's easy to imagine a later refactoring putting
      // us in this situation and it'd be hard to notice with either manual
      // testing or automated tests, because the only effect is on HTTP request
      // timing. (Update: Actually Aron saw this being called without even
      // window.Shiny being defined, but it was hard to repro.)
      return;
    }

    $(".shiny-frame-deferred").each(function (i, el) {
      var $el = $(el);
      $el.removeClass("shiny-frame-deferred");
      $el.attr("src", $el.attr("data-deferred-src"));
      $el.attr("data-deferred-src", null);
    });
  }

  $(function () {
    // Init Shiny a little later than document ready, so user code can
    // run first (i.e. to register bindings)
    setTimeout(initShiny, 1);
  });

  //---------------------------------------------------------------------
  // Source file: ../srcjs/reactlog.js

  $(document).on('keydown', function (e) {
    if (e.which !== 114 || !e.ctrlKey && !e.metaKey || e.shiftKey || e.altKey) return;
    var url = 'reactlog?w=' + window.escape(exports.shinyapp.config.workerId) + "&s=" + window.escape(exports.shinyapp.config.sessionId);
    window.open(url);
    e.preventDefault();
  });

  //---------------------------------------------------------------------
  // Source file: ../srcjs/_end.js
})();

//# sourceMappingURL=shiny.js.map
//# sourceMappingURL=shiny.js.map
