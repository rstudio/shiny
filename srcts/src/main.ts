/* eslint "@typescript-eslint/ban-ts-comment": 0 */
// @ts-nocheck
/* eslint "camelcase": 0 */
/* eslint "@typescript-eslint/no-unused-vars": 0 */
/* eslint "@typescript-eslint/no-this-alias": 0 */
/* eslint "@typescript-eslint/no-empty-function": 0 */
/* eslint "no-prototype-builtins": 0 */
/* eslint "prefer-const": 0 */
/* eslint "no-constant-condition": 0 */

import $ from "jquery";
jQuery = $;

import {
  escapeHTML,
  randomId,
  strToBool,
  getStyle,
  padZeros,
  roundSignif,
  parseDate,
  formatDateUTC,
  makeResizeFilter,
  pixelRatio,
  scopeExprToFunc,
  asArray,
  mergeSort,
  $escape,
  mapValues,
  isnan,
  _equal,
  equal,
  compareVersion,
  updateLabel,
  getComputedLinkColor,
  makeBlob,
} from "./utils";

import { isQt, isIE, IEVersion } from "./utils/browser";

import { FileProcessor } from "./file/FileProcessor";

import { Shiny } from "./shiny";

function main(): void {
  // "_start.js"
  // √

  // "utils.js"
  // √

  // "browser.js"
  // √

  // "input_rate.js"
  const Invoker = function (target, func) {
    this.target = target;
    this.func = func;
  };

  (function () {
    this.normalCall = this.immediateCall = function (...args) {
      this.func.apply(this.target, args);
    };
  }.call(Invoker.prototype));

  const Debouncer = function (target, func, delayMs) {
    this.target = target;
    this.func = func;
    this.delayMs = delayMs;

    this.timerId = null;
    this.args = null;
  };

  (function () {
    this.normalCall = function (...args) {
      const self = this;

      this.$clearTimer();
      this.args = args;

      this.timerId = setTimeout(function () {
        // IE8 doesn't reliably clear timeout, so this additional
        // check is needed
        if (self.timerId === null) return;
        self.$clearTimer();
        self.$invoke();
      }, this.delayMs);
    };
    this.immediateCall = function (...args) {
      this.$clearTimer();
      this.args = args;
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
  }.call(Debouncer.prototype));

  const Throttler = function (target, func, delayMs) {
    this.target = target;
    this.func = func;
    this.delayMs = delayMs;

    this.timerId = null;
    this.args = null;
  };

  (function () {
    this.normalCall = function (...args) {
      const self = this;

      this.args = args;
      if (this.timerId === null) {
        this.$invoke();
        this.timerId = setTimeout(function () {
          // IE8 doesn't reliably clear timeout, so this additional
          // check is needed
          if (self.timerId === null) return;
          self.$clearTimer();
          if (args.length > 0) self.normalCall.apply(self, ...args);
        }, this.delayMs);
      }
    };
    this.immediateCall = function (...args) {
      this.$clearTimer();
      this.args = args;
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
  }.call(Throttler.prototype));

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
    let timerId = null;
    let self, args;

    return function (...args) {
      self = this;
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
    let executionPending = false;
    let timerId = null;
    let self, args;

    function throttled(...argumentVals) {
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
        func.apply(this, argumentVals);
      } else {
        // Something executed recently. Don't do anything
        // except set up target/arguments to be called later
        executionPending = true;
        self = this;
        args = argumentVals;
      }
    }
    return throttled;
  }

  // Schedules data to be sent to shinyapp at the next setTimeout(0).
  // Batches multiple input calls into one websocket message.
  const InputBatchSender = function (shinyapp) {
    this.shinyapp = shinyapp;
    this.timerId = null;
    this.pendingData = {};
    this.reentrant = false;
    this.lastChanceCallback = [];
  };

  (function () {
    this.setInput = function (nameType, value, opts) {
      this.pendingData[nameType] = value;

      if (!this.reentrant) {
        if (opts.priority === "event") {
          this.$sendNow();
        } else if (!this.timerId) {
          this.timerId = setTimeout(this.$sendNow.bind(this), 0);
        }
      }
    };

    this.$sendNow = function () {
      if (this.reentrant) {
        console.trace("Unexpected reentrancy in InputBatchSender!");
      }

      this.reentrant = true;
      try {
        this.timerId = null;
        $.each(this.lastChanceCallback, (i, callback) => {
          callback();
        });
        const currentData = this.pendingData;

        this.pendingData = {};
        this.shinyapp.sendInput(currentData);
      } finally {
        this.reentrant = false;
      }
    };
  }.call(InputBatchSender.prototype));

  const InputNoResendDecorator = function (target, initialValues) {
    this.target = target;
    this.lastSentValues = this.reset(initialValues);
  };

  (function () {
    this.setInput = function (nameType, value, opts) {
      const { name: inputName, inputType: inputType } = splitInputNameType(
        nameType
      );
      const jsonValue = JSON.stringify(value);

      if (
        opts.priority !== "event" &&
        this.lastSentValues[inputName] &&
        this.lastSentValues[inputName].jsonValue === jsonValue &&
        this.lastSentValues[inputName].inputType === inputType
      ) {
        return;
      }
      this.lastSentValues[inputName] = { jsonValue, inputType };
      this.target.setInput(nameType, value, opts);
    };
    this.reset = function (values = {}) {
      // Given an object with flat name-value format:
      //   { x: "abc", "y.shiny.number": 123 }
      // Create an object in cache format and save it:
      //   { x: { jsonValue: '"abc"', inputType: "" },
      //     y: { jsonValue: "123", inputType: "shiny.number" } }
      const cacheValues = {};

      for (const inputName in values) {
        if (values.hasOwnProperty(inputName)) {
          const { name, inputType } = splitInputNameType(inputName);

          cacheValues[name] = {
            jsonValue: JSON.stringify(values[inputName]),
            inputType: inputType,
          };
        }
      }

      this.lastSentValues = cacheValues;
    };
    this.forget = function (name) {
      delete this.lastSentValues[name];
    };
  }.call(InputNoResendDecorator.prototype));

  const InputEventDecorator = function (target) {
    this.target = target;
  };

  (function () {
    this.setInput = function (nameType, value, opts) {
      const evt = jQuery.Event("shiny:inputchanged");

      const input = splitInputNameType(nameType);

      evt.name = input.name;
      evt.inputType = input.inputType;
      evt.value = value;
      evt.binding = opts.binding;
      evt.el = opts.el;
      evt.priority = opts.priority;

      $(opts.el).trigger(evt);

      if (!evt.isDefaultPrevented()) {
        let name = evt.name;

        if (evt.inputType !== "") name += ":" + evt.inputType;

        // Most opts aren't passed along to lower levels in the input decorator
        // stack.
        this.target.setInput(name, evt.value, { priority: opts.priority });
      }
    };
  }.call(InputEventDecorator.prototype));

  const InputRateDecorator = function (target) {
    this.target = target;
    this.inputRatePolicies = {};
  };

  (function () {
    // Note that the first argument of setInput() and setRatePolicy()
    // are passed both the input name (i.e., inputId) and type.
    // https://github.com/rstudio/shiny/blob/67d3a/srcjs/init_shiny.js#L111-L126
    // However, $ensureInit() and $doSetInput() are meant to be passed just
    // the input name (i.e., inputId), which is why we distinguish between
    // nameType and name.
    this.setInput = function (nameType, value, opts) {
      const { name: inputName } = splitInputNameType(nameType);

      this.$ensureInit(inputName);

      if (opts.priority !== "deferred")
        this.inputRatePolicies[inputName].immediateCall(nameType, value, opts);
      else this.inputRatePolicies[inputName].normalCall(nameType, value, opts);
    };
    this.setRatePolicy = function (nameType, mode, millis) {
      const { name: inputName } = splitInputNameType(nameType);

      if (mode === "direct") {
        this.inputRatePolicies[inputName] = new Invoker(this, this.$doSetInput);
      } else if (mode === "debounce") {
        this.inputRatePolicies[inputName] = new Debouncer(
          this,
          this.$doSetInput,
          millis
        );
      } else if (mode === "throttle") {
        this.inputRatePolicies[inputName] = new Throttler(
          this,
          this.$doSetInput,
          millis
        );
      }
    };
    this.$ensureInit = function (name) {
      if (!(name in this.inputRatePolicies)) this.setRatePolicy(name, "direct");
    };
    this.$doSetInput = function (nameType, value, opts) {
      this.target.setInput(nameType, value, opts);
    };
  }.call(InputRateDecorator.prototype));

  const InputDeferDecorator = function (target) {
    this.target = target;
    this.pendingInput = {};
  };

  (function () {
    this.setInput = function (nameType, value, opts) {
      if (/^\./.test(nameType)) this.target.setInput(nameType, value, opts);
      else this.pendingInput[nameType] = { value, opts };
    };
    this.submit = function () {
      for (const nameType in this.pendingInput) {
        if (this.pendingInput.hasOwnProperty(nameType)) {
          const { value, opts } = this.pendingInput[nameType];

          this.target.setInput(nameType, value, opts);
        }
      }
    };
  }.call(InputDeferDecorator.prototype));

  const InputValidateDecorator = function (target) {
    this.target = target;
  };

  (function () {
    this.setInput = function (nameType, value, opts) {
      if (!nameType) throw "Can't set input with empty name.";

      opts = addDefaultInputOpts(opts);

      this.target.setInput(nameType, value, opts);
    };
  }.call(InputValidateDecorator.prototype));

  // Merge opts with defaults, and return a new object.
  function addDefaultInputOpts(opts) {
    opts = $.extend(
      {
        priority: "immediate",
        binding: null,
        el: null,
      },
      opts
    );

    if (opts && typeof opts.priority !== "undefined") {
      switch (opts.priority) {
        case "deferred":
        case "immediate":
        case "event":
          break;
        default:
          throw new Error(
            "Unexpected input value mode: '" + opts.priority + "'"
          );
      }
    }

    return opts;
  }

  function splitInputNameType(nameType) {
    const name2 = nameType.split(":");

    return {
      name: name2[0],
      inputType: name2.length > 1 ? name2[1] : "",
    };
  }

  // "shinyapp.js"
  const ShinyApp = function () {
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

  (function () {
    this.connect = function (initialInput) {
      if (this.$socket)
        throw "Connect was already called on this application object";

      this.$socket = this.createSocket();
      this.$initialInput = initialInput;
      $.extend(this.$inputValues, initialInput);

      this.$updateConditionals();
    };

    this.isConnected = function () {
      return !!this.$socket;
    };

    let scheduledReconnect = null;

    this.reconnect = function () {
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
      const self = this;

      const createSocketFunc =
        Shiny.createSocket ||
        function () {
          let protocol = "ws:";

          if (window.location.protocol === "https:") protocol = "wss:";

          let defaultPath = window.location.pathname;
          // some older WebKit browsers return the pathname already decoded;
          // if we find invalid URL characters in the path, encode them

          if (!/^([$#!&-;=?-[\]_a-z~]|%[0-9a-fA-F]{2})+$/.test(defaultPath)) {
            defaultPath = encodeURI(defaultPath);
            // Bizarrely, QtWebKit requires us to encode these characters *twice*
            if (isQt()) {
              defaultPath = encodeURI(defaultPath);
            }
          }
          if (!/\/$/.test(defaultPath)) defaultPath += "/";
          defaultPath += "websocket/";

          const ws = new WebSocket(
            protocol + "//" + window.location.host + defaultPath
          );

          ws.binaryType = "arraybuffer";

          return ws;
        };

      const socket = createSocketFunc();
      let hasOpened = false;

      socket.onopen = function () {
        hasOpened = true;

        $(document).trigger({
          type: "shiny:connected",
          socket: socket,
        });

        self.onConnected();

        socket.send(
          JSON.stringify({
            method: "init",
            data: self.$initialInput,
          })
        );

        while (self.$pendingMessages.length) {
          const msg = self.$pendingMessages.shift();

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
            type: "shiny:disconnected",
            socket: socket,
          });

          self.$notifyDisconnected();
        }

        self.onDisconnected(); // Must be run before self.$removeSocket()
        self.$removeSocket();
      };
      return socket;
    };

    this.sendInput = function (values) {
      const msg = JSON.stringify({
        method: "update",
        data: values,
      });

      this.$sendMsg(msg);

      $.extend(this.$inputValues, values);
      this.$updateConditionals();
    };

    this.$notifyDisconnected = function () {
      if (window.parent) {
        window.parent.postMessage("disconnected", "*");
      }
    };

    this.$removeSocket = function () {
      this.$socket = null;
    };

    this.$scheduleReconnect = function (delay) {
      const self = this;

      scheduledReconnect = setTimeout(function () {
        self.reconnect();
      }, delay);
    };

    // How long should we wait before trying the next reconnection?
    // The delay will increase with subsequent attempts.
    // .next: Return the time to wait for next connection, and increment counter.
    // .reset: Reset the attempt counter.
    const reconnectDelay = (function () {
      let attempts = 0;
      // Time to wait before each reconnection attempt. If we go through all of
      // these values, repeated use the last one. Add 500ms to each one so that
      // in the last 0.5s, it shows "..."
      const delays = [1500, 1500, 2500, 2500, 5500, 5500, 10500];

      return {
        next: function () {
          let i = attempts;
          // Instead of going off the end, use the last one

          if (i >= delays.length) {
            i = delays.length - 1;
          }

          attempts++;
          return delays[i];
        },
        reset: function () {
          attempts = 0;
        },
      };
    })();

    this.onDisconnected = function () {
      // Add gray-out overlay, if not already present
      const $overlay = $("#shiny-disconnected-overlay");

      if ($overlay.length === 0) {
        $(document.body).append('<div id="shiny-disconnected-overlay"></div>');
      }

      // To try a reconnect, both the app (this.$allowReconnect) and the
      // server (this.$socket.allowReconnect) must allow reconnections, or
      // session$allowReconnect("force") was called. The "force" option should
      // only be used for testing.
      if (
        (this.$allowReconnect === true &&
          this.$socket.allowReconnect === true) ||
        this.$allowReconnect === "force"
      ) {
        const delay = reconnectDelay.next();

        Shiny.showReconnectDialog(delay);
        this.$scheduleReconnect(delay);
      }
    };

    this.onConnected = function () {
      $("#shiny-disconnected-overlay").remove();
      Shiny.hideReconnectDialog();
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
      let requestId = this.$nextRequestId;

      while (this.$activeRequests[requestId]) {
        requestId = (requestId + 1) % 1000000000;
      }
      this.$nextRequestId = requestId + 1;

      this.$activeRequests[requestId] = {
        onSuccess: onSuccess,
        onError: onError,
      };

      let msg = JSON.stringify({
        method: method,
        args: args,
        tag: requestId,
      });

      if (blobs) {
        // We have binary data to transfer; form a different kind of packet.
        // Start with a 4-byte signature, then for each blob, emit 4 bytes for
        // the length followed by the blob. The json payload is UTF-8 encoded
        // and used as the first blob.

        const uint32_to_buf = function (val) {
          const buffer = new ArrayBuffer(4);
          const view = new DataView(buffer);

          view.setUint32(0, val, true); // little-endian
          return buffer;
        };

        const payload = [];

        payload.push(uint32_to_buf(0x01020202)); // signature

        const jsonBuf = makeBlob([msg]);

        payload.push(uint32_to_buf(jsonBuf.size));
        payload.push(jsonBuf);

        for (let i = 0; i < blobs.length; i++) {
          payload.push(
            uint32_to_buf(blobs[i].byteLength || blobs[i].size || 0)
          );
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

      const binding = this.$bindings[name];
      const evt = jQuery.Event("shiny:error");

      evt.name = name;
      evt.error = error;
      evt.binding = binding;
      $(binding ? binding.el : document).trigger(evt);
      if (!evt.isDefaultPrevented() && binding && binding.onValueError) {
        binding.onValueError(evt.error);
      }
    };

    this.receiveOutput = function (name, value) {
      const binding = this.$bindings[name];
      const evt = jQuery.Event("shiny:value");

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

    this.bindOutput = function (id, binding) {
      if (!id) throw "Can't bind an element with no ID";
      if (this.$bindings[id]) throw "Duplicate binding for ID " + id;
      this.$bindings[id] = binding;

      if (this.$values[id] !== undefined)
        binding.onValueChange(this.$values[id]);
      else if (this.$errors[id] !== undefined)
        binding.onValueError(this.$errors[id]);

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

    // Narrows a scopeComponent -- an input or output object -- to one constrained
    // by nsPrefix. Returns a new object with keys removed and renamed as
    // necessary.
    function narrowScopeComponent(scopeComponent, nsPrefix) {
      return Object.keys(scopeComponent)
        .filter((k) => k.indexOf(nsPrefix) === 0)
        .map((k) => ({ [k.substring(nsPrefix.length)]: scopeComponent[k] }))
        .reduce((obj, pair) => $.extend(obj, pair), {});
    }

    // Narrows a scope -- an object with input and output "subComponents" -- to
    // one constrained by the nsPrefix string.
    //
    // If nsPrefix is null or empty, returns scope without modification.
    //
    // Otherwise, returns a new object with keys in subComponents removed and
    // renamed as necessary.
    function narrowScope(scope, nsPrefix) {
      if (nsPrefix) {
        return {
          input: narrowScopeComponent(scope.input, nsPrefix),
          output: narrowScopeComponent(scope.output, nsPrefix),
        };
      }
      return scope;
    }

    this.$updateConditionals = function () {
      $(document).trigger({
        type: "shiny:conditional",
      });

      const inputs = {};

      // Input keys use "name:type" format; we don't want the user to
      // have to know about the type suffix when referring to inputs.
      for (const name in this.$inputValues) {
        if (this.$inputValues.hasOwnProperty(name)) {
          const shortName = name.replace(/:.*/, "");

          inputs[shortName] = this.$inputValues[name];
        }
      }

      const scope = { input: inputs, output: this.$values };

      const conditionals = $(document).find("[data-display-if]");

      for (let i = 0; i < conditionals.length; i++) {
        const el = $(conditionals[i]);
        let condFunc = el.data("data-display-if-func");

        if (!condFunc) {
          const condExpr = el.attr("data-display-if");

          condFunc = scopeExprToFunc(condExpr);
          el.data("data-display-if-func", condFunc);
        }

        const nsPrefix = el.attr("data-ns-prefix");
        const nsScope = narrowScope(scope, nsPrefix);
        const show = condFunc(nsScope);
        const showing = el.css("display") !== "none";

        if (show !== showing) {
          if (show) {
            el.trigger("show");
            el.show();
            el.trigger("shown");
          } else {
            el.trigger("hide");
            el.hide();
            el.trigger("hidden");
          }
        }
      }
    };

    // Message handler management functions =================================

    // Records insertion order of handlers. Maps number to name. This is so
    // we can dispatch messages to handlers in the order that handlers were
    // added.
    const messageHandlerOrder = [];
    // Keep track of handlers by name. Maps name to handler function.
    const messageHandlers = {};

    // Two categories of message handlers: those that are from Shiny, and those
    // that are added by the user. The Shiny ones handle messages in
    // msgObj.values, msgObj.errors, and so on. The user ones handle messages
    // in msgObj.custom.foo and msgObj.custom.bar.
    const customMessageHandlerOrder = [];
    const customMessageHandlers = {};

    // Adds Shiny (internal) message handler
    function addMessageHandler(type, handler) {
      if (messageHandlers[type]) {
        throw 'handler for message of type "' + type + '" already added.';
      }
      if (typeof handler !== "function") {
        throw "handler must be a function.";
      }
      if (handler.length !== 1) {
        throw "handler must be a function that takes one argument.";
      }
      messageHandlerOrder.push(type);
      messageHandlers[type] = handler;
    }

    // Adds custom message handler - this one is exposed to the user
    function addCustomMessageHandler(type, handler) {
      // Remove any previously defined handlers so that only the most recent one
      // will be called
      if (customMessageHandlers[type]) {
        const typeIdx = customMessageHandlerOrder.indexOf(type);

        if (typeIdx !== -1) {
          customMessageHandlerOrder.splice(typeIdx, 1);
          delete customMessageHandlers[type];
        }
      }
      if (typeof handler !== "function") {
        throw "handler must be a function.";
      }
      if (handler.length !== 1) {
        throw "handler must be a function that takes one argument.";
      }

      customMessageHandlerOrder.push(type);
      customMessageHandlers[type] = handler;
    }

    Shiny.addCustomMessageHandler = addCustomMessageHandler;

    this.dispatchMessage = function (data) {
      let msgObj = {};

      if (typeof data === "string") {
        msgObj = JSON.parse(data);
      } else {
        // data is arraybuffer
        const len = new DataView(data, 0, 1).getUint8(0);
        const typedv = new DataView(data, 1, len);
        const typebuf = [];

        for (let i = 0; i < len; i++) {
          typebuf.push(String.fromCharCode(typedv.getUint8(i)));
        }
        const type = typebuf.join("");

        data = data.slice(len + 1);
        msgObj.custom = {};
        msgObj.custom[type] = data;
      }

      const evt = jQuery.Event("shiny:message");

      evt.message = msgObj;
      $(document).trigger(evt);
      if (evt.isDefaultPrevented()) return;

      // Send msgObj.foo and msgObj.bar to appropriate handlers
      this._sendMessagesToHandlers(
        evt.message,
        messageHandlers,
        messageHandlerOrder
      );

      this.$updateConditionals();
    };

    // A function for sending messages to the appropriate handlers.
    // - msgObj: the object containing messages, with format {msgObj.foo, msObj.bar
    this._sendMessagesToHandlers = function (msgObj, handlers, handlerOrder) {
      // Dispatch messages to handlers, if handler is present
      for (let i = 0; i < handlerOrder.length; i++) {
        const msgType = handlerOrder[i];

        if (msgObj.hasOwnProperty(msgType)) {
          // Execute each handler with 'this' referring to the present value of
          // 'this'
          handlers[msgType].call(this, msgObj[msgType]);
        }
      }
    };

    // Message handlers =====================================================

    addMessageHandler("values", function (message) {
      for (const name in this.$bindings) {
        if (this.$bindings.hasOwnProperty(name))
          this.$bindings[name].showProgress(false);
      }

      for (const key in message) {
        if (message.hasOwnProperty(key)) this.receiveOutput(key, message[key]);
      }
    });

    addMessageHandler("errors", function (message) {
      for (const key in message) {
        if (message.hasOwnProperty(key)) this.receiveError(key, message[key]);
      }
    });

    addMessageHandler("inputMessages", function (message) {
      // inputMessages should be an array
      for (let i = 0; i < message.length; i++) {
        const $obj = $(".shiny-bound-input#" + $escape(message[i].id));
        const inputBinding = $obj.data("shiny-input-binding");

        // Dispatch the message to the appropriate input object
        if ($obj.length > 0) {
          if (!$obj.attr("aria-live")) $obj.attr("aria-live", "polite");
          const el = $obj[0];
          const evt = jQuery.Event("shiny:updateinput");

          evt.message = message[i].message;
          evt.binding = inputBinding;
          $(el).trigger(evt);
          if (!evt.isDefaultPrevented())
            inputBinding.receiveMessage(el, evt.message);
        }
      }
    });

    addMessageHandler("javascript", function (message) {
      /*jshint evil: true */
      eval(message);
    });

    addMessageHandler("console", function (message) {
      for (let i = 0; i < message.length; i++) {
        if (console.log) console.log(message[i]);
      }
    });

    addMessageHandler("progress", function (message) {
      if (message.type && message.message) {
        const handler = progressHandlers[message.type];

        if (handler) handler.call(this, message.message);
      }
    });

    addMessageHandler("notification", function (message) {
      if (message.type === "show") Shiny.notifications.show(message.message);
      else if (message.type === "remove")
        Shiny.notifications.remove(message.message);
      else throw "Unkown notification type: " + message.type;
    });

    addMessageHandler("modal", function (message) {
      if (message.type === "show") Shiny.modal.show(message.message);
      else if (message.type === "remove") Shiny.modal.remove();
      // For 'remove', message content isn't used
      else throw "Unkown modal type: " + message.type;
    });

    addMessageHandler("response", function (message) {
      const requestId = message.tag;
      const request = this.$activeRequests[requestId];

      if (request) {
        delete this.$activeRequests[requestId];
        if ("value" in message) request.onSuccess(message.value);
        else request.onError(message.error);
      }
    });

    addMessageHandler("allowReconnect", function (message) {
      if (message === true || message === false || message === "force") {
        this.$allowReconnect = message;
      } else {
        throw "Invalid value for allowReconnect: " + message;
      }
    });

    addMessageHandler("custom", function (message) {
      // For old-style custom messages - should deprecate and migrate to new
      // method
      if (Shiny.oncustommessage) {
        Shiny.oncustommessage(message);
      }

      // Send messages.foo and messages.bar to appropriate handlers
      this._sendMessagesToHandlers(
        message,
        customMessageHandlers,
        customMessageHandlerOrder
      );
    });

    addMessageHandler("config", function (message) {
      this.config = {
        workerId: message.workerId,
        sessionId: message.sessionId,
      };
      if (message.user) Shiny.user = message.user;
      $(document).trigger("shiny:sessioninitialized");
    });

    addMessageHandler("busy", function (message) {
      if (message === "busy") {
        $(document.documentElement).addClass("shiny-busy");
        $(document).trigger("shiny:busy");
      } else if (message === "idle") {
        $(document.documentElement).removeClass("shiny-busy");
        $(document).trigger("shiny:idle");
      }
    });

    addMessageHandler("recalculating", function (message) {
      if (message.hasOwnProperty("name") && message.hasOwnProperty("status")) {
        const binding = this.$bindings[message.name];

        $(binding ? binding.el : null).trigger({
          type: "shiny:" + message.status,
        });
      }
    });

    addMessageHandler("reload", function (message) {
      window.location.reload();
    });

    addMessageHandler("shiny-insert-ui", function (message) {
      const targets = $(message.selector);

      if (targets.length === 0) {
        // render the HTML and deps to a null target, so
        // the side-effect of rendering the deps, singletons,
        // and <head> still occur
        console.warn(
          'The selector you chose ("' +
            message.selector +
            '") could not be found in the DOM.'
        );
        Shiny.renderHtml(message.content.html, $([]), message.content.deps);
      } else {
        targets.each(function (i, target) {
          Shiny.renderContent(target, message.content, message.where);
          return message.multiple;
        });
      }
    });

    addMessageHandler("shiny-remove-ui", function (message) {
      const els = $(message.selector);

      els.each(function (i, el) {
        Shiny.unbindAll(el, true);
        $(el).remove();
        // If `multiple` is false, returning false terminates the function
        // and no other elements are removed; if `multiple` is true,
        // returning true continues removing all remaining elements.
        return message.multiple;
      });
    });

    addMessageHandler("frozen", function (message) {
      for (let i = 0; i < message.ids.length; i++) {
        Shiny.forgetLastInputValue(message.ids[i]);
      }
    });

    function getTabset(id) {
      const $tabset = $("#" + $escape(id));

      if ($tabset.length === 0)
        throw (
          "There is no tabsetPanel (or navbarPage or navlistPanel) " +
          "with id equal to '" +
          id +
          "'"
        );
      return $tabset;
    }

    function getTabContent($tabset) {
      const tabsetId = $tabset.attr("data-tabsetid");
      const $tabContent = $(
        "div.tab-content[data-tabsetid='" + $escape(tabsetId) + "']"
      );

      return $tabContent;
    }

    function getTargetTabs($tabset, $tabContent, target) {
      const dataValue = "[data-value='" + $escape(target) + "']";
      const $aTag = $tabset.find("a" + dataValue);
      const $liTag = $aTag.parent();

      if ($liTag.length === 0) {
        throw (
          "There is no tabPanel (or navbarMenu) with value" +
          " (or menuName) equal to '" +
          target +
          "'"
        );
      }
      const $liTags = [];
      const $divTags = [];

      if ($aTag.attr("data-toggle") === "dropdown") {
        // dropdown
        const $dropdownTabset = $aTag.find("+ ul.dropdown-menu");
        const dropdownId = $dropdownTabset.attr("data-tabsetid");

        const $dropdownLiTags = $dropdownTabset
          .find("a[data-toggle='tab']")
          .parent("li");

        $dropdownLiTags.each(function (i, el) {
          $liTags.push($(el));
        });
        const selector = "div.tab-pane[id^='tab-" + $escape(dropdownId) + "']";
        const $dropdownDivs = $tabContent.find(selector);

        $dropdownDivs.each(function (i, el) {
          $divTags.push($(el));
        });
      } else {
        // regular tab
        $divTags.push($tabContent.find("div" + dataValue));
      }
      return { $liTag: $liTag, $liTags: $liTags, $divTags: $divTags };
    }

    addMessageHandler("shiny-insert-tab", function (message) {
      const $parentTabset = getTabset(message.inputId);
      let $tabset = $parentTabset;
      const $tabContent = getTabContent($tabset);
      let tabsetId = $parentTabset.attr("data-tabsetid");

      const $divTag = $(message.divTag.html);
      const $liTag = $(message.liTag.html);
      const $aTag = $liTag.find("> a");

      // Unless the item is being prepended/appended, the target tab
      // must be provided
      let target = null;
      let $targetLiTag = null;

      if (message.target !== null) {
        target = getTargetTabs($tabset, $tabContent, message.target);
        $targetLiTag = target.$liTag;
      }

      // If the item is to be placed inside a navbarMenu (dropdown),
      // change the value of $tabset from the parent's ul tag to the
      // dropdown's ul tag
      const dropdown = getDropdown();

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
        const index = getTabIndex($tabset, tabsetId);
        const tabId = "tab-" + tabsetId + "-" + index;

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

      Shiny.renderContent($liTag[0], {
        html: $liTag.html(),
        deps: message.liTag.deps,
      });
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
      Shiny.renderContent(
        $tabContent[0],
        { html: "", deps: message.divTag.deps },
        "beforeend"
      );
      $divTag.get().forEach((el) => {
        // Must not use jQuery for appending el to the doc, we don't want any
        // scripts to run (since they will run when renderContent takes a crack).
        $tabContent[0].appendChild(el);
        // If `el` itself is a script tag, this approach won't work (the script
        // won't be run), since we're only sending innerHTML through renderContent
        // and not the whole tag. That's fine in this case because we control the
        // R code that generates this HTML, and we know that the element is not
        // a script tag.
        Shiny.renderContent(el, el.innerHTML || el.textContent);
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
        const existingTabIds = [0];
        // loop through all existing tabs, find the one with highest id
        // (since this is based on a numeric counter), and increment

        $tabset.find("> li").each(function () {
          const $tab = $(this).find("> a[data-toggle='tab']");

          if ($tab.length > 0) {
            // remove leading url if it exists. (copy of bootstrap url stripper)
            const href = $tab.attr("href").replace(/.*(?=#[^\s]+$)/, "");
            // remove tab id to get the index
            const index = href.replace("#tab-" + tabsetId + "-", "");

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
          const $dropdownATag = $(
            "a.dropdown-toggle[data-value='" + $escape(message.menuName) + "']"
          );

          if ($dropdownATag.length === 0) {
            throw (
              "There is no navbarMenu with menuName equal to '" +
              message.menuName +
              "'"
            );
          }
          const $dropdownTabset = $dropdownATag.find("+ ul.dropdown-menu");
          const dropdownId = $dropdownTabset.attr("data-tabsetid");

          return { $tabset: $dropdownTabset, id: dropdownId };
        } else if (message.target !== null) {
          // if our item is to be placed next to a tab that is inside
          // a navbarMenu, our item will also be inside
          const $uncleTabset = $targetLiTag.parent("ul");

          if ($uncleTabset.hasClass("dropdown-menu")) {
            const uncleId = $uncleTabset.attr("data-tabsetid");

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
        const destTabValue = getFirstTab($tabset);
        const inputBinding = $tabset.data("shiny-input-binding");
        const evt = jQuery.Event("shiny:updateinput");

        evt.binding = inputBinding;
        $tabset.trigger(evt);
        inputBinding.setValue($tabset[0], destTabValue);
      }
    }

    // Given a tabset ul jquery object, return the value of the first tab
    // (in document order) that's visible and able to be selected.
    function getFirstTab($ul) {
      return (
        $ul
          .find("li:visible a[data-toggle='tab']")
          .first()
          .attr("data-value") || null
      );
    }

    function tabApplyFunction(target, func, liTags = false) {
      $.each(target, function (key, el) {
        if (key === "$liTag") {
          // $liTag is always just one jQuery element
          func(el);
        } else if (key === "$divTags") {
          // $divTags is always an array (even if length = 1)
          $.each(el, function (i, div) {
            func(div);
          });
        } else if (liTags && key === "$liTags") {
          // $liTags is always an array (even if length = 0)
          $.each(el, function (i, div) {
            func(div);
          });
        }
      });
    }

    addMessageHandler("shiny-remove-tab", function (message) {
      const $tabset = getTabset(message.inputId);
      const $tabContent = getTabContent($tabset);
      const target = getTargetTabs($tabset, $tabContent, message.target);

      tabApplyFunction(target, removeEl);

      ensureTabsetHasVisibleTab($tabset);

      function removeEl($el) {
        Shiny.unbindAll($el, true);
        $el.remove();
      }
    });

    addMessageHandler("shiny-change-tab-visibility", function (message) {
      const $tabset = getTabset(message.inputId);
      const $tabContent = getTabContent($tabset);
      const target = getTargetTabs($tabset, $tabContent, message.target);

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

    addMessageHandler("updateQueryString", function (message) {
      // leave the bookmarking code intact
      if (message.mode === "replace") {
        window.history.replaceState(null, null, message.queryString);
        return;
      }

      let what = null;

      if (message.queryString.charAt(0) === "#") what = "hash";
      else if (message.queryString.charAt(0) === "?") what = "query";
      else
        throw (
          "The 'query' string must start with either '?' " +
          "(to update the query string) or with '#' (to " +
          "update the hash)."
        );

      const path = window.location.pathname;
      const oldQS = window.location.search;
      const oldHash = window.location.hash;

      /* Barbara -- December 2016
    Note: we could check if the new QS and/or hash are different
    from the old one(s) and, if not, we could choose not to push
    a new state (whether or not we would replace it is moot/
    inconsequential). However, I think that it is better to
    interpret each call to `updateQueryString` as representing
    new state (even if the message.queryString is the same), so
    that check isn't even performed as of right now.
    */

      let relURL = path;

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

    addMessageHandler("resetBrush", function (message) {
      Shiny.resetBrush(message.brushId);
    });

    // Progress reporting ====================================================

    const progressHandlers = {
      // Progress for a particular object
      binding: function (message) {
        const key = message.id;
        const binding = this.$bindings[key];

        if (binding) {
          $(binding.el).trigger({
            type: "shiny:outputinvalidated",
            binding: binding,
            name: key,
          });
          if (binding.showProgress) binding.showProgress(true);
        }
      },

      // Open a page-level progress bar
      open: function (message) {
        if (message.style === "notification") {
          // For new-style (starting in Shiny 0.14) progress indicators that use
          // the notification API.

          // Progress bar starts hidden; will be made visible if a value is provided
          // during updates.
          Shiny.notifications.show({
            html:
              `<div id="shiny-progress-${message.id}" class="shiny-progress-notification">` +
              '<div class="progress active" style="display: none;"><div class="progress-bar"></div></div>' +
              '<div class="progress-text">' +
              '<span class="progress-message">message</span> ' +
              '<span class="progress-detail"></span>' +
              "</div>" +
              "</div>",
            id: message.id,
            duration: null,
          });
        } else if (message.style === "old") {
          // For old-style (Shiny <=0.13.2) progress indicators.

          // Add progress container (for all progress items) if not already present
          let $container = $(".shiny-progress-container");

          if ($container.length === 0) {
            $container = $('<div class="shiny-progress-container"></div>');
            $(document.body).append($container);
          }

          // Add div for just this progress ID
          const depth = $(".shiny-progress.open").length;
          // The 'bar' class is needed for backward compatibility with Bootstrap 2.
          const $progress = $(
            '<div class="shiny-progress open">' +
              '<div class="progress active"><div class="progress-bar bar"></div></div>' +
              '<div class="progress-text">' +
              '<span class="progress-message">message</span>' +
              '<span class="progress-detail"></span>' +
              "</div>" +
              "</div>"
          );

          $progress.attr("id", message.id);
          $container.append($progress);

          // Stack bars
          const $progressBar = $progress.find(".progress");

          $progressBar.css("top", depth * $progressBar.height() + "px");

          // Stack text objects
          const $progressText = $progress.find(".progress-text");

          $progressText.css(
            "top",
            3 * $progressBar.height() +
              depth * $progressText.outerHeight() +
              "px"
          );

          $progress.hide();
        }
      },

      // Update page-level progress bar
      update: function (message) {
        if (message.style === "notification") {
          // For new-style (starting in Shiny 0.14) progress indicators that use
          // the notification API.
          const $progress = $("#shiny-progress-" + message.id);

          if ($progress.length === 0) return;

          if (typeof message.message !== "undefined") {
            $progress.find(".progress-message").text(message.message);
          }
          if (typeof message.detail !== "undefined") {
            $progress.find(".progress-detail").text(message.detail);
          }
          if (typeof message.value !== "undefined" && message.value !== null) {
            $progress.find(".progress").show();
            $progress.find(".progress-bar").width(message.value * 100 + "%");
          }
        } else if (message.style === "old") {
          // For old-style (Shiny <=0.13.2) progress indicators.

          const $progress = $("#" + message.id + ".shiny-progress");

          if (typeof message.message !== "undefined") {
            $progress.find(".progress-message").text(message.message);
          }
          if (typeof message.detail !== "undefined") {
            $progress.find(".progress-detail").text(message.detail);
          }
          if (typeof message.value !== "undefined" && message.value !== null) {
            $progress.find(".progress").show();
            $progress.find(".bar").width(message.value * 100 + "%");
          }

          $progress.fadeIn();
        }
      },

      // Close page-level progress bar
      close: function (message) {
        if (message.style === "notification") {
          Shiny.notifications.remove(message.id);
        } else if (message.style === "old") {
          const $progress = $("#" + message.id + ".shiny-progress");

          $progress.removeClass("open");

          $progress.fadeOut({
            complete: function () {
              $progress.remove();

              // If this was the last shiny-progress, remove container
              if ($(".shiny-progress").length === 0)
                $(".shiny-progress-container").remove();
            },
          });
        }
      },
    };

    Shiny.progressHandlers = progressHandlers;

    // Returns a URL which can be queried to get values from inside the server
    // function. This is enabled with `options(shiny.testmode=TRUE)`.
    this.getTestSnapshotBaseUrl = function ({ fullUrl = true } = {}) {
      const loc = window.location;
      let url = "";

      if (fullUrl) {
        // Strip off everything after last slash in path, like dirname() in R
        url = loc.origin + loc.pathname.replace(/\/[^/]*$/, "");
      }
      url +=
        "/session/" +
        encodeURIComponent(this.config.sessionId) +
        "/dataobj/shinytest?w=" +
        encodeURIComponent(this.config.workerId) +
        "&nonce=" +
        randomId();

      return url;
    };
  }.call(ShinyApp.prototype));

  Shiny.showReconnectDialog = (function () {
    let reconnectTime = null;

    function updateTime() {
      const $time = $("#shiny-reconnect-time");
      // If the time has been removed, exit and don't reschedule this function.

      if ($time.length === 0) return;

      const seconds = Math.floor((reconnectTime - new Date().getTime()) / 1000);

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
      if ($("#shiny-reconnect-text").length > 0) return;

      const html =
        '<span id="shiny-reconnect-text">Attempting to reconnect</span>' +
        '<span id="shiny-reconnect-time"></span>';
      const action =
        '<a id="shiny-reconnect-now" href="#" onclick="Shiny.shinyapp.reconnect();">Try now</a>';

      Shiny.notifications.show({
        id: "reconnect",
        html: html,
        action: action,
        duration: null,
        closeButton: false,
        type: "warning",
      });

      updateTime();
    };
  })();

  Shiny.hideReconnectDialog = function () {
    Shiny.notifications.remove("reconnect");
  };

  // "notifications.js"
  Shiny.notifications = (function () {
    // Milliseconds to fade in or out
    const fadeDuration = 250;

    function show({
      html = "",
      action = "",
      deps = [],
      duration = 5000,
      id = null,
      closeButton = true,
      type = null,
    } = {}) {
      if (!id) id = randomId();

      // Create panel if necessary
      _createPanel();

      // Get existing DOM element for this ID, or create if needed.
      let $notification = _get(id);

      if ($notification.length === 0) $notification = _create(id);

      // Render html and dependencies
      const newHtml =
        `<div class="shiny-notification-content-text">${html}</div>` +
        `<div class="shiny-notification-content-action">${action}</div>`;
      const $content = $notification.find(".shiny-notification-content");

      Shiny.renderContent($content, { html: newHtml, deps: deps });

      // Remove any existing classes of the form 'shiny-notification-xxxx'.
      // The xxxx would be strings like 'warning'.
      const classes = $notification
        .attr("class")
        .split(/\s+/)
        .filter((cls) => cls.match(/^shiny-notification-/))
        .join(" ");

      $notification.removeClass(classes);

      // Add class. 'default' means no additional CSS class.
      if (type && type !== "default")
        $notification.addClass("shiny-notification-" + type);

      // Make sure that the presence/absence of close button matches with value
      // of `closeButton`.
      const $close = $notification.find(".shiny-notification-close");

      if (closeButton && $close.length === 0) {
        $notification.append(
          '<div class="shiny-notification-close">&times;</div>'
        );
      } else if (!closeButton && $close.length !== 0) {
        $close.remove();
      }

      // If duration was provided, schedule removal. If not, clear existing
      // removal callback (this happens if a message was first added with
      // a duration, and then updated with no duration).
      if (duration) _addRemovalCallback(id, duration);
      else _clearRemovalCallback(id);

      return id;
    }

    function remove(id) {
      _get(id).fadeOut(fadeDuration, function () {
        Shiny.unbindAll(this);
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
      return _getPanel().find("#shiny-notification-" + $escape(id));
    }

    // Return array of all notification IDs
    function _ids() {
      return _getPanel()
        .find(".shiny-notification")
        .map(function () {
          return this.id.replace(/shiny-notification-/, "");
        })
        .get();
    }

    // Returns the notification panel DOM object (wrapped in jQuery).
    function _getPanel() {
      return $("#shiny-notification-panel");
    }

    // Create notifications panel and return the jQuery object. If the DOM
    // element already exists, just return it.
    function _createPanel() {
      const $panel = _getPanel();

      if ($panel.length > 0) return $panel;

      $(document.body).append('<div id="shiny-notification-panel">');

      return $panel;
    }

    // Create a notification DOM element and return the jQuery object. If the
    // DOM element already exists for the ID, just return it without creating.
    function _create(id) {
      let $notification = _get(id);

      if ($notification.length === 0) {
        $notification = $(
          `<div id="shiny-notification-${id}" class="shiny-notification">` +
            '<div class="shiny-notification-close">&times;</div>' +
            '<div class="shiny-notification-content"></div>' +
            "</div>"
        );

        $notification.find(".shiny-notification-close").on("click", (e) => {
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
      const removalCallback = setTimeout(function () {
        remove(id);
      }, delay);

      _get(id).data("removalCallback", removalCallback);
    }

    // Clear a removal callback from a notification, if present.
    function _clearRemovalCallback(id) {
      const $notification = _get(id);
      const oldRemovalCallback = $notification.data("removalCallback");

      if (oldRemovalCallback) {
        clearTimeout(oldRemovalCallback);
      }
    }

    return {
      show,
      remove,
    };
  })();

  // "modal.js"
  Shiny.modal = {
    // Show a modal dialog. This is meant to handle two types of cases: one is
    // that the content is a Bootstrap modal dialog, and the other is that the
    // content is non-Bootstrap. Bootstrap modals require some special handling,
    // which is coded in here.
    show: function ({ html = "", deps = [] } = {}) {
      // If there was an existing Bootstrap modal, then there will be a modal-
      // backdrop div that was added outside of the modal wrapper, and it must be
      // removed; otherwise there can be multiple of these divs.
      $(".modal-backdrop").remove();

      // Get existing wrapper DOM element, or create if needed.
      let $modal = $("#shiny-modal-wrapper");

      if ($modal.length === 0) {
        $modal = $('<div id="shiny-modal-wrapper"></div>');
        $(document.body).append($modal);

        // If the wrapper's content is a Bootstrap modal, then when the inner
        // modal is hidden, remove the entire thing, including wrapper.
        $modal.on("hidden.bs.modal", function (e) {
          if (e.target === $("#shiny-modal")[0]) {
            Shiny.unbindAll($modal);
            $modal.remove();
          }
        });
      }

      $modal.on("keydown.shinymodal", function (e) {
        // If we're listening for Esc, don't let the event propagate. See
        // https://github.com/rstudio/shiny/issues/1453. The value of
        // data("keyboard") needs to be checked inside the handler, because at
        // the time that $modal.on() is called, the $("#shiny-modal") div doesn't
        // yet exist.
        if ($("#shiny-modal").data("keyboard") === false) return;

        if (e.keyCode === 27) {
          e.stopPropagation();
          e.preventDefault();
        }
      });

      // Set/replace contents of wrapper with html.
      Shiny.renderContent($modal, { html: html, deps: deps });
    },

    remove: function () {
      const $modal = $("#shiny-modal-wrapper");

      $modal.off("keydown.shinymodal");

      // Look for a Bootstrap modal and if present, trigger hide event. This will
      // trigger the hidden.bs.modal callback that we set in show(), which unbinds
      // and removes the element.
      if ($modal.find(".modal").length > 0) {
        $modal.find(".modal").modal("hide");
      } else {
        // If not a Bootstrap modal dialog, simply unbind and remove it.
        Shiny.unbindAll($modal);
        $modal.remove();
      }
    },
  };

  // "file_processor.js"
  // √

  // "binding_registry.js"
  const BindingRegistry = function () {
    this.bindings = [];
    this.bindingNames = {};
  };

  (function () {
    this.register = function (binding, bindingName, priority) {
      const bindingObj = { binding: binding, priority: priority || 0 };

      this.bindings.unshift(bindingObj);
      if (bindingName) {
        this.bindingNames[bindingName] = bindingObj;
        binding.name = bindingName;
      }
    };
    this.setPriority = function (bindingName, priority) {
      const bindingObj = this.bindingNames[bindingName];

      if (!bindingObj)
        throw "Tried to set priority on unknown binding " + bindingName;
      bindingObj.priority = priority || 0;
    };
    this.getPriority = function (bindingName) {
      const bindingObj = this.bindingNames[bindingName];

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
  }.call(BindingRegistry.prototype));

  const inputBindings = (Shiny.inputBindings = new BindingRegistry());
  const outputBindings = (Shiny.outputBindings = new BindingRegistry());

  // "output_binding.js"
  const OutputBinding = (Shiny.OutputBinding = function () {});

  (function () {
    // Returns a jQuery object or element array that contains the
    // descendants of scope that match this binding
    this.find = function (scope) {
      throw "Not implemented";
    };

    this.getId = function (el) {
      return el["data-input-id"] || el.id;
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
      if (err.message === "") {
        // not really error, but we just need to wait (e.g. action buttons)
        $(el).empty();
        return;
      }
      let errClass = "shiny-output-error";

      if (err.type !== null) {
        // use the classes of the error condition as CSS class names
        errClass =
          errClass +
          " " +
          $.map(asArray(err.type), function (type) {
            return errClass + "-" + type;
          }).join(" ");
      }
      $(el).addClass(errClass).text(err.message);
    };
    this.clearError = function (el) {
      $(el).attr("class", function (i, c) {
        return c.replace(/(^|\s)shiny-output-error\S*/g, "");
      });
    };
    this.showProgress = function (el, show) {
      const RECALC_CLASS = "recalculating";

      if (show) $(el).addClass(RECALC_CLASS);
      else $(el).removeClass(RECALC_CLASS);
    };
  }.call(OutputBinding.prototype));

  // "output_binding_text.js"
  const textOutputBinding = new OutputBinding();

  $.extend(textOutputBinding, {
    find: function (scope) {
      return $(scope).find(".shiny-text-output");
    },
    renderValue: function (el, data) {
      $(el).text(data);
    },
  });
  outputBindings.register(textOutputBinding, "shiny.textOutput");

  // "output_binding_image.js"
  const imageOutputBinding = new OutputBinding();

  $.extend(imageOutputBinding, {
    find: function (scope) {
      return $(scope).find(".shiny-image-output, .shiny-plot-output");
    },
    renderValue: function (el, data) {
      // The overall strategy:
      // * Clear out existing image and event handlers.
      // * Create new image.
      // * Create various event handlers.
      // * Bind those event handlers to events.
      // * Insert the new image.

      const outputId = this.getId(el);

      const $el = $(el);
      let img;

      // Get existing img element if present.
      let $img = $el.find("img");

      if ($img.length === 0) {
        // If a img element is not already present, that means this is either
        // the first time renderValue() has been called, or this is after an
        // error.
        img = document.createElement("img");
        $el.append(img);
        $img = $(img);
      } else {
        // Trigger custom 'reset' event for any existing images in the div
        img = $img[0];
        $img.trigger("reset");
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

      const opts = {
        clickId: $el.data("click-id"),
        clickClip: OR(strToBool($el.data("click-clip")), true),

        dblclickId: $el.data("dblclick-id"),
        dblclickClip: OR(strToBool($el.data("dblclick-clip")), true),
        dblclickDelay: OR($el.data("dblclick-delay"), 400),

        hoverId: $el.data("hover-id"),
        hoverClip: OR(strToBool($el.data("hover-clip")), true),
        hoverDelayType: OR($el.data("hover-delay-type"), "debounce"),
        hoverDelay: OR($el.data("hover-delay"), 300),
        hoverNullOutside: OR(strToBool($el.data("hover-null-outside")), false),

        brushId: $el.data("brush-id"),
        brushClip: OR(strToBool($el.data("brush-clip")), true),
        brushDelayType: OR($el.data("brush-delay-type"), "debounce"),
        brushDelay: OR($el.data("brush-delay"), 300),
        brushFill: OR($el.data("brush-fill"), "#666"),
        brushStroke: OR($el.data("brush-stroke"), "#000"),
        brushOpacity: OR($el.data("brush-opacity"), 0.3),
        brushDirection: OR($el.data("brush-direction"), "xy"),
        brushResetOnNew: OR(strToBool($el.data("brush-reset-on-new")), false),

        coordmap: data.coordmap,
      };

      if (opts.brushFill === "auto") {
        opts.brushFill = getComputedLinkColor($el[0]);
      }
      if (opts.brushStroke === "auto") {
        opts.brushStroke = getStyle($el[0], "color");
      }

      // Copy items from data to img. Don't set the coordmap as an attribute.
      $.each(data, function (key, value) {
        if (value === null || key === "coordmap") {
          return;
        }
        // this checks only against base64 encoded src values
        // images put here are only from renderImage and renderPlot
        if (key === "src" && value === img.getAttribute("src")) {
          // Ensure the browser actually fires an onLoad event, which doesn't
          // happen on WebKit if the value we set on src is the same as the
          // value it already has
          // https://github.com/rstudio/shiny/issues/2197
          // https://stackoverflow.com/questions/5024111/javascript-image-onload-doesnt-fire-in-webkit-if-loading-same-image
          img.removeAttribute("src");
        }
        img.setAttribute(key, value);
      });

      // Unset any attributes in the current img that were not provided in the
      // new data.
      for (let i = 0; i < img.attributes.length; i++) {
        const attrib = img.attributes[i];
        // Need to check attrib.specified on IE because img.attributes contains
        // all possible attributes on IE.

        if (attrib.specified && !data.hasOwnProperty(attrib.name)) {
          img.removeAttribute(attrib.name);
        }
      }

      if (!opts.coordmap) {
        opts.coordmap = {
          panels: [],
          dims: {
            // These values be set to the naturalWidth and naturalHeight once the image has loaded
            height: null,
            width: null,
          },
        };
      }

      // Remove event handlers that were added in previous runs of this function.
      $el.off(".image_output");
      $img.off(".image_output");

      // When the image loads, initialize all the interaction handlers. When the
      // value of src is set, the browser may not load the image immediately,
      // even if it's a data URL. If we try to initialize this stuff
      // immediately, it can cause problems because we use we need the raw image
      // height and width
      $img.off("load.shiny_image_interaction");
      $img.one("load.shiny_image_interaction", function () {
        imageutils.initCoordmap($el, opts.coordmap);

        // This object listens for mousedowns, and triggers mousedown2 and dblclick2
        // events as appropriate.
        const clickInfo = imageutils.createClickInfo(
          $el,
          opts.dblclickId,
          opts.dblclickDelay
        );

        $el.on("mousedown.image_output", clickInfo.mousedown);

        if (isIE() && IEVersion() === 8) {
          $el.on("dblclick.image_output", clickInfo.dblclickIE8);
        }

        // ----------------------------------------------------------
        // Register the various event handlers
        // ----------------------------------------------------------
        if (opts.clickId) {
          imageutils.disableDrag($el, $img);

          const clickHandler = imageutils.createClickHandler(
            opts.clickId,
            opts.clickClip,
            opts.coordmap
          );

          $el.on("mousedown2.image_output", clickHandler.mousedown);

          $el.on("resize.image_output", clickHandler.onResize);

          // When img is reset, do housekeeping: clear $el's mouse listener and
          // call the handler's onResetImg callback.
          $img.on("reset.image_output", clickHandler.onResetImg);
        }

        if (opts.dblclickId) {
          imageutils.disableDrag($el, $img);

          // We'll use the clickHandler's mousedown function, but register it to
          // our custom 'dblclick2' event.
          const dblclickHandler = imageutils.createClickHandler(
            opts.dblclickId,
            opts.clickClip,
            opts.coordmap
          );

          $el.on("dblclick2.image_output", dblclickHandler.mousedown);

          $el.on("resize.image_output", dblclickHandler.onResize);
          $img.on("reset.image_output", dblclickHandler.onResetImg);
        }

        if (opts.hoverId) {
          imageutils.disableDrag($el, $img);

          const hoverHandler = imageutils.createHoverHandler(
            opts.hoverId,
            opts.hoverDelay,
            opts.hoverDelayType,
            opts.hoverClip,
            opts.hoverNullOutside,
            opts.coordmap
          );

          $el.on("mousemove.image_output", hoverHandler.mousemove);
          $el.on("mouseout.image_output", hoverHandler.mouseout);

          $el.on("resize.image_output", hoverHandler.onResize);
          $img.on("reset.image_output", hoverHandler.onResetImg);
        }

        if (opts.brushId) {
          imageutils.disableDrag($el, $img);

          const brushHandler = imageutils.createBrushHandler(
            opts.brushId,
            $el,
            opts,
            opts.coordmap,
            outputId
          );

          $el.on("mousedown.image_output", brushHandler.mousedown);
          $el.on("mousemove.image_output", brushHandler.mousemove);

          $el.on("resize.image_output", brushHandler.onResize);
          $img.on("reset.image_output", brushHandler.onResetImg);
        }

        if (opts.clickId || opts.dblclickId || opts.hoverId || opts.brushId) {
          $el.addClass("crosshair");
        }

        if (data.error)
          console.log("Error on server extracting coordmap: " + data.error);
      });
    },

    renderError: function (el, err) {
      $(el).find("img").trigger("reset");
      OutputBinding.prototype.renderError.call(this, el, err);
    },

    clearError: function (el) {
      // Remove all elements except img and the brush; this is usually just
      // error messages.
      $(el)
        .contents()
        .filter(function () {
          return this.tagName !== "IMG" && this.id !== el.id + "_brush";
        })
        .remove();

      OutputBinding.prototype.clearError.call(this, el);
    },

    resize: function (el, width, height) {
      $(el).find("img").trigger("resize");
    },
  });
  outputBindings.register(imageOutputBinding, "shiny.imageOutput");

  const imageutils = {};

  imageutils.disableDrag = function ($el, $img) {
    // Make image non-draggable (Chrome, Safari)
    $img.css("-webkit-user-drag", "none");

    // Firefox, IE<=10
    // First remove existing handler so we don't keep adding handlers.
    $img.off("dragstart.image_output");
    $img.on("dragstart.image_output", function () {
      return false;
    });

    // Disable selection of image and text when dragging in IE<=10
    $el.off("selectstart.image_output");
    $el.on("selectstart.image_output", function () {
      return false;
    });
  };

  // Modifies the panel objects in a coordmap, adding scaleImgToData(),
  // scaleDataToImg(), and clipImg() functions to each one. The panel objects
  // use img and data coordinates only; they do not use css coordinates. The
  // domain is in data coordinates; the range is in img coordinates.
  imageutils.initPanelScales = function (panels) {
    // Map a value x from a domain to a range. If clip is true, clip it to the
    // range.
    function mapLinear(x, domainMin, domainMax, rangeMin, rangeMax, clip) {
      // By default, clip to range
      clip = clip || true;

      const factor = (rangeMax - rangeMin) / (domainMax - domainMin);
      const val = x - domainMin;
      let newval = val * factor + rangeMin;

      if (clip) {
        const max = Math.max(rangeMax, rangeMin);
        const min = Math.min(rangeMax, rangeMin);

        if (newval > max) newval = max;
        else if (newval < min) newval = min;
      }
      return newval;
    }

    // Create scale and inverse-scale functions for a single direction (x or y).
    function scaler1D(domainMin, domainMax, rangeMin, rangeMax, logbase) {
      return {
        scale: function (val, clip) {
          if (logbase) val = Math.log(val) / Math.log(logbase);
          return mapLinear(val, domainMin, domainMax, rangeMin, rangeMax, clip);
        },

        scaleInv: function (val, clip) {
          let res = mapLinear(
            val,
            rangeMin,
            rangeMax,
            domainMin,
            domainMax,
            clip
          );

          if (logbase) res = Math.pow(logbase, res);
          return res;
        },
      };
    }

    // Modify panel, adding scale and inverse-scale functions that take objects
    // like {x:1, y:3}, and also add clip function.
    function addScaleFuns(panel) {
      const d = panel.domain;
      const r = panel.range;
      const xlog = panel.log && panel.log.x ? panel.log.x : null;
      const ylog = panel.log && panel.log.y ? panel.log.y : null;
      const xscaler = scaler1D(d.left, d.right, r.left, r.right, xlog);
      const yscaler = scaler1D(d.bottom, d.top, r.bottom, r.top, ylog);

      // Given an object of form {x:1, y:2}, or {x:1, xmin:2:, ymax: 3}, convert
      // from data coordinates to img. Whether a value is converted as x or y
      // depends on the first character of the key.
      panel.scaleDataToImg = function (val, clip) {
        return mapValues(val, (value, key) => {
          const prefix = key.substring(0, 1);

          if (prefix === "x") {
            return xscaler.scale(value, clip);
          } else if (prefix === "y") {
            return yscaler.scale(value, clip);
          }
          return null;
        });
      };

      panel.scaleImgToData = function (val, clip) {
        return mapValues(val, (value, key) => {
          const prefix = key.substring(0, 1);

          if (prefix === "x") {
            return xscaler.scaleInv(value, clip);
          } else if (prefix === "y") {
            return yscaler.scaleInv(value, clip);
          }
          return null;
        });
      };

      // Given a scaled offset (in img pixels), clip it to the nearest panel region.
      panel.clipImg = function (offset_img) {
        const newOffset = {
          x: offset_img.x,
          y: offset_img.y,
        };

        const bounds = panel.range;

        if (offset_img.x > bounds.right) newOffset.x = bounds.right;
        else if (offset_img.x < bounds.left) newOffset.x = bounds.left;

        if (offset_img.y > bounds.bottom) newOffset.y = bounds.bottom;
        else if (offset_img.y < bounds.top) newOffset.y = bounds.top;

        return newOffset;
      };
    }

    // Add the functions to each panel object.
    for (let i = 0; i < panels.length; i++) {
      const panel = panels[i];

      addScaleFuns(panel);
    }
  };

  // This adds functions to the coordmap object to handle various
  // coordinate-mapping tasks, and send information to the server. The input
  // coordmap is an array of objects, each of which represents a panel. coordmap
  // must be an array, even if empty, so that it can be modified in place; when
  // empty, we add a dummy panel to the array. It also calls initPanelScales,
  // which modifies each panel object to have scaleImgToData, scaleDataToImg,
  // and clip functions.
  //
  // There are three coordinate spaces which we need to translate between:
  //
  // 1. css: The pixel coordinates in the web browser, also known as CSS pixels.
  //    The origin is the upper-left corner of the <img> (not including padding
  //    and border).
  // 2. img: The pixel coordinates of the image data. A common case is on a
  //    HiDPI device, where the source PNG image could be 1000 pixels wide but
  //    be displayed in 500 CSS pixels. Another case is when the image has
  //    additional scaling due to CSS transforms or width.
  // 3. data: The coordinates in the data space. This is a bit more complicated
  //    than the other two, because there can be multiple panels (as in facets).
  imageutils.initCoordmap = function ($el, coordmap) {
    const $img = $el.find("img");
    const img = $img[0];

    // If we didn't get any panels, create a dummy one where the domain and range
    // are simply the pixel dimensions.
    // that we modify.
    if (coordmap.panels.length === 0) {
      const bounds = {
        top: 0,
        left: 0,
        right: img.clientWidth - 1,
        bottom: img.clientHeight - 1,
      };

      coordmap.panels[0] = {
        domain: bounds,
        range: bounds,
        mapping: {},
      };
    }

    // If no dim height and width values are found, set them to the raw image height and width
    // These values should be the same...
    // This is only done to initialize an image output, whose height and width are unknown until the image is retrieved
    coordmap.dims.height = coordmap.dims.height || img.naturalHeight;
    coordmap.dims.width = coordmap.dims.width || img.naturalWidth;

    // Add scaling functions to each panel
    imageutils.initPanelScales(coordmap.panels);

    // This returns the offset of the mouse in CSS pixels relative to the img,
    // but not including the  padding or border, if present.
    coordmap.mouseOffsetCss = function (mouseEvent) {
      const img_origin = findOrigin($img);

      // The offset of the mouse from the upper-left corner of the img, in
      // pixels.
      return {
        x: mouseEvent.pageX - img_origin.x,
        y: mouseEvent.pageY - img_origin.y,
      };
    };

    // Given an offset in an img in CSS pixels, return the corresponding offset
    // in source image pixels. The offset_css can have properties like "x",
    // "xmin", "y", and "ymax" -- anything that starts with "x" and "y". If the
    // img content is 1000 pixels wide, but is scaled to 400 pixels on screen,
    // and the input is x:400, then this will return x:1000.
    coordmap.scaleCssToImg = function (offset_css) {
      const pixel_scaling = coordmap.imgToCssScalingRatio();

      const result = mapValues(offset_css, (value, key) => {
        const prefix = key.substring(0, 1);

        if (prefix === "x") {
          return offset_css[key] / pixel_scaling.x;
        } else if (prefix === "y") {
          return offset_css[key] / pixel_scaling.y;
        }
        return null;
      });

      return result;
    };

    // Given an offset in an img, in source image pixels, return the
    // corresponding offset in CSS pixels. If the img content is 1000 pixels
    // wide, but is scaled to 400 pixels on screen, and the input is x:1000,
    // then this will return x:400.
    coordmap.scaleImgToCss = function (offset_img) {
      const pixel_scaling = coordmap.imgToCssScalingRatio();

      const result = mapValues(offset_img, (value, key) => {
        const prefix = key.substring(0, 1);

        if (prefix === "x") {
          return offset_img[key] * pixel_scaling.x;
        } else if (prefix === "y") {
          return offset_img[key] * pixel_scaling.y;
        }
        return null;
      });

      return result;
    };

    // Returns the x and y ratio the image content is scaled to on screen. If
    // the image data is 1000 pixels wide and is scaled to 300 pixels on screen,
    // then this returns 0.3. (Note the 300 pixels refers to CSS pixels.)
    coordmap.imgToCssScalingRatio = function () {
      const img_dims = findDims($img);

      return {
        x: img_dims.x / coordmap.dims.width,
        y: img_dims.y / coordmap.dims.height,
      };
    };

    coordmap.cssToImgScalingRatio = function () {
      const res = coordmap.imgToCssScalingRatio();

      return {
        x: 1 / res.x,
        y: 1 / res.y,
      };
    };

    // Given an offset in css pixels, return an object representing which panel
    // it's in. The `expand` argument tells it to expand the panel area by that
    // many pixels. It's possible for an offset to be within more than one
    // panel, because of the `expand` value. If that's the case, find the
    // nearest panel.
    coordmap.getPanelCss = function (offset_css, expand = 0) {
      const offset_img = coordmap.scaleCssToImg(offset_css);
      const x = offset_img.x;
      const y = offset_img.y;

      // Convert expand from css pixels to img pixels
      const cssToImgRatio = coordmap.cssToImgScalingRatio();
      const expand_img = {
        x: expand * cssToImgRatio.x,
        y: expand * cssToImgRatio.y,
      };

      const matches = []; // Panels that match
      const dists = []; // Distance of offset to each matching panel
      let b;
      let i;

      for (i = 0; i < coordmap.panels.length; i++) {
        b = coordmap.panels[i].range;

        if (
          x <= b.right + expand_img.x &&
          x >= b.left - expand_img.x &&
          y <= b.bottom + expand_img.y &&
          y >= b.top - expand_img.y
        ) {
          matches.push(coordmap.panels[i]);

          // Find distance from edges for x and y
          let xdist = 0;
          let ydist = 0;

          if (x > b.right && x <= b.right + expand_img.x) {
            xdist = x - b.right;
          } else if (x < b.left && x >= b.left - expand_img.x) {
            xdist = x - b.left;
          }
          if (y > b.bottom && y <= b.bottom + expand_img.y) {
            ydist = y - b.bottom;
          } else if (y < b.top && y >= b.top - expand_img.y) {
            ydist = y - b.top;
          }

          // Cartesian distance
          dists.push(Math.sqrt(Math.pow(xdist, 2) + Math.pow(ydist, 2)));
        }
      }

      if (matches.length) {
        // Find shortest distance
        const min_dist = Math.min.apply(null, dists);

        for (i = 0; i < matches.length; i++) {
          if (dists[i] === min_dist) {
            return matches[i];
          }
        }
      }

      return null;
    };

    // Is an offset (in css pixels) in a panel? If supplied, `expand` tells us
    // to expand the panels by that many pixels in all directions.
    coordmap.isInPanelCss = function (offset_css, expand = 0) {
      if (coordmap.getPanelCss(offset_css, expand)) return true;

      return false;
    };

    // Returns a function that sends mouse coordinates, scaled to data space.
    // If that function is passed a null event, it will send null.
    coordmap.mouseCoordinateSender = function (inputId, clip, nullOutside) {
      if (clip === undefined) clip = true;
      if (nullOutside === undefined) nullOutside = false;

      return function (e) {
        if (e === null) {
          Shiny.setInputValue(inputId, null);
          return;
        }
        const coords = {};
        const coords_css = coordmap.mouseOffsetCss(e);
        // If outside of plotting region

        if (!coordmap.isInPanelCss(coords_css)) {
          if (nullOutside) {
            Shiny.setInputValue(inputId, null);
            return;
          }
          if (clip) return;

          coords.coords_css = coords_css;
          coords.coords_img = coordmap.scaleCssToImg(coords_css);

          Shiny.setInputValue(inputId, coords, { priority: "event" });
          return;
        }
        const panel = coordmap.getPanelCss(coords_css);

        const coords_img = coordmap.scaleCssToImg(coords_css);
        const coords_data = panel.scaleImgToData(coords_img);

        coords.x = coords_data.x;
        coords.y = coords_data.y;
        coords.coords_css = coords_css;
        coords.coords_img = coords_img;

        coords.img_css_ratio = coordmap.cssToImgScalingRatio();

        // Add the panel (facet) variables, if present
        $.extend(coords, panel.panel_vars);

        // Add variable name mappings
        coords.mapping = panel.mapping;

        // Add scaling information
        coords.domain = panel.domain;
        coords.range = panel.range;
        coords.log = panel.log;

        Shiny.setInputValue(inputId, coords, { priority: "event" });
      };
    };
  };

  // Given two sets of x/y coordinates, return an object representing the min
  // and max x and y values. (This could be generalized to any number of
  // points).
  imageutils.findBox = function (offset1, offset2) {
    return {
      xmin: Math.min(offset1.x, offset2.x),
      xmax: Math.max(offset1.x, offset2.x),
      ymin: Math.min(offset1.y, offset2.y),
      ymax: Math.max(offset1.y, offset2.y),
    };
  };

  // Shift an array of values so that they are within a min and max. The vals
  // will be shifted so that they maintain the same spacing internally. If the
  // range in vals is larger than the range of min and max, the result might not
  // make sense.
  imageutils.shiftToRange = function (vals, min, max) {
    if (!(vals instanceof Array)) vals = [vals];

    const maxval = Math.max.apply(null, vals);
    const minval = Math.min.apply(null, vals);
    let shiftAmount = 0;

    if (maxval > max) {
      shiftAmount = max - maxval;
    } else if (minval < min) {
      shiftAmount = min - minval;
    }

    const newvals = [];

    for (let i = 0; i < vals.length; i++) {
      newvals[i] = vals[i] + shiftAmount;
    }
    return newvals;
  };

  // This object provides two public event listeners: mousedown, and
  // dblclickIE8.
  // We need to make sure that, when the image is listening for double-
  // clicks, that a double-click doesn't trigger two click events. We'll
  // trigger custom mousedown2 and dblclick2 events with this mousedown
  // listener.
  imageutils.createClickInfo = function ($el, dblclickId, dblclickDelay) {
    let clickTimer = null;
    let pending_e = null; // A pending mousedown2 event

    // Create a new event of type eventType (like 'mousedown2'), and trigger
    // it with the information stored in this.e.
    function triggerEvent(newEventType, e) {
      // Extract important info from e and construct a new event with type
      // eventType.
      const e2 = $.Event(newEventType, {
        which: e.which,
        pageX: e.pageX,
        pageY: e.pageY,
      });

      $el.trigger(e2);
    }

    function triggerPendingMousedown2() {
      // It's possible that between the scheduling of a mousedown2 and the
      // time this callback is executed, someone else triggers a
      // mousedown2, so check for that.
      if (pending_e) {
        triggerEvent("mousedown2", pending_e);
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
        triggerEvent("mousedown2", e);
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
        if (
          (pending_e && Math.abs(pending_e.pageX - e.pageX) > 2) ||
          Math.abs(pending_e.pageY - e.pageY) > 2
        ) {
          triggerPendingMousedown2();
          scheduleMousedown2(e);
        } else {
          // The second click was close to the first one. If it happened
          // within specified delay, trigger our custom 'dblclick2' event.
          pending_e = null;
          triggerEvent("dblclick2", e);
        }
      }
    }

    // IE8 needs a special hack because when you do a double-click it doesn't
    // trigger the click event twice - it directly triggers dblclick.
    function dblclickIE8(e) {
      e.which = 1; // In IE8, e.which is 0 instead of 1. ???
      triggerEvent("dblclick2", e);
    }

    return {
      mousedown: mousedown,
      dblclickIE8: dblclickIE8,
    };
  };

  // ----------------------------------------------------------
  // Handler creators for click, hover, brush.
  // Each of these returns an object with a few public members. These public
  // members are callbacks that are meant to be bound to events on $el with
  // the same name (like 'mousedown').
  // ----------------------------------------------------------

  imageutils.createClickHandler = function (inputId, clip, coordmap) {
    const clickInfoSender = coordmap.mouseCoordinateSender(inputId, clip);

    return {
      mousedown: function (e) {
        // Listen for left mouse button only
        if (e.which !== 1) return;
        clickInfoSender(e);
      },
      onResetImg: function () {
        clickInfoSender(null);
      },
      onResize: null,
    };
  };

  imageutils.createHoverHandler = function (
    inputId,
    delay,
    delayType,
    clip,
    nullOutside,
    coordmap
  ) {
    const sendHoverInfo = coordmap.mouseCoordinateSender(
      inputId,
      clip,
      nullOutside
    );

    let hoverInfoSender;

    if (delayType === "throttle")
      hoverInfoSender = new Throttler(null, sendHoverInfo, delay);
    else hoverInfoSender = new Debouncer(null, sendHoverInfo, delay);

    // What to do when mouse exits the image
    let mouseout;

    if (nullOutside)
      mouseout = function () {
        hoverInfoSender.normalCall(null);
      };
    else mouseout = function () {};

    return {
      mousemove: function (e) {
        hoverInfoSender.normalCall(e);
      },
      mouseout: mouseout,
      onResetImg: function () {
        hoverInfoSender.immediateCall(null);
      },
      onResize: null,
    };
  };

  // Returns a brush handler object. This has three public functions:
  // mousedown, mousemove, and onResetImg.
  imageutils.createBrushHandler = function (
    inputId,
    $el,
    opts,
    coordmap,
    outputId
  ) {
    // Parameter: expand the area in which a brush can be started, by this
    // many pixels in all directions. (This should probably be a brush option)
    const expandPixels = 20;

    // Represents the state of the brush
    const brush = imageutils.createBrush($el, opts, coordmap, expandPixels);

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
      $el.removeClass(
        "crosshair grabbable grabbing ns-resize ew-resize nesw-resize nwse-resize"
      );

      if (style) $el.addClass(style);
    }

    function sendBrushInfo() {
      const coords = brush.boundsData();

      // We're in a new or reset state
      if (isNaN(coords.xmin)) {
        Shiny.setInputValue(inputId, null);
        // Must tell other brushes to clear.
        imageOutputBinding.find(document).trigger("shiny-internal:brushed", {
          brushId: inputId,
          outputId: null,
        });
        return;
      }

      const panel = brush.getPanel();

      // Add the panel (facet) variables, if present
      $.extend(coords, panel.panel_vars);

      coords.coords_css = brush.boundsCss();
      coords.coords_img = coordmap.scaleCssToImg(coords.coords_css);

      coords.img_css_ratio = coordmap.cssToImgScalingRatio();

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
      Shiny.setInputValue(inputId, coords);

      $el.data("mostRecentBrush", true);
      imageOutputBinding
        .find(document)
        .trigger("shiny-internal:brushed", coords);
    }

    let brushInfoSender;

    if (opts.brushDelayType === "throttle") {
      brushInfoSender = new Throttler(null, sendBrushInfo, opts.brushDelay);
    } else {
      brushInfoSender = new Debouncer(null, sendBrushInfo, opts.brushDelay);
    }

    function mousedown(e) {
      // This can happen when mousedown inside the graphic, then mouseup
      // outside, then mousedown inside. Just ignore the second
      // mousedown.
      if (brush.isBrushing() || brush.isDragging() || brush.isResizing())
        return;

      // Listen for left mouse button only
      if (e.which !== 1) return;

      // In general, brush uses css pixels, and coordmap uses img pixels.
      const offset_css = coordmap.mouseOffsetCss(e);

      // Ignore mousedown events outside of plotting region, expanded by
      // a number of pixels specified in expandPixels.
      if (opts.brushClip && !coordmap.isInPanelCss(offset_css, expandPixels))
        return;

      brush.up({ x: NaN, y: NaN });
      brush.down(offset_css);

      if (brush.isInResizeArea(offset_css)) {
        brush.startResizing(offset_css);

        // Attach the move and up handlers to the window so that they respond
        // even when the mouse is moved outside of the image.
        $(document)
          .on("mousemove.image_brush", mousemoveResizing)
          .on("mouseup.image_brush", mouseupResizing);
      } else if (brush.isInsideBrush(offset_css)) {
        brush.startDragging(offset_css);
        setCursorStyle("grabbing");

        // Attach the move and up handlers to the window so that they respond
        // even when the mouse is moved outside of the image.
        $(document)
          .on("mousemove.image_brush", mousemoveDragging)
          .on("mouseup.image_brush", mouseupDragging);
      } else {
        const panel = coordmap.getPanelCss(offset_css, expandPixels);

        brush.startBrushing(panel.clipImg(coordmap.scaleCssToImg(offset_css)));

        // Attach the move and up handlers to the window so that they respond
        // even when the mouse is moved outside of the image.
        $(document)
          .on("mousemove.image_brush", mousemoveBrushing)
          .on("mouseup.image_brush", mouseupBrushing);
      }
    }

    // This sets the cursor style when it's in the el
    function mousemove(e) {
      // In general, brush uses css pixels, and coordmap uses img pixels.
      const offset_css = coordmap.mouseOffsetCss(e);

      if (!(brush.isBrushing() || brush.isDragging() || brush.isResizing())) {
        // Set the cursor depending on where it is
        if (brush.isInResizeArea(offset_css)) {
          const r = brush.whichResizeSides(offset_css);

          if ((r.left && r.top) || (r.right && r.bottom)) {
            setCursorStyle("nwse-resize");
          } else if ((r.left && r.bottom) || (r.right && r.top)) {
            setCursorStyle("nesw-resize");
          } else if (r.left || r.right) {
            setCursorStyle("ew-resize");
          } else if (r.top || r.bottom) {
            setCursorStyle("ns-resize");
          }
        } else if (brush.isInsideBrush(offset_css)) {
          setCursorStyle("grabbable");
        } else if (coordmap.isInPanelCss(offset_css, expandPixels)) {
          setCursorStyle("crosshair");
        } else {
          setCursorStyle(null);
        }
      }
    }

    // mousemove handlers while brushing or dragging
    function mousemoveBrushing(e) {
      brush.brushTo(coordmap.mouseOffsetCss(e));
      brushInfoSender.normalCall();
    }

    function mousemoveDragging(e) {
      brush.dragTo(coordmap.mouseOffsetCss(e));
      brushInfoSender.normalCall();
    }

    function mousemoveResizing(e) {
      brush.resizeTo(coordmap.mouseOffsetCss(e));
      brushInfoSender.normalCall();
    }

    // mouseup handlers while brushing or dragging
    function mouseupBrushing(e) {
      // Listen for left mouse button only
      if (e.which !== 1) return;

      $(document).off("mousemove.image_brush").off("mouseup.image_brush");

      brush.up(coordmap.mouseOffsetCss(e));

      brush.stopBrushing();
      setCursorStyle("crosshair");

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

      $(document).off("mousemove.image_brush").off("mouseup.image_brush");

      brush.up(coordmap.mouseOffsetCss(e));

      brush.stopDragging();
      setCursorStyle("grabbable");

      if (brushInfoSender.isPending()) brushInfoSender.immediateCall();
    }

    function mouseupResizing(e) {
      // Listen for left mouse button only
      if (e.which !== 1) return;

      $(document).off("mousemove.image_brush").off("mouseup.image_brush");

      brush.up(coordmap.mouseOffsetCss(e));
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
        // Importing an old brush must happen after the image data has loaded
        // and the <img> DOM element has the updated size. If importOldBrush()
        // is called before this happens, then the css-img coordinate mappings
        // will give the wrong result, and the brush will have the wrong
        // position.
        //
        // jcheng 09/26/2018: This used to happen in img.onLoad, but recently
        // we moved to all brush initialization moving to img.onLoad so this
        // logic can be executed inline.
        brush.importOldBrush();
        brushInfoSender.immediateCall();
      }
    }

    function onResize() {
      brush.onResize();
      brushInfoSender.immediateCall();
    }

    return {
      mousedown: mousedown,
      mousemove: mousemove,
      onResetImg: onResetImg,
      onResize: onResize,
    };
  };

  // Returns an object that represents the state of the brush. This gets wrapped
  // in a brushHandler, which provides various event listeners.
  imageutils.createBrush = function ($el, opts, coordmap, expandPixels) {
    // Number of pixels outside of brush to allow start resizing
    const resizeExpand = 10;

    const el = $el[0];
    let $div = null; // The div representing the brush

    const state = {};

    // Aliases for conciseness
    const cssToImg = coordmap.scaleCssToImg;
    const imgToCss = coordmap.scaleImgToCss;

    reset();

    function reset() {
      // Current brushing/dragging/resizing state
      state.brushing = false;
      state.dragging = false;
      state.resizing = false;

      // Offset of last mouse down and up events (in CSS pixels)
      state.down = { x: NaN, y: NaN };
      state.up = { x: NaN, y: NaN };

      // Which side(s) we're currently resizing
      state.resizeSides = {
        left: false,
        right: false,
        top: false,
        bottom: false,
      };

      // Bounding rectangle of the brush, in CSS pixel and data dimensions. We
      // need to record data dimensions along with pixel dimensions so that when
      // a new plot is sent, we can re-draw the brush div with the appropriate
      // coords.
      state.boundsCss = {
        xmin: NaN,
        xmax: NaN,
        ymin: NaN,
        ymax: NaN,
      };
      state.boundsData = {
        xmin: NaN,
        xmax: NaN,
        ymin: NaN,
        ymax: NaN,
      };

      // Panel object that the brush is in
      state.panel = null;

      // The bounds at the start of a drag/resize (in CSS pixels)
      state.changeStartBounds = {
        xmin: NaN,
        xmax: NaN,
        ymin: NaN,
        ymax: NaN,
      };

      if ($div) $div.remove();
    }

    // If there's an existing brush div, use that div to set the new brush's
    // settings, provided that the x, y, and panel variables have the same names,
    // and there's a panel with matching panel variable values.
    function importOldBrush() {
      const oldDiv = $el.find("#" + el.id + "_brush");

      if (oldDiv.length === 0) return;

      const oldBoundsData = oldDiv.data("bounds-data");
      const oldPanel = oldDiv.data("panel");

      if (!oldBoundsData || !oldPanel) return;

      // Find a panel that has matching vars; if none found, we can't restore.
      // The oldPanel and new panel must match on their mapping vars, and the
      // values.
      for (let i = 0; i < coordmap.panels.length; i++) {
        const curPanel = coordmap.panels[i];

        if (
          equal(oldPanel.mapping, curPanel.mapping) &&
          equal(oldPanel.panel_vars, curPanel.panel_vars)
        ) {
          // We've found a matching panel
          state.panel = coordmap.panels[i];
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

    // This will reposition the brush div when the image is resized, maintaining
    // the same data coordinates. Note that the "resize" here refers to the
    // wrapper div/img being resized; elsewhere, "resize" refers to the brush
    // div being resized.
    function onResize() {
      const bounds_data = boundsData();
      // Check to see if we have valid boundsData

      for (const val in bounds_data) {
        if (isnan(bounds_data[val])) return;
      }

      boundsData(bounds_data);
      updateDiv();
    }

    // Return true if the offset is inside min/max coords
    function isInsideBrush(offset_css) {
      const bounds = state.boundsCss;

      return (
        offset_css.x <= bounds.xmax &&
        offset_css.x >= bounds.xmin &&
        offset_css.y <= bounds.ymax &&
        offset_css.y >= bounds.ymin
      );
    }

    // Return true if offset is inside a region to start a resize
    function isInResizeArea(offset_css) {
      const sides = whichResizeSides(offset_css);

      return sides.left || sides.right || sides.top || sides.bottom;
    }

    // Return an object representing which resize region(s) the cursor is in.
    function whichResizeSides(offset_css) {
      const b = state.boundsCss;
      // Bounds with expansion
      const e = {
        xmin: b.xmin - resizeExpand,
        xmax: b.xmax + resizeExpand,
        ymin: b.ymin - resizeExpand,
        ymax: b.ymax + resizeExpand,
      };
      const res = {
        left: false,
        right: false,
        top: false,
        bottom: false,
      };

      if (
        (opts.brushDirection === "xy" || opts.brushDirection === "x") &&
        offset_css.y <= e.ymax &&
        offset_css.y >= e.ymin
      ) {
        if (offset_css.x < b.xmin && offset_css.x >= e.xmin) res.left = true;
        else if (offset_css.x > b.xmax && offset_css.x <= e.xmax)
          res.right = true;
      }

      if (
        (opts.brushDirection === "xy" || opts.brushDirection === "y") &&
        offset_css.x <= e.xmax &&
        offset_css.x >= e.xmin
      ) {
        if (offset_css.y < b.ymin && offset_css.y >= e.ymin) res.top = true;
        else if (offset_css.y > b.ymax && offset_css.y <= e.ymax)
          res.bottom = true;
      }

      return res;
    }

    // Sets the bounds of the brush (in CSS pixels), given a box and optional
    // panel. This will fit the box bounds into the panel, so we don't brush
    // outside of it. This knows whether we're brushing in the x, y, or xy
    // directions, and sets bounds accordingly. If no box is passed in, just
    // return current bounds.
    function boundsCss(box_css) {
      if (box_css === undefined) {
        return $.extend({}, state.boundsCss);
      }

      let min_css = { x: box_css.xmin, y: box_css.ymin };
      let max_css = { x: box_css.xmax, y: box_css.ymax };

      const panel = state.panel;
      const panelBounds_img = panel.range;

      if (opts.brushClip) {
        min_css = imgToCss(panel.clipImg(cssToImg(min_css)));
        max_css = imgToCss(panel.clipImg(cssToImg(max_css)));
      }

      if (opts.brushDirection === "xy") {
        // No change
      } else if (opts.brushDirection === "x") {
        // Extend top and bottom of plotting area
        min_css.y = imgToCss({ y: panelBounds_img.top }).y;
        max_css.y = imgToCss({ y: panelBounds_img.bottom }).y;
      } else if (opts.brushDirection === "y") {
        min_css.x = imgToCss({ x: panelBounds_img.left }).x;
        max_css.x = imgToCss({ x: panelBounds_img.right }).x;
      }

      state.boundsCss = {
        xmin: min_css.x,
        xmax: max_css.x,
        ymin: min_css.y,
        ymax: max_css.y,
      };

      // Positions in data space
      const min_data = state.panel.scaleImgToData(cssToImg(min_css));
      const max_data = state.panel.scaleImgToData(cssToImg(max_css));
      // For reversed scales, the min and max can be reversed, so use findBox
      // to ensure correct order.

      state.boundsData = imageutils.findBox(min_data, max_data);
      // Round to 14 significant digits to avoid spurious changes in FP values
      // (#1634).
      state.boundsData = mapValues(state.boundsData, (val) =>
        roundSignif(val, 14)
      );

      // We also need to attach the data bounds and panel as data attributes, so
      // that if the image is re-sent, we can grab the data bounds to create a new
      // brush. This should be fast because it doesn't actually modify the DOM.
      $div.data("bounds-data", state.boundsData);
      $div.data("panel", state.panel);
      return undefined;
    }

    // Get or set the bounds of the brush using coordinates in the data space.
    function boundsData(box_data) {
      if (box_data === undefined) {
        return $.extend({}, state.boundsData);
      }

      let box_css = imgToCss(state.panel.scaleDataToImg(box_data));
      // Round to 13 significant digits to avoid spurious changes in FP values
      // (#2197).

      box_css = mapValues(box_css, (val) => roundSignif(val, 13));

      // The scaling function can reverse the direction of the axes, so we need to
      // find the min and max again.
      boundsCss({
        xmin: Math.min(box_css.xmin, box_css.xmax),
        xmax: Math.max(box_css.xmin, box_css.xmax),
        ymin: Math.min(box_css.ymin, box_css.ymax),
        ymax: Math.max(box_css.ymin, box_css.ymax),
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
      $div = $(document.createElement("div"))
        .attr("id", el.id + "_brush")
        .css({
          "background-color": opts.brushFill,
          opacity: opts.brushOpacity,
          "pointer-events": "none",
          position: "absolute",
        })
        .hide();

      const borderStyle = "1px solid " + opts.brushStroke;

      if (opts.brushDirection === "xy") {
        $div.css({
          border: borderStyle,
        });
      } else if (opts.brushDirection === "x") {
        $div.css({
          "border-left": borderStyle,
          "border-right": borderStyle,
        });
      } else if (opts.brushDirection === "y") {
        $div.css({
          "border-top": borderStyle,
          "border-bottom": borderStyle,
        });
      }

      $el.append($div);
      $div.offset({ x: 0, y: 0 }).width(0).outerHeight(0);
    }

    // Update the brush div to reflect the current brush bounds.
    function updateDiv() {
      // Need parent offset relative to page to calculate mouse offset
      // relative to page.
      const img_offset_css = findOrigin($el.find("img"));
      const b = state.boundsCss;

      $div
        .offset({
          top: img_offset_css.y + b.ymin,
          left: img_offset_css.x + b.xmin,
        })
        .outerWidth(b.xmax - b.xmin + 1)
        .outerHeight(b.ymax - b.ymin + 1);
    }

    function down(offset_css) {
      if (offset_css === undefined) return state.down;

      state.down = offset_css;
      return undefined;
    }

    function up(offset_css) {
      if (offset_css === undefined) return state.up;

      state.up = offset_css;
      return undefined;
    }

    function isBrushing() {
      return state.brushing;
    }

    function startBrushing() {
      state.brushing = true;
      addDiv();
      state.panel = coordmap.getPanelCss(state.down, expandPixels);

      boundsCss(imageutils.findBox(state.down, state.down));
      updateDiv();
    }

    function brushTo(offset_css) {
      boundsCss(imageutils.findBox(state.down, offset_css));
      $div.show();
      updateDiv();
    }

    function stopBrushing() {
      state.brushing = false;
      // Save the final bounding box of the brush
      boundsCss(imageutils.findBox(state.down, state.up));
    }

    function isDragging() {
      return state.dragging;
    }

    function startDragging() {
      state.dragging = true;
      state.changeStartBounds = $.extend({}, state.boundsCss);
    }

    function dragTo(offset_css) {
      // How far the brush was dragged
      const dx = offset_css.x - state.down.x;
      const dy = offset_css.y - state.down.y;

      // Calculate what new positions would be, before clipping.
      const start = state.changeStartBounds;
      let newBounds_css = {
        xmin: start.xmin + dx,
        xmax: start.xmax + dx,
        ymin: start.ymin + dy,
        ymax: start.ymax + dy,
      };

      // Clip to the plotting area
      if (opts.brushClip) {
        const panelBounds_img = state.panel.range;
        const newBounds_img = cssToImg(newBounds_css);

        // Convert to format for shiftToRange
        let xvals_img = [newBounds_img.xmin, newBounds_img.xmax];
        let yvals_img = [newBounds_img.ymin, newBounds_img.ymax];

        xvals_img = imageutils.shiftToRange(
          xvals_img,
          panelBounds_img.left,
          panelBounds_img.right
        );
        yvals_img = imageutils.shiftToRange(
          yvals_img,
          panelBounds_img.top,
          panelBounds_img.bottom
        );

        // Convert back to bounds format
        newBounds_css = imgToCss({
          xmin: xvals_img[0],
          xmax: xvals_img[1],
          ymin: yvals_img[0],
          ymax: yvals_img[1],
        });
      }

      boundsCss(newBounds_css);
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
      state.changeStartBounds = $.extend({}, state.boundsCss);
      state.resizeSides = whichResizeSides(state.down);
    }

    function resizeTo(offset_css) {
      // How far the brush was dragged
      const d_css = {
        x: offset_css.x - state.down.x,
        y: offset_css.y - state.down.y,
      };

      const d_img = cssToImg(d_css);

      // Calculate what new positions would be, before clipping.
      const b_img = cssToImg(state.changeStartBounds);
      const panelBounds_img = state.panel.range;

      if (state.resizeSides.left) {
        const xmin_img = imageutils.shiftToRange(
          b_img.xmin + d_img.x,
          panelBounds_img.left,
          b_img.xmax
        )[0];

        b_img.xmin = xmin_img;
      } else if (state.resizeSides.right) {
        const xmax_img = imageutils.shiftToRange(
          b_img.xmax + d_img.x,
          b_img.xmin,
          panelBounds_img.right
        )[0];

        b_img.xmax = xmax_img;
      }

      if (state.resizeSides.top) {
        const ymin_img = imageutils.shiftToRange(
          b_img.ymin + d_img.y,
          panelBounds_img.top,
          b_img.ymax
        )[0];

        b_img.ymin = ymin_img;
      } else if (state.resizeSides.bottom) {
        const ymax_img = imageutils.shiftToRange(
          b_img.ymax + d_img.y,
          b_img.ymin,
          panelBounds_img.bottom
        )[0];

        b_img.ymax = ymax_img;
      }

      boundsCss(imgToCss(b_img));
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

      onResize: onResize, // A callback when the wrapper div or img is resized.

      boundsCss: boundsCss,
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
      stopResizing: stopResizing,
    };
  };

  Shiny.resetBrush = function (brushId) {
    Shiny.setInputValue(brushId, null);
    imageOutputBinding.find(document).trigger("shiny-internal:brushed", {
      brushId: brushId,
      outputId: null,
    });
  };

  // -----------------------------------------------------------------------
  // Utility functions for finding dimensions and locations of DOM elements
  // -----------------------------------------------------------------------

  // Returns the ratio that an element has been scaled (for example, by CSS
  // transforms) in the x and y directions.
  function findScalingRatio($el) {
    const boundingRect = $el[0].getBoundingClientRect();

    return {
      x: boundingRect.width / $el.outerWidth(),
      y: boundingRect.height / $el.outerHeight(),
    };
  }

  function findOrigin($el) {
    const offset = $el.offset();
    const scaling_ratio = findScalingRatio($el);

    // Find the size of the padding and border, for the top and left. This is
    // before any transforms.
    const paddingBorder = {
      left:
        parseInt($el.css("border-left-width")) +
        parseInt($el.css("padding-left")),
      top:
        parseInt($el.css("border-top-width")) +
        parseInt($el.css("padding-top")),
    };

    // offset() returns the upper left corner of the element relative to the
    // page, but it includes padding and border. Here we find the upper left
    // of the element, not including padding and border.
    return {
      x: offset.left + scaling_ratio.x * paddingBorder.left,
      y: offset.top + scaling_ratio.y * paddingBorder.top,
    };
  }

  // Find the dimensions of a tag, after transforms, and without padding and
  // border.
  function findDims($el) {
    // If there's any padding/border, we need to find the ratio of the actual
    // element content compared to the element plus padding and border.
    const content_ratio = {
      x: $el.width() / $el.outerWidth(),
      y: $el.height() / $el.outerHeight(),
    };

    // Get the dimensions of the element _after_ any CSS transforms. This
    // includes the padding and border.
    const bounding_rect = $el[0].getBoundingClientRect();

    // Dimensions of the element after any CSS transforms, and without
    // padding/border.
    return {
      x: content_ratio.x * bounding_rect.width,
      y: content_ratio.y * bounding_rect.height,
    };
  }

  // "output_binding_html.js"
  const htmlOutputBinding = new OutputBinding();

  $.extend(htmlOutputBinding, {
    find: function (scope) {
      return $(scope).find(".shiny-html-output");
    },
    onValueError: function (el, err) {
      Shiny.unbindAll(el);
      this.renderError(el, err);
    },
    renderValue: function (el, data) {
      Shiny.renderContent(el, data);
    },
  });
  outputBindings.register(htmlOutputBinding, "shiny.htmlOutput");

  const renderDependencies = (Shiny.renderDependencies = function (
    dependencies
  ) {
    if (dependencies) {
      $.each(dependencies, function (i, dep) {
        renderDependency(dep);
      });
    }
  });

  // Render HTML in a DOM element, add dependencies, and bind Shiny
  // inputs/outputs. `content` can be null, a string, or an object with
  // properties 'html' and 'deps'.
  Shiny.renderContent = function (el, content, where = "replace") {
    if (where === "replace") {
      Shiny.unbindAll(el);
    }

    let html;
    let dependencies = [];

    if (content === null) {
      html = "";
    } else if (typeof content === "string") {
      html = content;
    } else if (typeof content === "object") {
      html = content.html;
      dependencies = content.deps || [];
    }

    Shiny.renderHtml(html, el, dependencies, where);

    let scope = el;

    if (where === "replace") {
      Shiny.initializeInputs(el);
      Shiny.bindAll(el);
    } else {
      const $parent = $(el).parent();

      if ($parent.length > 0) {
        scope = $parent;
        if (where === "beforeBegin" || where === "afterEnd") {
          const $grandparent = $parent.parent();

          if ($grandparent.length > 0) scope = $grandparent;
        }
      }
      Shiny.initializeInputs(scope);
      Shiny.bindAll(scope);
    }
  };

  // Render HTML in a DOM element, inserting singletons into head as needed
  Shiny.renderHtml = function (html, el, dependencies, where = "replace") {
    renderDependencies(dependencies);
    return singletons.renderHtml(html, el, where);
  };

  const htmlDependencies = {};

  function registerDependency(name, version) {
    htmlDependencies[name] = version;
  }

  // Re-render stylesheet(s) if the dependency has specificially requested it
  // and it matches an existing dependency (name and version)
  function needsRestyle(dep) {
    if (!dep.restyle) {
      return false;
    }
    const names = Object.keys(htmlDependencies);
    const idx = names.indexOf(dep.name);

    if (idx === -1) {
      return false;
    }
    return htmlDependencies[names[idx]] === dep.version;
  }

  // Client-side dependency resolution and rendering
  function renderDependency(dep) {
    const restyle = needsRestyle(dep);

    if (htmlDependencies.hasOwnProperty(dep.name) && !restyle) return false;

    registerDependency(dep.name, dep.version);

    const href = dep.src.href;

    const $head = $("head").first();

    if (dep.meta && !restyle) {
      const metas = $.map(asArray(dep.meta), function (obj, idx) {
        // only one named pair is expected in obj as it's already been decomposed
        const name = Object.keys(obj)[0];

        return $("<meta>").attr("name", name).attr("content", obj[name]);
      });

      $head.append(metas);
    }

    if (dep.stylesheet) {
      const links = $.map(asArray(dep.stylesheet), function (stylesheet) {
        return $("<link rel='stylesheet' type='text/css'>").attr(
          "href",
          href + "/" + encodeURI(stylesheet)
        );
      });

      if (!restyle) {
        $head.append(links);
      } else {
        // This inline <style> based approach works for IE11
        let refreshStyle = function (href, oldSheet) {
          const xhr = new XMLHttpRequest();

          xhr.open("GET", href);
          xhr.onload = function () {
            const id =
              "shiny_restyle_" + href.split("?restyle")[0].replace(/\W/g, "_");
            const oldStyle = $head.find("style#" + id);
            const newStyle = $("<style>").attr("id", id).html(xhr.responseText);

            $head.append(newStyle);
            setTimeout(() => oldStyle.remove(), 500);
            setTimeout(() => removeSheet(oldSheet), 500);
          };
          xhr.send();
        };

        let findSheet = function (href) {
          for (let i = 0; i < document.styleSheets.length; i++) {
            const sheet = document.styleSheets[i];
            // The sheet's href is a full URL

            if (
              typeof sheet.href === "string" &&
              sheet.href.indexOf(href) > -1
            ) {
              return sheet;
            }
          }
          return null;
        };

        let removeSheet = function (sheet) {
          if (!sheet) return;
          sheet.disabled = true;
          if (isIE()) sheet.cssText = "";
          $(sheet.ownerNode).remove();
        };

        $.map(links, function (link) {
          // Find any document.styleSheets that match this link's href
          // so we can remove it after bringing in the new stylesheet
          const oldSheet = findSheet(link.attr("href"));
          // Add a timestamp to the href to prevent caching
          const href = link.attr("href") + "?restyle=" + new Date().getTime();
          // Use inline <style> approach for IE, otherwise use the more elegant
          // <link> -based approach

          if (isIE()) {
            refreshStyle(href, oldSheet);
          } else {
            link.attr("href", href);
            // Once the new <link> is loaded, schedule the old <link> to be removed
            // on the next tick which is needed to avoid FOUC
            link.attr("onload", () => {
              setTimeout(() => removeSheet(oldSheet), 500);
            });
            $head.append(link);
          }
        });

        // Once the new styles are applied, CSS values that are accessible server-side
        // (e.g., getCurrentOutputInfo(), output visibility, etc) may become outdated.
        // At the time of writing, that means we need to do sendImageSize() &
        // sendOutputHiddenState() again, which can be done by re-binding.
        /* global Shiny */
        const bindDebouncer = new Debouncer(null, Shiny.bindAll, 100);

        setTimeout(() => bindDebouncer.normalCall(), 100);
      }
    }

    if (dep.script && !restyle) {
      const scripts = $.map(asArray(dep.script), function (scriptName) {
        return $("<script>").attr("src", href + "/" + encodeURI(scriptName));
      });

      $head.append(scripts);
    }

    if (dep.attachment && !restyle) {
      // dep.attachment might be a single string, an array, or an object.
      let attachments = dep.attachment;

      if (typeof attachments === "string") attachments = [attachments];
      if ($.isArray(attachments)) {
        // The contract for attachments is that arrays of attachments are
        // addressed using 1-based indexes. Convert this array to an object.
        const tmp = {};

        $.each(attachments, function (index, attachment) {
          tmp[index + 1 + ""] = attachment;
        });
        attachments = tmp;
      }

      const attach = $.map(attachments, function (attachment, key) {
        return $("<link rel='attachment'>")
          .attr("id", dep.name + "-" + key + "-attachment")
          .attr("href", href + "/" + encodeURI(attachment));
      });

      $head.append(attach);
    }

    if (dep.head && !restyle) {
      const $newHead = $("<head></head>");

      $newHead.html(dep.head);
      $head.append($newHead.children());
    }
    return true;
  }

  const singletons = {
    knownSingletons: {},
    renderHtml: function (html, el, where) {
      const processed = this._processHtml(html);

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
    register: function (s) {
      $.extend(this.knownSingletons, s);
    },
    // Takes a string or array of strings and adds them to knownSingletons
    registerNames: function (s) {
      if (typeof s === "string") {
        this.knownSingletons[s] = true;
      } else if (s instanceof Array) {
        for (let i = 0; i < s.length; i++) {
          this.knownSingletons[s[i]] = true;
        }
      }
    },
    // Inserts new content into document head
    _addToHead: function (head) {
      if (head.length > 0) {
        const tempDiv = $("<div>" + head + "</div>")[0];
        const $head = $("head");

        while (tempDiv.hasChildNodes()) {
          $head.append(tempDiv.firstChild);
        }
      }
    },
    // Reads HTML and returns an object with info about singletons
    _processHtml: function (val) {
      const self = this;
      const newSingletons = {};
      let newVal;

      const findNewPayload = function (match, p1, sig, payload) {
        if (self.knownSingletons[sig] || newSingletons[sig]) return "";
        newSingletons[sig] = true;
        return payload;
      };

      while (true) {
        newVal = val.replace(self._reSingleton, findNewPayload);
        if (val.length === newVal.length) break;
        val = newVal;
      }

      const heads = [];
      const headAddPayload = function (match, payload) {
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
        singletons: newSingletons,
      };
    },
    _reSingleton: /<!--(SHINY.SINGLETON\[([\w]+)\])-->([\s\S]*?)<!--\/\1-->/,
    _reHead: /<head(?:\s[^>]*)?>([\s\S]*?)<\/head>/,
  };

  // "output_binding_downloadlink.js"
  const downloadLinkOutputBinding = new OutputBinding();

  $.extend(downloadLinkOutputBinding, {
    find: function (scope) {
      return $(scope).find("a.shiny-download-link");
    },
    renderValue: function (el, data) {
      $(el).attr("href", data);
    },
  });
  outputBindings.register(downloadLinkOutputBinding, "shiny.downloadLink");

  // Trigger shiny:filedownload event whenever a downloadButton/Link is clicked
  $(document).on(
    "click.shinyDownloadLink",
    "a.shiny-download-link",
    function (e) {
      const evt = jQuery.Event("shiny:filedownload");

      evt.name = this.id;
      evt.href = this.href;
      $(document).trigger(evt);
    }
  );

  // "output_binding_datatable.js"
  const datatableOutputBinding = new OutputBinding();

  $.extend(datatableOutputBinding, {
    find: function (scope) {
      return $(scope).find(".shiny-datatable-output");
    },
    onValueError: function (el, err) {
      Shiny.unbindAll(el);
      this.renderError(el, err);
    },
    renderValue: function (el, data) {
      const $el = $(el).empty();

      if (!data || !data.colnames) return;

      const colnames = $.makeArray(data.colnames);
      let header = $.map(colnames, function (x) {
        return "<th>" + x + "</th>";
      }).join("");

      header = "<thead><tr>" + header + "</tr></thead>";
      let footer = "";

      if (data.options === null || data.options.searching !== false) {
        footer = $.map(colnames, function (x) {
          // placeholder needs to be escaped (and HTML tags are stripped off)
          return (
            '<th><input type="text" placeholder="' +
            escapeHTML(x.replace(/(<([^>]+)>)/gi, "")) +
            '" /></th>'
          );
        }).join("");
        footer = "<tfoot>" + footer + "</tfoot>";
      }
      const content =
        '<table class="table table-striped table-hover">' +
        header +
        footer +
        "</table>";

      $el.append(content);

      // options that should be eval()ed
      if (data.evalOptions)
        $.each(data.evalOptions, function (i, x) {
          /*jshint evil: true */
          data.options[x] = eval("(" + data.options[x] + ")");
        });

      // caseInsensitive searching? default true
      const searchCI =
        data.options === null ||
        typeof data.options.search === "undefined" ||
        data.options.search.caseInsensitive !== false;
      const oTable = $(el)
        .children("table")
        .DataTable(
          $.extend(
            {
              processing: true,
              serverSide: true,
              order: [],
              orderClasses: false,
              pageLength: 25,
              ajax: {
                url: data.action,
                type: "POST",
                data: function (d) {
                  d.search.caseInsensitive = searchCI;
                  d.escape = data.escape;
                },
              },
            },
            data.options
          )
        );
      // the table object may need post-processing

      if (typeof data.callback === "string") {
        /*jshint evil: true */
        const callback = eval("(" + data.callback + ")");

        if (typeof callback === "function") callback(oTable);
      }

      // use debouncing for searching boxes
      $el
        .find("label input")
        .first()
        .unbind("keyup")
        .keyup(
          debounce(data.searchDelay, function () {
            oTable.search(this.value).draw();
          })
        );
      const searchInputs = $el.find("tfoot input");

      if (searchInputs.length > 0) {
        // this is a little weird: aoColumns/bSearchable are still in DT 1.10
        // https://github.com/DataTables/DataTables/issues/388
        $.each(oTable.settings()[0].aoColumns, function (i, x) {
          // hide the text box if not searchable
          if (!x.bSearchable) searchInputs.eq(i).hide();
        });
        searchInputs.keyup(
          debounce(data.searchDelay, function () {
            oTable.column(searchInputs.index(this)).search(this.value).draw();
          })
        );
      }
      // FIXME: ugly scrollbars in tab panels b/c Bootstrap uses 'visible: auto'
      $el.parents(".tab-content").css("overflow", "visible");
    },
  });
  outputBindings.register(datatableOutputBinding, "shiny.datatableOutput");

  // "output_binding_adapter.js"
  const OutputBindingAdapter = function (el, binding) {
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
  }.call(OutputBindingAdapter.prototype));

  // "input_binding.js"
  const InputBinding = (Shiny.InputBinding = function () {});

  (function () {
    // Returns a jQuery object or element array that contains the
    // descendants of scope that match this binding
    this.find = function (scope) {
      throw "Not implemented";
    };

    this.getId = function (el) {
      return el["data-input-id"] || el.id;
    };

    // Gives the input a type in case the server needs to know it
    // to deserialize the JSON correctly
    this.getType = function () {
      return false;
    };
    this.getValue = function (el) {
      throw "Not implemented";
    };

    // The callback method takes one argument, whose value is boolean. If true,
    // allow deferred (debounce or throttle) sending depending on the value of
    // getRatePolicy. If false, send value immediately.
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
  }.call(InputBinding.prototype));

  // "input_binding_text.js"
  const textInputBinding = new InputBinding();

  $.extend(textInputBinding, {
    find: function (scope) {
      const $inputs = $(scope).find(
        'input[type="text"], input[type="search"], input[type="url"], input[type="email"]'
      );
      // selectize.js 0.12.4 inserts a hidden text input with an
      // id that ends in '-selectized'. The .not() selector below
      // is to prevent textInputBinding from accidentally picking up
      // this hidden element as a shiny input (#2396)

      return $inputs.not('input[type="text"][id$="-selectized"]');
    },
    getId: function (el) {
      return InputBinding.prototype.getId.call(this, el) || el.name;
    },
    getValue: function (el) {
      return el.value;
    },
    setValue: function (el, value) {
      el.value = value;
    },
    subscribe: function (el, callback) {
      $(el).on(
        "keyup.textInputBinding input.textInputBinding",
        function (event) {
          callback(true);
        }
      );
      $(el).on("change.textInputBinding", function (event) {
        callback(false);
      });
    },
    unsubscribe: function (el) {
      $(el).off(".textInputBinding");
    },
    receiveMessage: function (el, data) {
      if (data.hasOwnProperty("value")) this.setValue(el, data.value);

      updateLabel(data.label, this._getLabelNode(el));

      if (data.hasOwnProperty("placeholder")) el.placeholder = data.placeholder;

      $(el).trigger("change");
    },
    getState: function (el) {
      return {
        label: this._getLabelNode(el).text(),
        value: el.value,
        placeholder: el.placeholder,
      };
    },
    getRatePolicy: function () {
      return {
        policy: "debounce",
        delay: 250,
      };
    },
    _getLabelNode: function (el) {
      return $(el)
        .parent()
        .find('label[for="' + $escape(el.id) + '"]');
    },
  });
  inputBindings.register(textInputBinding, "shiny.textInput");

  // "input_binding_textarea.js"
  const textareaInputBinding = {};

  $.extend(textareaInputBinding, textInputBinding, {
    find: function (scope) {
      return $(scope).find("textarea");
    },
  });
  inputBindings.register(textareaInputBinding, "shiny.textareaInput");

  // "input_binding_password.js"
  const passwordInputBinding = {};

  $.extend(passwordInputBinding, textInputBinding, {
    find: function (scope) {
      return $(scope).find('input[type="password"]');
    },
    getType: function (el) {
      return "shiny.password";
    },
  });
  inputBindings.register(passwordInputBinding, "shiny.passwordInput");

  // "input_binding_number.js"
  const numberInputBinding = {};

  $.extend(numberInputBinding, textInputBinding, {
    find: function (scope) {
      return $(scope).find('input[type="number"]');
    },
    getValue: function (el) {
      const numberVal = $(el).val();

      if (/^\s*$/.test(numberVal))
        // Return null if all whitespace
        return null;
      else if (!isNaN(numberVal))
        // If valid Javascript number string, coerce to number
        return +numberVal;
      else return numberVal; // If other string like "1e6", send it unchanged
    },
    setValue: function (el, value) {
      el.value = value;
    },
    getType: function (el) {
      return "shiny.number";
    },
    receiveMessage: function (el, data) {
      if (data.hasOwnProperty("value")) el.value = data.value;
      if (data.hasOwnProperty("min")) el.min = data.min;
      if (data.hasOwnProperty("max")) el.max = data.max;
      if (data.hasOwnProperty("step")) el.step = data.step;

      updateLabel(data.label, this._getLabelNode(el));

      $(el).trigger("change");
    },
    getState: function (el) {
      return {
        label: this._getLabelNode(el).text(),
        value: this.getValue(el),
        min: Number(el.min),
        max: Number(el.max),
        step: Number(el.step),
      };
    },
    _getLabelNode: function (el) {
      return $(el)
        .parent()
        .find('label[for="' + $escape(el.id) + '"]');
    },
  });
  inputBindings.register(numberInputBinding, "shiny.numberInput");

  // "input_binding_checkbox.js"
  const checkboxInputBinding = new InputBinding();

  $.extend(checkboxInputBinding, {
    find: function (scope) {
      return $(scope).find('input[type="checkbox"]');
    },
    getValue: function (el) {
      return el.checked;
    },
    setValue: function (el, value) {
      el.checked = value;
    },
    subscribe: function (el, callback) {
      $(el).on("change.checkboxInputBinding", function (event) {
        callback(true);
      });
    },
    unsubscribe: function (el) {
      $(el).off(".checkboxInputBinding");
    },
    getState: function (el) {
      return {
        label: $(el).parent().find("span").text(),
        value: el.checked,
      };
    },
    receiveMessage: function (el, data) {
      if (data.hasOwnProperty("value")) el.checked = data.value;

      // checkboxInput()'s label works different from other
      // input labels...the label container should always exist
      if (data.hasOwnProperty("label"))
        $(el).parent().find("span").text(data.label);

      $(el).trigger("change");
    },
  });
  inputBindings.register(checkboxInputBinding, "shiny.checkboxInput");

  // "input_binding_slider.js"
  // Necessary to get hidden sliders to send their updated values
  function forceIonSliderUpdate(slider) {
    if (slider.$cache && slider.$cache.input)
      slider.$cache.input.trigger("change");
    else console.log("Couldn't force ion slider to update");
  }

  function getTypePrettifyer(dataType, timeFormat, timezone) {
    let timeFormatter;
    let prettify;

    if (dataType === "date") {
      timeFormatter = strftime.utc();
      prettify = function (num) {
        return timeFormatter(timeFormat, new Date(num));
      };
    } else if (dataType === "datetime") {
      if (timezone) timeFormatter = strftime.timezone(timezone);
      else timeFormatter = strftime;

      prettify = function (num) {
        return timeFormatter(timeFormat, new Date(num));
      };
    } else {
      // The default prettify function for ion.rangeSlider adds thousands
      // separators after the decimal mark, so we have our own version here.
      // (#1958)
      prettify = function (num) {
        // When executed, `this` will refer to the `IonRangeSlider.options`
        // object.
        return formatNumber(num, this.prettify_separator);
      };
    }
    return prettify;
  }

  const sliderInputBinding = {};

  $.extend(sliderInputBinding, textInputBinding, {
    find: function (scope) {
      // Check if ionRangeSlider plugin is loaded
      if (!$.fn.ionRangeSlider) return [];

      return $(scope).find("input.js-range-slider");
    },
    getType: function (el) {
      const dataType = $(el).data("data-type");

      if (dataType === "date") return "shiny.date";
      else if (dataType === "datetime") return "shiny.datetime";
      else return false;
    },
    getValue: function (el) {
      const $el = $(el);
      const result = $(el).data("ionRangeSlider").result;

      // Function for converting numeric value from slider to appropriate type.
      let convert;
      const dataType = $el.data("data-type");

      if (dataType === "date") {
        convert = function (val) {
          return formatDateUTC(new Date(+val));
        };
      } else if (dataType === "datetime") {
        convert = function (val) {
          // Convert ms to s
          return +val / 1000;
        };
      } else {
        convert = function (val) {
          return +val;
        };
      }

      if (this._numValues(el) === 2) {
        return [convert(result.from), convert(result.to)];
      } else {
        return convert(result.from);
      }
    },
    setValue: function (el, value) {
      const $el = $(el);
      const slider = $el.data("ionRangeSlider");

      $el.data("immediate", true);
      try {
        if (this._numValues(el) === 2 && value instanceof Array) {
          slider.update({ from: value[0], to: value[1] });
        } else {
          slider.update({ from: value });
        }

        forceIonSliderUpdate(slider);
      } finally {
        $el.data("immediate", false);
      }
    },
    subscribe: function (el, callback) {
      $(el).on("change.sliderInputBinding", function (event) {
        callback(!$(el).data("immediate") && !$(el).data("animating"));
      });
    },
    unsubscribe: function (el) {
      $(el).off(".sliderInputBinding");
    },
    receiveMessage: function (el, data) {
      const $el = $(el);
      const slider = $el.data("ionRangeSlider");
      const msg = {};

      if (data.hasOwnProperty("value")) {
        if (this._numValues(el) === 2 && data.value instanceof Array) {
          msg.from = data.value[0];
          msg.to = data.value[1];
        } else {
          msg.from = data.value;
        }
      }
      const sliderFeatures = ["min", "max", "step"];

      for (let i = 0; i < sliderFeatures.length; i++) {
        const feats = sliderFeatures[i];

        if (data.hasOwnProperty(feats)) {
          msg[feats] = data[feats];
        }
      }

      updateLabel(data.label, this._getLabelNode(el));

      const domElements = ["data-type", "time-format", "timezone"];

      for (let i = 0; i < domElements.length; i++) {
        const elem = domElements[i];

        if (data.hasOwnProperty(elem)) {
          $el.data(elem, data[elem]);
        }
      }

      const dataType = $el.data("data-type");
      const timeFormat = $el.data("time-format");
      const timezone = $el.data("timezone");

      msg.prettify = getTypePrettifyer(dataType, timeFormat, timezone);

      $el.data("immediate", true);
      try {
        slider.update(msg);
        forceIonSliderUpdate(slider);
      } finally {
        $el.data("immediate", false);
      }
    },
    getRatePolicy: function () {
      return {
        policy: "debounce",
        delay: 250,
      };
    },
    getState: function (el) {},
    initialize: function (el) {
      const opts = {};
      const $el = $(el);
      const dataType = $el.data("data-type");
      const timeFormat = $el.data("time-format");
      const timezone = $el.data("timezone");

      opts.prettify = getTypePrettifyer(dataType, timeFormat, timezone);

      $el.ionRangeSlider(opts);
    },
    _getLabelNode: function (el) {
      return $(el)
        .parent()
        .find('label[for="' + $escape(el.id) + '"]');
    },
    // Number of values; 1 for single slider, 2 for range slider
    _numValues: function (el) {
      if ($(el).data("ionRangeSlider").options.type === "double") return 2;
      else return 1;
    },
  });
  inputBindings.register(sliderInputBinding, "shiny.sliderInput");

  // Format numbers for nicer output.
  // formatNumber(1234567.12345)           === "1,234,567.12345"
  // formatNumber(1234567.12345, ".", ",") === "1.234.567,12345"
  // formatNumber(1000, " ")               === "1 000"
  // formatNumber(20)                      === "20"
  // formatNumber(1.2345e24)               === "1.2345e+24"
  function formatNumber(num, thousand_sep = ",", decimal_sep = ".") {
    const parts = num.toString().split(".");

    // Add separators to portion before decimal mark.
    parts[0] = parts[0].replace(
      /(\d{1,3}(?=(?:\d\d\d)+(?!\d)))/g,
      "$1" + thousand_sep
    );

    if (parts.length === 1) return parts[0];
    else if (parts.length === 2) return parts[0] + decimal_sep + parts[1];
    else return "";
  }

  $(document).on("click", ".slider-animate-button", function (evt) {
    evt.preventDefault();
    const self = $(this);
    const target = $("#" + $escape(self.attr("data-target-id")));
    const startLabel = "Play";
    const stopLabel = "Pause";
    const loop =
      self.attr("data-loop") !== undefined &&
      !/^\s*false\s*$/i.test(self.attr("data-loop"));
    let animInterval = self.attr("data-interval");

    if (isNaN(animInterval)) animInterval = 1500;
    else animInterval = +animInterval;

    if (!target.data("animTimer")) {
      let slider;
      let timer;

      // Separate code paths:
      // Backward compatible code for old-style jsliders (Shiny <= 0.10.2.2),
      // and new-style ionsliders.
      if (target.hasClass("jslider")) {
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
        slider = target.data("ionRangeSlider");
        // Single sliders have slider.options.type == "single", and only the
        // `from` value is used. Double sliders have type == "double", and also
        // use the `to` value for the right handle.
        const sliderCanStep = function () {
          if (slider.options.type === "double")
            return slider.result.to < slider.result.max;
          else return slider.result.from < slider.result.max;
        };
        const sliderReset = function () {
          const val = { from: slider.result.min };
          // Preserve the current spacing for double sliders

          if (slider.options.type === "double")
            val.to = val.from + (slider.result.to - slider.result.from);

          slider.update(val);
          forceIonSliderUpdate(slider);
        };
        const sliderStep = function () {
          // Don't overshoot the end
          const val = {
            from: Math.min(
              slider.result.max,
              slider.result.from + slider.options.step
            ),
          };

          if (slider.options.type === "double")
            val.to = Math.min(
              slider.result.max,
              slider.result.to + slider.options.step
            );

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

      target.data("animTimer", timer);
      self.attr("title", stopLabel);
      self.addClass("playing");
      target.data("animating", true);
    } else {
      clearTimeout(target.data("animTimer"));
      target.removeData("animTimer");
      self.attr("title", startLabel);
      self.removeClass("playing");
      target.removeData("animating");
    }
  });

  // "input_binding_date.js"
  const dateInputBinding = new InputBinding();

  $.extend(dateInputBinding, {
    find: function (scope) {
      return $(scope).find(".shiny-date-input");
    },
    getType: function (el) {
      return "shiny.date";
    },
    // Return the date in an unambiguous format, yyyy-mm-dd (as opposed to a
    // format like mm/dd/yyyy)
    getValue: function (el) {
      const date = $(el).find("input").bsDatepicker("getUTCDate");

      return formatDateUTC(date);
    },
    // value must be an unambiguous string like '2001-01-01', or a Date object.
    setValue: function (el, value) {
      // R's NA, which is null here will remove current value
      if (value === null) {
        $(el).find("input").val("").bsDatepicker("update");
        return;
      }

      const date = this._newDate(value);
      // If date is invalid, do nothing

      if (isNaN(date)) return;

      $(el).find("input").bsDatepicker("setUTCDate", date);
    },
    getState: function (el) {
      const $el = $(el);
      const $input = $el.find("input");

      let min = $input.data("datepicker").startDate;
      let max = $input.data("datepicker").endDate;

      // Stringify min and max. If min and max aren't set, they will be
      // -Infinity and Infinity; replace these with null.
      min = min === -Infinity ? null : formatDateUTC(min);
      max = max === Infinity ? null : formatDateUTC(max);

      // startViewMode is stored as a number; convert to string
      let startview = $input.data("datepicker").startViewMode;

      if (startview === 2) startview = "decade";
      else if (startview === 1) startview = "year";
      else if (startview === 0) startview = "month";

      return {
        label: this._getLabelNode(el).text(),
        value: this.getValue(el),
        valueString: $input.val(),
        min: min,
        max: max,
        language: $input.data("datepicker").language,
        weekstart: $input.data("datepicker").weekStart,
        format: this._formatToString($input.data("datepicker").format),
        startview: startview,
      };
    },
    receiveMessage: function (el, data) {
      const $input = $(el).find("input");

      updateLabel(data.label, this._getLabelNode(el));

      if (data.hasOwnProperty("min")) this._setMin($input[0], data.min);

      if (data.hasOwnProperty("max")) this._setMax($input[0], data.max);

      // Must set value only after min and max have been set. If new value is
      // outside the bounds of the previous min/max, then the result will be a
      // blank input.
      if (data.hasOwnProperty("value")) this.setValue(el, data.value);

      $(el).trigger("change");
    },
    subscribe: function (el, callback) {
      $(el).on(
        "keyup.dateInputBinding input.dateInputBinding",
        function (event) {
          // Use normal debouncing policy when typing
          callback(true);
        }
      );
      $(el).on(
        "changeDate.dateInputBinding change.dateInputBinding",
        function (event) {
          // Send immediately when clicked
          callback(false);
        }
      );
    },
    unsubscribe: function (el) {
      $(el).off(".dateInputBinding");
    },
    getRatePolicy: function () {
      return {
        policy: "debounce",
        delay: 250,
      };
    },
    initialize: function (el) {
      const $input = $(el).find("input");

      // The challenge with dates is that we want them to be at 00:00 in UTC so
      // that we can do comparisons with them. However, the Date object itself
      // does not carry timezone information, so we should call _floorDateTime()
      // on Dates as soon as possible so that we know we're always working with
      // consistent objects.

      let date = $input.data("initial-date");
      // If initial_date is null, set to current date

      if (date === undefined || date === null) {
        // Get local date, but normalized to beginning of day in UTC.
        date = this._floorDateTime(this._dateAsUTC(new Date()));
      }

      this.setValue(el, date);

      // Set the start and end dates, from min-date and max-date. These always
      // use yyyy-mm-dd format, instead of bootstrap-datepicker's built-in
      // support for date-startdate and data-enddate, which use the current
      // date format.
      if ($input.data("min-date") !== undefined) {
        this._setMin($input[0], $input.data("min-date"));
      }
      if ($input.data("max-date") !== undefined) {
        this._setMax($input[0], $input.data("max-date"));
      }
    },
    _getLabelNode: function (el) {
      return $(el).find('label[for="' + $escape(el.id) + '"]');
    },
    // Given a format object from a date picker, return a string
    _formatToString: function (format) {
      // Format object has structure like:
      // { parts: ['mm', 'dd', 'yy'], separators: ['', '/', '/' ,''] }
      let str = "";

      let i;

      for (i = 0; i < format.parts.length; i++) {
        str += format.separators[i] + format.parts[i];
      }
      str += format.separators[i];
      return str;
    },
    // Given an unambiguous date string or a Date object, set the min (start) date.
    // null will unset. undefined will result in no change,
    _setMin: function (el, date) {
      if (date === undefined) return;
      if (date === null) {
        $(el).bsDatepicker("setStartDate", null);
        return;
      }

      date = this._newDate(date);
      // If date parsing fails, do nothing
      if (date === null) return;

      if (isNaN(date)) return;
      // Workarounds for
      // https://github.com/rstudio/shiny/issues/2335
      const curValue = $(el).bsDatepicker("getUTCDate");

      // Note that there's no `setUTCStartDate`, so we need to convert this Date.
      // It starts at 00:00 UTC, and we convert it to 00:00 in local time, which
      // is what's needed for `setStartDate`.
      $(el).bsDatepicker("setStartDate", this._UTCDateAsLocal(date));

      // If the new min is greater than the current date, unset the current date.
      if (date && curValue && date.getTime() > curValue.getTime()) {
        $(el).bsDatepicker("clearDates");
      } else {
        // Setting the date needs to be done AFTER `setStartDate`, because the
        // datepicker has a bug where calling `setStartDate` will clear the date
        // internally (even though it will still be visible in the UI) when a
        // 2-digit year format is used.
        // https://github.com/eternicode/bootstrap-datepicker/issues/2010
        $(el).bsDatepicker("setUTCDate", curValue);
      }
    },
    // Given an unambiguous date string or a Date object, set the max (end) date
    // null will unset.
    _setMax: function (el, date) {
      if (date === undefined) return;
      if (date === null) {
        $(el).bsDatepicker("setEndDate", null);
        return;
      }

      date = this._newDate(date);
      // If date parsing fails, do nothing
      if (date === null) return;

      if (isNaN(date)) return;

      // Workaround for same issue as in _setMin.
      const curValue = $(el).bsDatepicker("getUTCDate");

      $(el).bsDatepicker("setEndDate", this._UTCDateAsLocal(date));

      // If the new min is greater than the current date, unset the current date.
      if (date && curValue && date.getTime() < curValue.getTime()) {
        $(el).bsDatepicker("clearDates");
      } else {
        $(el).bsDatepicker("setUTCDate", curValue);
      }
    },
    // Given a date string of format yyyy-mm-dd, return a Date object with
    // that date at 12AM UTC.
    // If date is a Date object, return it unchanged.
    _newDate: function (date) {
      if (date instanceof Date) return date;
      if (!date) return null;

      // Get Date object - this will be at 12AM in UTC, but may print
      // differently at the Javascript console.
      const d = parseDate(date);

      // If invalid date, return null
      if (isNaN(d)) return null;

      return d;
    },
    // A Date can have any time during a day; this will return a new Date object
    // set to 00:00 in UTC.
    _floorDateTime: function (date) {
      date = new Date(date.getTime());
      date.setUTCHours(0, 0, 0, 0);
      return date;
    },
    // Given a Date object, return a Date object which has the same "clock time"
    // in UTC. For example, if input date is 2013-02-01 23:00:00 GMT-0600 (CST),
    // output will be 2013-02-01 23:00:00 UTC. Note that the JS console may
    // print this in local time, as "Sat Feb 02 2013 05:00:00 GMT-0600 (CST)".
    _dateAsUTC: function (date) {
      return new Date(date.getTime() - date.getTimezoneOffset() * 60000);
    },
    // The inverse of _dateAsUTC. This is needed to adjust time zones because
    // some bootstrap-datepicker methods only take local dates as input, and not
    // UTC.
    _UTCDateAsLocal: function (date) {
      return new Date(date.getTime() + date.getTimezoneOffset() * 60000);
    },
  });
  inputBindings.register(dateInputBinding, "shiny.dateInput");

  // "input_binding_daterange.js"
  const dateRangeInputBinding = {};

  $.extend(dateRangeInputBinding, dateInputBinding, {
    find: function (scope) {
      return $(scope).find(".shiny-date-range-input");
    },
    // Return the date in an unambiguous format, yyyy-mm-dd (as opposed to a
    // format like mm/dd/yyyy)
    getValue: function (el) {
      const $inputs = $(el).find("input");
      const start = $inputs.eq(0).bsDatepicker("getUTCDate");
      const end = $inputs.eq(1).bsDatepicker("getUTCDate");

      return [formatDateUTC(start), formatDateUTC(end)];
    },
    // value must be an object, with optional fields `start` and `end`. These
    // should be unambiguous strings like '2001-01-01', or Date objects.
    setValue: function (el, value) {
      if (!(value instanceof Object)) {
        return;
      }

      // Get the start and end input objects
      const $inputs = $(el).find("input");

      // If value is undefined, don't try to set
      // null will remove the current value
      if (value.start !== undefined) {
        if (value.start === null) {
          $inputs.eq(0).val("").bsDatepicker("update");
        } else {
          const start = this._newDate(value.start);

          $inputs.eq(0).bsDatepicker("setUTCDate", start);
        }
      }
      if (value.end !== undefined) {
        if (value.end === null) {
          $inputs.eq(1).val("").bsDatepicker("update");
        } else {
          const end = this._newDate(value.end);

          $inputs.eq(1).bsDatepicker("setUTCDate", end);
        }
      }
    },
    getState: function (el) {
      const $el = $(el);
      const $inputs = $el.find("input");
      const $startinput = $inputs.eq(0);
      const $endinput = $inputs.eq(1);

      // For many of the properties, assume start and end have the same values
      let min = $startinput.bsDatepicker("getStartDate");
      let max = $startinput.bsDatepicker("getEndDate");

      // Stringify min and max. If min and max aren't set, they will be
      // -Infinity and Infinity; replace these with null.
      min = min === -Infinity ? null : formatDateUTC(min);
      max = max === Infinity ? null : formatDateUTC(max);

      // startViewMode is stored as a number; convert to string
      let startview = $startinput.data("datepicker").startView;

      if (startview === 2) startview = "decade";
      else if (startview === 1) startview = "year";
      else if (startview === 0) startview = "month";

      return {
        label: this._getLabelNode(el).text(),
        value: this.getValue(el),
        valueString: [$startinput.val(), $endinput.val()],
        min: min,
        max: max,
        weekstart: $startinput.data("datepicker").weekStart,
        format: this._formatToString($startinput.data("datepicker").format),
        language: $startinput.data("datepicker").language,
        startview: startview,
      };
    },
    receiveMessage: function (el, data) {
      const $el = $(el);
      const $inputs = $el.find("input");
      const $startinput = $inputs.eq(0);
      const $endinput = $inputs.eq(1);

      updateLabel(data.label, this._getLabelNode(el));

      if (data.hasOwnProperty("min")) {
        this._setMin($startinput[0], data.min);
        this._setMin($endinput[0], data.min);
      }

      if (data.hasOwnProperty("max")) {
        this._setMax($startinput[0], data.max);
        this._setMax($endinput[0], data.max);
      }

      // Must set value only after min and max have been set. If new value is
      // outside the bounds of the previous min/max, then the result will be a
      // blank input.
      if (data.hasOwnProperty("value")) this.setValue(el, data.value);

      $el.trigger("change");
    },
    initialize: function (el) {
      const $el = $(el);
      const $inputs = $el.find("input");
      const $startinput = $inputs.eq(0);
      const $endinput = $inputs.eq(1);

      let start = $startinput.data("initial-date");
      let end = $endinput.data("initial-date");

      // If empty/null, use local date, but as UTC
      if (start === undefined || start === null)
        start = this._dateAsUTC(new Date());

      if (end === undefined || end === null) end = this._dateAsUTC(new Date());

      this.setValue(el, { start: start, end: end });

      // // Set the start and end dates, from min-date and max-date. These always
      // // use yyyy-mm-dd format, instead of bootstrap-datepicker's built-in
      // // support for date-startdate and data-enddate, which use the current
      // // date format.
      this._setMin($startinput[0], $startinput.data("min-date"));
      this._setMin($endinput[0], $startinput.data("min-date"));
      this._setMax($startinput[0], $endinput.data("max-date"));
      this._setMax($endinput[0], $endinput.data("max-date"));
    },
    subscribe: function (el, callback) {
      $(el).on(
        "keyup.dateRangeInputBinding input.dateRangeInputBinding",
        function (event) {
          // Use normal debouncing policy when typing
          callback(true);
        }
      );
      $(el).on(
        "changeDate.dateRangeInputBinding change.dateRangeInputBinding",
        function (event) {
          // Send immediately when clicked
          callback(false);
        }
      );
    },
    unsubscribe: function (el) {
      $(el).off(".dateRangeInputBinding");
    },
    _getLabelNode: function (el) {
      return $(el).find('label[for="' + $escape(el.id) + '"]');
    },
  });
  inputBindings.register(dateRangeInputBinding, "shiny.dateRangeInput");

  // "input_binding_select.js"
  const selectInputBinding = new InputBinding();

  $.extend(selectInputBinding, {
    find: function (scope) {
      return $(scope).find("select");
    },
    getType: function (el) {
      const $el = $(el);

      if (!$el.hasClass("symbol")) {
        // default character type
        return null;
      }
      if ($el.attr("multiple") === "multiple") {
        return "shiny.symbolList";
      } else {
        return "shiny.symbol";
      }
    },
    getId: function (el) {
      return InputBinding.prototype.getId.call(this, el) || el.name;
    },
    getValue: function (el) {
      return $(el).val();
    },
    setValue: function (el, value) {
      if (!this._is_selectize(el)) {
        $(el).val(value);
      } else {
        const selectize = this._selectize(el);

        if (selectize) {
          selectize.setValue(value);
        }
      }
    },
    getState: function (el) {
      // Store options in an array of objects, each with with value and label
      const options = new Array(el.length);

      for (let i = 0; i < el.length; i++) {
        options[i] = { value: el[i].value, label: el[i].label };
      }

      return {
        label: this._getLabelNode(el),
        value: this.getValue(el),
        options: options,
      };
    },
    receiveMessage: function (el, data) {
      let $el = $(el),
        selectize;

      // This will replace all the options
      if (data.hasOwnProperty("options")) {
        selectize = this._selectize(el);
        // Must destroy selectize before appending new options, otherwise
        // selectize will restore the original select
        if (selectize) selectize.destroy();
        // Clear existing options and add each new one
        $el.empty().append(data.options);
        this._selectize(el);
      }

      // re-initialize selectize
      if (data.hasOwnProperty("config")) {
        $el
          .parent()
          .find('script[data-for="' + $escape(el.id) + '"]')
          .replaceWith(data.config);
        this._selectize(el, true);
      }

      // use server-side processing for selectize
      if (data.hasOwnProperty("url")) {
        selectize = this._selectize(el);
        selectize.clearOptions();
        let loaded = false;

        selectize.settings.load = function (query, callback) {
          const settings = selectize.settings;

          $.ajax({
            url: data.url,
            data: {
              query: query,
              field: JSON.stringify([settings.searchField]),
              value: settings.valueField,
              conju: settings.searchConjunction,
              maxop: settings.maxOptions,
            },
            type: "GET",
            error: function () {
              callback();
            },
            success: function (res) {
              // res = [{label: '1', value: '1', group: '1'}, ...]
              // success is called after options are added, but
              // groups need to be added manually below
              $.each(res, function (index, elem) {
                // Call selectize.addOptionGroup once for each optgroup; the
                // first argument is the group ID, the second is an object with
                // the group's label and value. We use the current settings of
                // the selectize object to decide the fieldnames of that obj.
                const optgroupId = elem[settings.optgroupField || "optgroup"];
                const optgroup = {};

                optgroup[settings.optgroupLabelField || "label"] = optgroupId;
                optgroup[settings.optgroupValueField || "value"] = optgroupId;
                selectize.addOptionGroup(optgroupId, optgroup);
              });
              callback(res);
              if (!loaded) {
                if (data.hasOwnProperty("value")) {
                  selectize.setValue(data.value);
                } else if (settings.maxItems === 1) {
                  // first item selected by default only for single-select
                  selectize.setValue(res[0].value);
                }
              }
              loaded = true;
            },
          });
        };
        // perform an empty search after changing the `load` function
        selectize.load(function (callback) {
          selectize.settings.load.apply(selectize, ["", callback]);
        });
      } else if (data.hasOwnProperty("value")) {
        this.setValue(el, data.value);
      }

      updateLabel(data.label, this._getLabelNode(el));

      $(el).trigger("change");
    },
    subscribe: function (el, callback) {
      $(el).on("change.selectInputBinding", (event) => {
        // https://github.com/rstudio/shiny/issues/2162
        // Prevent spurious events that are gonna be squelched in
        // a second anyway by the onItemRemove down below
        if (el.nonempty && this.getValue(el) === "") {
          return;
        }
        callback();
      });
    },
    unsubscribe: function (el) {
      $(el).off(".selectInputBinding");
    },
    initialize: function (el) {
      this._selectize(el);
    },
    _getLabelNode: function (el) {
      let escaped_id = $escape(el.id);

      if (this._is_selectize(el)) {
        escaped_id += "-selectized";
      }
      return $(el)
        .parent()
        .parent()
        .find('label[for="' + escaped_id + '"]');
    },
    // Return true if it's a selectize input, false if it's a regular select input.
    _is_selectize: function (el) {
      const config = $(el)
        .parent()
        .find('script[data-for="' + $escape(el.id) + '"]');

      return config.length > 0;
    },
    _selectize: function (el, update) {
      if (!$.fn.selectize) return undefined;
      const $el = $(el);
      const config = $el
        .parent()
        .find('script[data-for="' + $escape(el.id) + '"]');

      if (config.length === 0) return undefined;

      let options = $.extend(
        {
          labelField: "label",
          valueField: "value",
          searchField: ["label"],
        },
        JSON.parse(config.html())
      );

      // selectize created from selectInput()
      if (typeof config.data("nonempty") !== "undefined") {
        el.nonempty = true;
        options = $.extend(options, {
          onItemRemove: function (value) {
            if (this.getValue() === "")
              $("select#" + $escape(el.id))
                .empty()
                .append(
                  $("<option/>", {
                    value: value,
                    selected: true,
                  })
                )
                .trigger("change");
          },
          onDropdownClose: function ($dropdown) {
            if (this.getValue() === "")
              this.setValue($("select#" + $escape(el.id)).val());
          },
        });
      } else {
        el.nonempty = false;
      }
      // options that should be eval()ed
      if (config.data("eval") instanceof Array)
        $.each(config.data("eval"), function (i, x) {
          /*jshint evil: true*/
          options[x] = eval("(" + options[x] + ")");
        });
      let control = $el.selectize(options)[0].selectize;
      // .selectize() does not really update settings; must destroy and rebuild

      if (update) {
        const settings = $.extend(control.settings, options);

        control.destroy();
        control = $el.selectize(settings)[0].selectize;
      }
      return control;
    },
  });
  inputBindings.register(selectInputBinding, "shiny.selectInput");

  // "input_binding_radio.js"
  const radioInputBinding = new InputBinding();

  $.extend(radioInputBinding, {
    find: function (scope) {
      return $(scope).find(".shiny-input-radiogroup");
    },
    getValue: function (el) {
      // Select the radio objects that have name equal to the grouping div's id
      const checked_items = $(
        'input:radio[name="' + $escape(el.id) + '"]:checked'
      );

      if (checked_items.length === 0) {
        // If none are checked, the input will return null (it's the default on load,
        // but it wasn't emptied when calling updateRadioButtons with character(0)
        return null;
      }

      return checked_items.val();
    },
    setValue: function (el, value) {
      if ($.isArray(value) && value.length === 0) {
        // Removing all checked item if the sent data is empty
        $('input:radio[name="' + $escape(el.id) + '"]').prop("checked", false);
      } else {
        $(
          'input:radio[name="' +
            $escape(el.id) +
            '"][value="' +
            $escape(value) +
            '"]'
        ).prop("checked", true);
      }
    },
    getState: function (el) {
      const $objs = $('input:radio[name="' + $escape(el.id) + '"]');

      // Store options in an array of objects, each with with value and label
      const options = new Array($objs.length);

      for (let i = 0; i < options.length; i++) {
        options[i] = { value: $objs[i].value, label: this._getLabel($objs[i]) };
      }

      return {
        label: this._getLabelNode(el).text(),
        value: this.getValue(el),
        options: options,
      };
    },
    receiveMessage: function (el, data) {
      const $el = $(el);
      // This will replace all the options

      if (data.hasOwnProperty("options")) {
        // Clear existing options and add each new one
        $el.find("div.shiny-options-group").remove();
        // Backward compatibility: for HTML generated by shinybootstrap2 package
        $el.find("label.radio").remove();
        $el.append(data.options);
      }

      if (data.hasOwnProperty("value")) this.setValue(el, data.value);

      updateLabel(data.label, this._getLabelNode(el));

      $(el).trigger("change");
    },
    subscribe: function (el, callback) {
      $(el).on("change.radioInputBinding", function (event) {
        callback();
      });
    },
    unsubscribe: function (el) {
      $(el).off(".radioInputBinding");
    },
    // Get the DOM element that contains the top-level label
    _getLabelNode: function (el) {
      return $(el)
        .parent()
        .find('label[for="' + $escape(el.id) + '"]');
    },
    // Given an input DOM object, get the associated label. Handles labels
    // that wrap the input as well as labels associated with 'for' attribute.
    _getLabel: function (obj) {
      // If <label><input /><span>label text</span></label>
      if (obj.parentNode.tagName === "LABEL") {
        return $(obj.parentNode).find("span").text().trim();
      }

      return null;
    },
    // Given an input DOM object, set the associated label. Handles labels
    // that wrap the input as well as labels associated with 'for' attribute.
    _setLabel: function (obj, value) {
      // If <label><input /><span>label text</span></label>
      if (obj.parentNode.tagName === "LABEL") {
        $(obj.parentNode).find("span").text(value);
      }

      return null;
    },
  });
  inputBindings.register(radioInputBinding, "shiny.radioInput");

  // "input_binding_checkboxgroup.js"
  const checkboxGroupInputBinding = new InputBinding();

  $.extend(checkboxGroupInputBinding, {
    find: function (scope) {
      return $(scope).find(".shiny-input-checkboxgroup");
    },
    getValue: function (el) {
      // Select the checkbox objects that have name equal to the grouping div's id
      const $objs = $('input:checkbox[name="' + $escape(el.id) + '"]:checked');
      const values = new Array($objs.length);

      for (let i = 0; i < $objs.length; i++) {
        values[i] = $objs[i].value;
      }
      return values;
    },
    setValue: function (el, value) {
      // Clear all checkboxes
      $('input:checkbox[name="' + $escape(el.id) + '"]').prop("checked", false);

      // Accept array
      if (value instanceof Array) {
        for (let i = 0; i < value.length; i++) {
          $(
            'input:checkbox[name="' +
              $escape(el.id) +
              '"][value="' +
              $escape(value[i]) +
              '"]'
          ).prop("checked", true);
        }
        // Else assume it's a single value
      } else {
        $(
          'input:checkbox[name="' +
            $escape(el.id) +
            '"][value="' +
            $escape(value) +
            '"]'
        ).prop("checked", true);
      }
    },
    getState: function (el) {
      const $objs = $('input:checkbox[name="' + $escape(el.id) + '"]');

      // Store options in an array of objects, each with with value and label
      const options = new Array($objs.length);

      for (let i = 0; i < options.length; i++) {
        options[i] = { value: $objs[i].value, label: this._getLabel($objs[i]) };
      }

      return {
        label: this._getLabelNode(el).text(),
        value: this.getValue(el),
        options: options,
      };
    },
    receiveMessage: function (el, data) {
      const $el = $(el);

      // This will replace all the options
      if (data.hasOwnProperty("options")) {
        // Clear existing options and add each new one
        $el.find("div.shiny-options-group").remove();
        // Backward compatibility: for HTML generated by shinybootstrap2 package
        $el.find("label.checkbox").remove();
        $el.append(data.options);
      }

      if (data.hasOwnProperty("value")) this.setValue(el, data.value);

      updateLabel(data.label, this._getLabelNode(el));

      $(el).trigger("change");
    },
    subscribe: function (el, callback) {
      $(el).on("change.checkboxGroupInputBinding", function (event) {
        callback();
      });
    },
    unsubscribe: function (el) {
      $(el).off(".checkboxGroupInputBinding");
    },
    // Get the DOM element that contains the top-level label
    _getLabelNode: function (el) {
      return $(el).find('label[for="' + $escape(el.id) + '"]');
    },
    // Given an input DOM object, get the associated label. Handles labels
    // that wrap the input as well as labels associated with 'for' attribute.
    _getLabel: function (obj) {
      // If <label><input /><span>label text</span></label>
      if (obj.parentNode.tagName === "LABEL") {
        return $(obj.parentNode).find("span").text().trim();
      }

      return null;
    },
    // Given an input DOM object, set the associated label. Handles labels
    // that wrap the input as well as labels associated with 'for' attribute.
    _setLabel: function (obj, value) {
      // If <label><input /><span>label text</span></label>
      if (obj.parentNode.tagName === "LABEL") {
        $(obj.parentNode).find("span").text(value);
      }

      return null;
    },
  });
  inputBindings.register(checkboxGroupInputBinding, "shiny.checkboxGroupInput");

  // "input_binding_actionbutton.js"
  const actionButtonInputBinding = new InputBinding();

  $.extend(actionButtonInputBinding, {
    find: function (scope) {
      return $(scope).find(".action-button");
    },
    getValue: function (el) {
      return $(el).data("val") || 0;
    },
    setValue: function (el, value) {
      $(el).data("val", value);
    },
    getType: function (el) {
      return "shiny.action";
    },
    subscribe: function (el, callback) {
      $(el).on("click.actionButtonInputBinding", function (e) {
        const $el = $(this);
        const val = $el.data("val") || 0;

        $el.data("val", val + 1);

        callback();
      });
    },
    getState: function (el) {
      return { value: this.getValue(el) };
    },
    receiveMessage: function (el, data) {
      const $el = $(el);

      // retrieve current label and icon
      let label = $el.text();
      let icon = "";

      // to check (and store) the previous icon, we look for a $el child
      // object that has an i tag, and some (any) class (this prevents
      // italicized text - which has an i tag but, usually, no class -
      // from being mistakenly selected)
      if ($el.find("i[class]").length > 0) {
        const icon_html = $el.find("i[class]")[0];

        if (icon_html === $el.children()[0]) {
          // another check for robustness
          icon = $(icon_html).prop("outerHTML");
        }
      }

      // update the requested properties
      if (data.hasOwnProperty("label")) label = data.label;
      if (data.hasOwnProperty("icon")) {
        icon = data.icon;
        // if the user entered icon=character(0), remove the icon
        if (icon.length === 0) icon = "";
      }

      // produce new html
      $el.html(icon + " " + label);
    },
    unsubscribe: function (el) {
      $(el).off(".actionButtonInputBinding");
    },
  });
  inputBindings.register(actionButtonInputBinding, "shiny.actionButtonInput");

  $(document).on("click", "a.action-button", function (e) {
    e.preventDefault();
  });

  // "input_binding_tabinput.js"
  const bootstrapTabInputBinding = new InputBinding();

  $.extend(bootstrapTabInputBinding, {
    find: function (scope) {
      return $(scope).find("ul.nav.shiny-tab-input");
    },
    getValue: function (el) {
      const anchor = $(el).find("li:not(.dropdown).active").children("a");

      if (anchor.length === 1) return this._getTabName(anchor);

      return null;
    },
    setValue: function (el, value) {
      const self = this;
      let success = false;

      if (value) {
        const anchors = $(el).find("li:not(.dropdown)").children("a");

        anchors.each(function () {
          if (self._getTabName($(this)) === value) {
            $(this).tab("show");
            success = true;
            return false; // Break out of each()
          }
          return true;
        });
      }
      if (!success) {
        // This is to handle the case where nothing is selected, e.g. the last tab
        // was removed using removeTab.
        $(el).trigger("change");
      }
    },
    getState: function (el) {
      return { value: this.getValue(el) };
    },
    receiveMessage: function (el, data) {
      if (data.hasOwnProperty("value")) this.setValue(el, data.value);
      $(el).trigger("change");
    },
    subscribe: function (el, callback) {
      $(el).on(
        "change shown.bootstrapTabInputBinding shown.bs.tab.bootstrapTabInputBinding",
        function (event) {
          callback();
        }
      );
    },
    unsubscribe: function (el) {
      $(el).off(".bootstrapTabInputBinding");
    },
    _getTabName: function (anchor) {
      return anchor.attr("data-value") || anchor.text();
    },
  });
  inputBindings.register(bootstrapTabInputBinding, "shiny.bootstrapTabInput");

  // "input_binding_fileinput.js"
  const FileUploader = function (shinyapp, id, files, el) {
    this.shinyapp = shinyapp;
    this.id = id;
    this.el = el;
    FileProcessor.call(this, files);
  };

  $.extend(FileUploader.prototype, FileProcessor.prototype);
  (function () {
    this.makeRequest = function (method, args, onSuccess, onFailure, blobs) {
      this.shinyapp.makeRequest(method, args, onSuccess, onFailure, blobs);
    };
    this.onBegin = function (files, cont) {
      const self = this;

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

      const fileInfo = $.map(files, function (file, i) {
        return {
          name: file.name,
          size: file.size,
          type: file.type,
        };
      });

      this.makeRequest(
        "uploadInit",
        [fileInfo],
        function (response) {
          self.jobId = response.jobId;
          self.uploadUrl = response.uploadUrl;
          cont();
        },
        function (error) {
          self.onError(error);
        }
      );
    };
    this.onFile = function (file, cont) {
      const self = this;

      this.onProgress(file, 0);

      $.ajax(this.uploadUrl, {
        type: "POST",
        cache: false,
        xhr: function () {
          const xhrVal = $.ajaxSettings.xhr();

          if (xhrVal.upload) {
            xhrVal.upload.onprogress = function (e) {
              if (e.lengthComputable) {
                self.onProgress(
                  file,
                  (self.progressBytes + e.loaded) / self.totalBytes
                );
              }
            };
          }
          return xhrVal;
        },
        data: file,
        contentType: "application/octet-stream",
        processData: false,
        success: function () {
          self.progressBytes += file.size;
          cont();
        },
        error: function (jqXHR, textStatus, errorThrown) {
          self.onError(jqXHR.responseText || textStatus);
        },
      });
    };
    this.onComplete = function () {
      const self = this;

      const fileInfo = $.map(this.files, function (file, i) {
        return {
          name: file.name,
          size: file.size,
          type: file.type,
        };
      });

      // Trigger shiny:inputchanged. Unlike a normal shiny:inputchanged event,
      // it's not possible to modify the information before the values get
      // sent to the server.
      const evt = jQuery.Event("shiny:inputchanged");

      evt.name = this.id;
      evt.value = fileInfo;
      evt.binding = fileInputBinding;
      evt.el = this.el;
      evt.inputType = "shiny.fileupload";
      $(document).trigger(evt);

      this.makeRequest(
        "uploadEnd",
        [this.jobId, this.id],
        function (response) {
          self.$setActive(false);
          self.onProgress(null, 1);
          self.$bar().text("Upload complete");
          // Reset the file input's value to "". This allows the same file to be
          // uploaded again. https://stackoverflow.com/a/22521275
          $(evt.el).val("");
        },
        function (error) {
          self.onError(error);
        }
      );
      this.$bar().text("Finishing upload");
    };
    this.onError = function (message) {
      this.$setError(message || "");
      this.$setActive(false);
    };
    this.onAbort = function () {
      this.$setVisible(false);
    };
    this.onProgress = function (file, completed) {
      this.$bar().width(Math.round(completed * 100) + "%");
      this.$bar().text(file ? file.name : "");
    };
    this.$container = function () {
      return $("#" + $escape(this.id) + "_progress.shiny-file-input-progress");
    };
    this.$bar = function () {
      return $(
        "#" +
          $escape(this.id) +
          "_progress.shiny-file-input-progress .progress-bar"
      );
    };
    this.$setVisible = function (visible) {
      this.$container().css("visibility", visible ? "visible" : "hidden");
    };
    this.$setError = function (error) {
      this.$bar().toggleClass("progress-bar-danger", error !== null);
      if (error !== null) {
        this.onProgress(null, 1);
        this.$bar().text(error);
      }
    };
    this.$setActive = function (active) {
      this.$container().toggleClass("active", !!active);
    };
  }.call(FileUploader.prototype));

  // NOTE On Safari, at least version 10.1.2, *if the developer console is open*,
  // setting the input's value will behave strangely because of a Safari bug. The
  // uploaded file's name will appear over the placeholder value, instead of
  // replacing it. The workaround is to restart Safari. When I (Alan Dipert) ran
  // into this bug Winston Chang helped me diagnose the exact problem, and Winston
  // then submitted a bug report to Apple.
  function setFileText($el, files) {
    const $fileText = $el.closest("div.input-group").find("input[type=text]");

    if (files.length === 1) {
      $fileText.val(files[0].name);
    } else {
      $fileText.val(files.length + " files");
    }
  }

  // If previously selected files are uploading, abort that.
  function abortCurrentUpload($el) {
    const uploader = $el.data("currentUploader");

    if (uploader) uploader.abort();
    // Clear data-restore attribute if present.
    $el.removeAttr("data-restore");
  }

  function uploadDroppedFilesIE10Plus(el, files) {
    const $el = $(el);

    abortCurrentUpload($el);

    // Set the label in the text box
    setFileText($el, files);

    // Start the new upload and put the uploader in 'currentUploader'.
    $el.data(
      "currentUploader",
      new FileUploader(Shiny.shinyapp, fileInputBinding.getId(el), files, el)
    );
  }

  function uploadFiles(evt) {
    const $el = $(evt.target);

    abortCurrentUpload($el);

    const files = evt.target.files;
    const id = fileInputBinding.getId(evt.target);

    if (files.length === 0) return;

    // Set the label in the text box
    setFileText($el, files);

    // Start the new upload and put the uploader in 'currentUploader'.
    $el.data(
      "currentUploader",
      new FileUploader(Shiny.shinyapp, id, files, evt.target)
    );
  }

  // Here we maintain a list of all the current file inputs. This is necessary
  // because we need to trigger events on them in order to respond to file drag
  // events. For example, they should all light up when a file is dragged on to
  // the page.
  let $fileInputs = $();

  const fileInputBinding = new InputBinding();

  $.extend(fileInputBinding, {
    find: function (scope) {
      return $(scope).find('input[type="file"]');
    },
    getId: function (el) {
      return InputBinding.prototype.getId.call(this, el) || el.name;
    },
    getValue: function (el) {
      // This returns a non-undefined value only when there's a 'data-restore'
      // attribute, which is set only when restoring Shiny state. If a file is
      // uploaded through the browser, 'data-restore' gets cleared.
      let data = $(el).attr("data-restore");

      if (data) {
        data = JSON.parse(data);

        // Set the label in the text box
        const $fileText = $(el)
          .closest("div.input-group")
          .find("input[type=text]");

        if (data.name.length === 1) {
          $fileText.val(data.name[0]);
        } else {
          $fileText.val(data.name.length + " files");
        }

        // Manually set up progress bar. A bit inelegant because it duplicates
        // code from FileUploader, but duplication is less bad than alternatives.
        const $progress = $(el).closest("div.form-group").find(".progress");
        const $bar = $progress.find(".progress-bar");

        $progress.removeClass("active");
        $bar.width("100%");
        $bar.css("visibility", "visible");

        return data;
      } else {
        return null;
      }
    },
    setValue: function (el, value) {
      // Not implemented
    },
    getType: function (el) {
      // This will be used only when restoring a file from a saved state.
      return "shiny.file";
    },
    _zoneOf: function (el) {
      return $(el).closest("div.input-group");
    },
    // This function makes it possible to attach listeners to the dragenter,
    // dragleave, and drop events of a single element with children. It's not
    // intuitive to do directly because outer elements fire "dragleave" events
    // both when the drag leaves the element and when the drag enters a child. To
    // make it easier, we maintain a count of the elements being dragged across
    // and trigger 3 new types of event:
    //
    // 1. draghover:enter - When a drag enters el and any of its children.
    // 2. draghover:leave - When the drag leaves el and all of its children.
    // 3. draghover:drop - When an item is dropped on el or any of its children.
    _enableDraghover: function (el) {
      let $el = $(el),
        childCounter = 0;

      $el.on({
        "dragenter.draghover": (e) => {
          if (childCounter++ === 0) {
            $el.trigger("draghover:enter", e);
          }
        },
        "dragleave.draghover": (e) => {
          if (--childCounter === 0) {
            $el.trigger("draghover:leave", e);
          }
          if (childCounter < 0) {
            console.error("draghover childCounter is negative somehow");
          }
        },
        "dragover.draghover": (e) => {
          e.preventDefault();
        },
        "drop.draghover": (e) => {
          childCounter = 0;
          $el.trigger("draghover:drop", e);
          e.preventDefault();
        },
      });
      return $el;
    },
    _disableDraghover: function (el) {
      return $(el).off(".draghover");
    },
    _ZoneClass: {
      ACTIVE: "shiny-file-input-active",
      OVER: "shiny-file-input-over",
    },
    _enableDocumentEvents: function () {
      let $doc = $("html"),
        { ACTIVE, OVER } = this._ZoneClass;

      this._enableDraghover($doc).on({
        "draghover:enter.draghover": (e) => {
          this._zoneOf($fileInputs).addClass(ACTIVE);
        },
        "draghover:leave.draghover": (e) => {
          this._zoneOf($fileInputs).removeClass(ACTIVE);
        },
        "draghover:drop.draghover": (e) => {
          this._zoneOf($fileInputs).removeClass(OVER).removeClass(ACTIVE);
        },
      });
    },
    _disableDocumentEvents: function () {
      const $doc = $("html");

      $doc.off(".draghover");
      this._disableDraghover($doc);
    },
    _canSetFiles: function (fileList) {
      const testEl = document.createElement("input");

      testEl.type = "file";
      try {
        testEl.files = fileList;
      } catch (e) {
        return false;
      }
      return true;
    },
    _handleDrop: function (e, el) {
      const files = e.originalEvent.dataTransfer.files,
        $el = $(el);

      if (files === undefined || files === null) {
        // 1. The FileList object isn't supported by this browser, and
        // there's nothing else we can try. (< IE 10)
        console.log(
          "Dropping files is not supported on this browser. (no FileList)"
        );
      } else if (!this._canSetFiles(files)) {
        // 2. The browser doesn't support assigning a type=file input's .files
        // property, but we do have a FileList to work with. (IE10+/Edge)
        $el.val("");
        uploadDroppedFilesIE10Plus(el, files);
      } else {
        // 3. The browser supports FileList and input.files assignment.
        // (Chrome, Safari)
        $el.val("");
        el.files = e.originalEvent.dataTransfer.files;
        // Recent versions of Firefox (57+, or "Quantum" and beyond) don't seem to
        // automatically trigger a change event, so we trigger one manually here.
        // On browsers that do trigger change, this operation appears to be
        // idempotent, as el.files doesn't change between events.
        $el.trigger("change");
      }
    },
    subscribe: function (el, callback) {
      $(el).on("change.fileInputBinding", uploadFiles);
      // Here we try to set up the necessary events for Drag and Drop ("DnD").
      if ($fileInputs.length === 0) this._enableDocumentEvents();
      $fileInputs = $fileInputs.add(el);
      const $zone = this._zoneOf(el),
        { OVER } = this._ZoneClass;

      this._enableDraghover($zone).on({
        "draghover:enter.draghover": (e) => {
          $zone.addClass(OVER);
        },
        "draghover:leave.draghover": (e) => {
          $zone.removeClass(OVER);
          // Prevent this event from bubbling to the document handler,
          // which would deactivate all zones.
          e.stopPropagation();
        },
        "draghover:drop.draghover": (e, dropEvent) => {
          this._handleDrop(dropEvent, el);
        },
      });
    },

    unsubscribe: function (el) {
      const $el = $(el),
        $zone = this._zoneOf(el);

      $zone
        .removeClass(this._ZoneClass.OVER)
        .removeClass(this._ZoneClass.ACTIVE);

      this._disableDraghover($zone);
      $el.off(".fileInputBinding");
      $zone.off(".draghover");

      // Remove el from list of inputs and (maybe) clean up global event handlers.
      $fileInputs = $fileInputs.not(el);
      if ($fileInputs.length === 0) this._disableDocumentEvents();
    },
  });
  inputBindings.register(fileInputBinding, "shiny.fileInputBinding");

  // "init_shiny.js"
  function initShiny() {
    const shinyapp = (Shiny.shinyapp = new ShinyApp());

    function bindOutputs(scope = document) {
      scope = $(scope);

      const bindings = outputBindings.getBindings();

      for (let i = 0; i < bindings.length; i++) {
        const binding = bindings[i].binding;
        const matches = binding.find(scope) || [];

        for (let j = 0; j < matches.length; j++) {
          const el = matches[j];
          const id = binding.getId(el);

          // Check if ID is falsy
          if (!id) continue;

          // In some uncommon cases, elements that are later in the
          // matches array can be removed from the document by earlier
          // iterations. See https://github.com/rstudio/shiny/issues/1399
          if (!$.contains(document, el)) continue;

          const $el = $(el);

          if ($el.hasClass("shiny-bound-output")) {
            // Already bound; can happen with nested uiOutput (bindAll
            // gets called on two ancestors)
            continue;
          }

          // If this element reports its CSS styles to getCurrentOutputInfo()
          // then it should have a MutationObserver() to resend CSS if its
          // style/class attributes change. This observer should already exist
          // for _static_ UI, but not yet for _dynamic_ UI
          maybeAddThemeObserver(el);

          const bindingAdapter = new OutputBindingAdapter(el, binding);

          shinyapp.bindOutput(id, bindingAdapter);
          $el.data("shiny-output-binding", bindingAdapter);
          $el.addClass("shiny-bound-output");
          if (!$el.attr("aria-live")) $el.attr("aria-live", "polite");
          $el.trigger({
            type: "shiny:bound",
            binding: binding,
            bindingType: "output",
          });
        }
      }

      // Send later in case DOM layout isn't final yet.
      setTimeout(sendImageSize, 0);
      setTimeout(sendOutputHiddenState, 0);
    }

    function unbindOutputs(scope = document, includeSelf = false) {
      const outputs = $(scope).find(".shiny-bound-output");

      if (includeSelf && $(scope).hasClass("shiny-bound-output")) {
        outputs.push(scope);
      }

      for (let i = 0; i < outputs.length; i++) {
        const $el = $(outputs[i]);
        const bindingAdapter = $el.data("shiny-output-binding");

        if (!bindingAdapter) continue;
        const id = bindingAdapter.binding.getId(outputs[i]);

        shinyapp.unbindOutput(id, bindingAdapter);
        $el.removeClass("shiny-bound-output");
        $el.removeData("shiny-output-binding");
        $el.trigger({
          type: "shiny:unbound",
          binding: bindingAdapter.binding,
          bindingType: "output",
        });
      }

      // Send later in case DOM layout isn't final yet.
      setTimeout(sendImageSize, 0);
      setTimeout(sendOutputHiddenState, 0);
    }

    const inputBatchSender = new InputBatchSender(shinyapp);
    const inputsNoResend = new InputNoResendDecorator(inputBatchSender);
    const inputsEvent = new InputEventDecorator(inputsNoResend);
    const inputsRate = new InputRateDecorator(inputsEvent);
    const inputsDefer = new InputDeferDecorator(inputsEvent);

    let inputs;

    if ($('input[type="submit"], button[type="submit"]').length > 0) {
      // If there is a submit button on the page, use defer decorator
      inputs = inputsDefer;

      $('input[type="submit"], button[type="submit"]').each(function () {
        $(this).click(function (event) {
          event.preventDefault();
          inputsDefer.submit();
        });
      });
    } else {
      // By default, use rate decorator
      inputs = inputsRate;
    }

    inputs = new InputValidateDecorator(inputs);

    Shiny.setInputValue = Shiny.onInputChange = function (name, value, opts) {
      opts = addDefaultInputOpts(opts);
      inputs.setInput(name, value, opts);
    };

    // By default, Shiny deduplicates input value changes; that is, if
    // `setInputValue` is called with the same value as the input already
    // has, the call is ignored (unless opts.priority = "event"). Calling
    // `forgetLastInputValue` tells Shiny that the very next call to
    // `setInputValue` for this input id shouldn't be ignored, even if it
    // is a dupe of the existing value.
    Shiny.forgetLastInputValue = function (name) {
      inputsNoResend.forget(name);
    };

    const boundInputs = {};

    function valueChangeCallback(binding, el, allowDeferred) {
      let id = binding.getId(el);

      if (id) {
        const value = binding.getValue(el);
        const type = binding.getType(el);

        if (type) id = id + ":" + type;

        const opts = {
          priority: allowDeferred ? "deferred" : "immediate",
          binding: binding,
          el: el,
        };

        inputs.setInput(id, value, opts);
      }
    }

    function bindInputs(scope = document) {
      const bindings = inputBindings.getBindings();

      const inputItems = {};

      for (let i = 0; i < bindings.length; i++) {
        const binding = bindings[i].binding;
        const matches = binding.find(scope) || [];

        for (let j = 0; j < matches.length; j++) {
          const el = matches[j];
          const id = binding.getId(el);

          // Check if ID is falsy, or if already bound
          if (!id || boundInputs[id]) continue;

          const type = binding.getType(el);
          const effectiveId = type ? id + ":" + type : id;

          inputItems[effectiveId] = {
            value: binding.getValue(el),
            opts: {
              immediate: true,
              binding: binding,
              el: el,
            },
          };

          /*jshint loopfunc:true*/
          const thisCallback = (function () {
            const thisBinding = binding;
            const thisEl = el;

            return function (allowDeferred) {
              valueChangeCallback(thisBinding, thisEl, allowDeferred);
            };
          })();

          binding.subscribe(el, thisCallback);
          $(el).data("shiny-input-binding", binding);
          $(el).addClass("shiny-bound-input");
          const ratePolicy = binding.getRatePolicy(el);

          if (ratePolicy !== null) {
            inputsRate.setRatePolicy(
              effectiveId,
              ratePolicy.policy,
              ratePolicy.delay
            );
          }

          boundInputs[id] = {
            binding: binding,
            node: el,
          };

          $(el).trigger({
            type: "shiny:bound",
            binding: binding,
            bindingType: "input",
          });
        }
      }

      return inputItems;
    }

    function unbindInputs(scope = document, includeSelf = false) {
      const inputs = $(scope).find(".shiny-bound-input");

      if (includeSelf && $(scope).hasClass("shiny-bound-input")) {
        inputs.push(scope);
      }

      for (let i = 0; i < inputs.length; i++) {
        const el = inputs[i];
        const binding = $(el).data("shiny-input-binding");

        if (!binding) continue;
        const id = binding.getId(el);

        $(el).removeClass("shiny-bound-input");
        delete boundInputs[id];
        binding.unsubscribe(el);
        $(el).trigger({
          type: "shiny:unbound",
          binding: binding,
          bindingType: "input",
        });
      }
    }

    function _bindAll(scope) {
      bindOutputs(scope);
      return bindInputs(scope);
    }
    function unbindAll(scope, includeSelf = false) {
      unbindInputs(scope, includeSelf);
      unbindOutputs(scope, includeSelf);
    }
    Shiny.bindAll = function (scope) {
      // _bindAll returns input values; it doesn't send them to the server.
      // export.bindAll needs to send the values to the server.
      const currentInputItems = _bindAll(scope);

      $.each(currentInputItems, function (name, item) {
        inputs.setInput(name, item.value, item.opts);
      });

      // Not sure if the iframe stuff is an intrinsic part of bindAll, but bindAll
      // is a convenient place to hang it. bindAll will be called anytime new HTML
      // appears that might contain inputs/outputs; it's reasonable to assume that
      // any such HTML may contain iframes as well.
      initDeferredIframes();
    };
    Shiny.unbindAll = unbindAll;

    // Calls .initialize() for all of the input objects in all input bindings,
    // in the given scope.
    function initializeInputs(scope = document) {
      const bindings = inputBindings.getBindings();

      // Iterate over all bindings
      for (let i = 0; i < bindings.length; i++) {
        const binding = bindings[i].binding;
        const inputObjects = binding.find(scope) || [];

        // Iterate over all input objects for this binding
        for (let j = 0; j < inputObjects.length; j++) {
          if (!inputObjects[j]._shiny_initialized) {
            inputObjects[j]._shiny_initialized = true;
            binding.initialize(inputObjects[j]);
          }
        }
      }
    }
    Shiny.initializeInputs = initializeInputs;

    function getIdFromEl(el) {
      const $el = $(el);
      const bindingAdapter = $el.data("shiny-output-binding");

      if (!bindingAdapter) return null;
      else return bindingAdapter.getId();
    }

    // Initialize all input objects in the document, before binding
    initializeInputs(document);

    // The input values returned by _bindAll() each have a structure like this:
    //   { value: 123, opts: { ... } }
    // We want to only keep the value. This is because when the initialValues is
    // passed to ShinyApp.connect(), the ShinyApp object stores the
    // initialValues object for the duration of the session, and the opts may
    // have a reference to the DOM element, which would prevent it from being
    // GC'd.
    const initialValues = mapValues(_bindAll(document), (x) => x.value);

    // The server needs to know the size of each image and plot output element,
    // in case it is auto-sizing
    $(".shiny-image-output, .shiny-plot-output, .shiny-report-size").each(
      function () {
        const id = getIdFromEl(this);

        if (this.offsetWidth !== 0 || this.offsetHeight !== 0) {
          initialValues[
            ".clientdata_output_" + id + "_width"
          ] = this.offsetWidth;
          initialValues[
            ".clientdata_output_" + id + "_height"
          ] = this.offsetHeight;
        }
      }
    );

    function getComputedBgColor(el) {
      if (!el) {
        // Top of document, can't recurse further
        return null;
      }

      const bgColor = getStyle(el, "background-color");
      const m = bgColor.match(
        /^rgba\(\s*([\d.]+)\s*,\s*([\d.]+)\s*,\s*([\d.]+)\s*,\s*([\d.]+)\s*\)$/
      );

      if (bgColor === "transparent" || (m && parseFloat(m[4]) === 0)) {
        // No background color on this element. See if it has a background image.
        const bgImage = getStyle(el, "background-image");

        if (bgImage && bgImage !== "none") {
          // Failed to detect background color, since it has a background image
          return null;
        } else {
          // Recurse
          return getComputedBgColor(el.parentElement);
        }
      }
      return bgColor;
    }

    function getComputedFont(el) {
      const fontFamily = getStyle(el, "font-family");
      const fontSize = getStyle(el, "font-size");

      return {
        families: fontFamily.replace(/"/g, "").split(", "),
        size: fontSize,
      };
    }

    $(".shiny-image-output, .shiny-plot-output, .shiny-report-theme").each(
      function () {
        const el = this,
          id = getIdFromEl(el);

        initialValues[".clientdata_output_" + id + "_bg"] = getComputedBgColor(
          el
        );
        initialValues[".clientdata_output_" + id + "_fg"] = getStyle(
          el,
          "color"
        );
        initialValues[
          ".clientdata_output_" + id + "_accent"
        ] = getComputedLinkColor(el);
        initialValues[".clientdata_output_" + id + "_font"] = getComputedFont(
          el
        );
        maybeAddThemeObserver(el);
      }
    );

    // Resend computed styles if *an output element's* class or style attribute changes.
    // This gives us some level of confidence that getCurrentOutputInfo() will be
    // properly invalidated if output container is mutated; but unfortunately,
    // we don't have a reasonable way to detect change in *inherited* styles
    // (other than session$setCurrentTheme())
    // https://github.com/rstudio/shiny/issues/3196
    // https://github.com/rstudio/shiny/issues/2998
    function maybeAddThemeObserver(el) {
      if (!window.MutationObserver) {
        return; // IE10 and lower
      }

      const cl = el.classList;
      const reportTheme =
        cl.contains("shiny-image-output") ||
        cl.contains("shiny-plot-output") ||
        cl.contains("shiny-report-theme");

      if (!reportTheme) {
        return;
      }

      const $el = $(el);

      if ($el.data("shiny-theme-observer")) {
        return; // i.e., observer is already observing
      }

      const observerCallback = new Debouncer(null, () => doSendTheme(el), 100);
      const observer = new MutationObserver(() =>
        observerCallback.normalCall()
      );
      const config = { attributes: true, attributeFilter: ["style", "class"] };

      observer.observe(el, config);
      $el.data("shiny-theme-observer", observer);
    }

    function doSendTheme(el) {
      // Sending theme info on error isn't necessary (it'd add an unnecessary additional round-trip)
      if (el.classList.contains("shiny-output-error")) {
        return;
      }
      const id = getIdFromEl(el);

      inputs.setInput(
        ".clientdata_output_" + id + "_bg",
        getComputedBgColor(el)
      );
      inputs.setInput(
        ".clientdata_output_" + id + "_fg",
        getStyle(el, "color")
      );
      inputs.setInput(
        ".clientdata_output_" + id + "_accent",
        getComputedLinkColor(el)
      );
      inputs.setInput(
        ".clientdata_output_" + id + "_font",
        getComputedFont(el)
      );
    }

    function doSendImageSize() {
      $(".shiny-image-output, .shiny-plot-output, .shiny-report-size").each(
        function () {
          const id = getIdFromEl(this);

          if (this.offsetWidth !== 0 || this.offsetHeight !== 0) {
            inputs.setInput(
              ".clientdata_output_" + id + "_width",
              this.offsetWidth
            );
            inputs.setInput(
              ".clientdata_output_" + id + "_height",
              this.offsetHeight
            );
          }
        }
      );

      $(".shiny-image-output, .shiny-plot-output, .shiny-report-theme").each(
        function () {
          doSendTheme(this);
        }
      );

      $(".shiny-bound-output").each(function () {
        const $this = $(this),
          binding = $this.data("shiny-output-binding");

        $this.trigger({
          type: "shiny:visualchange",
          visible: !isHidden(this),
          binding: binding,
        });
        binding.onResize();
      });
    }
    const sendImageSizeDebouncer = new Debouncer(null, doSendImageSize, 0);

    function sendImageSize() {
      sendImageSizeDebouncer.normalCall();
    }
    // Make sure sendImageSize actually gets called before the inputBatchSender
    // sends data to the server.
    inputBatchSender.lastChanceCallback.push(function () {
      if (sendImageSizeDebouncer.isPending())
        sendImageSizeDebouncer.immediateCall();
    });

    // Return true if the object or one of its ancestors in the DOM tree has
    // style='display:none'; otherwise return false.
    function isHidden(obj) {
      // null means we've hit the top of the tree. If width or height is
      // non-zero, then we know that no ancestor has display:none.
      if (obj === null || obj.offsetWidth !== 0 || obj.offsetHeight !== 0) {
        return false;
      } else if (getStyle(obj, "display") === "none") {
        return true;
      } else {
        return isHidden(obj.parentNode);
      }
    }
    let lastKnownVisibleOutputs = {};
    // Set initial state of outputs to hidden, if needed

    $(".shiny-bound-output").each(function () {
      const id = getIdFromEl(this);

      if (isHidden(this)) {
        initialValues[".clientdata_output_" + id + "_hidden"] = true;
      } else {
        lastKnownVisibleOutputs[id] = true;
        initialValues[".clientdata_output_" + id + "_hidden"] = false;
      }
    });
    // Send update when hidden state changes
    function doSendOutputHiddenState() {
      const visibleOutputs = {};

      $(".shiny-bound-output").each(function () {
        const id = getIdFromEl(this);

        delete lastKnownVisibleOutputs[id];
        // Assume that the object is hidden when width and height are 0
        const hidden = isHidden(this),
          evt = {
            type: "shiny:visualchange",
            visible: !hidden,
          };

        if (hidden) {
          inputs.setInput(".clientdata_output_" + id + "_hidden", true);
        } else {
          visibleOutputs[id] = true;
          inputs.setInput(".clientdata_output_" + id + "_hidden", false);
        }
        const $this = $(this);

        evt.binding = $this.data("shiny-output-binding");
        $this.trigger(evt);
      });
      // Anything left in lastKnownVisibleOutputs is orphaned
      for (const name in lastKnownVisibleOutputs) {
        if (lastKnownVisibleOutputs.hasOwnProperty(name))
          inputs.setInput(".clientdata_output_" + name + "_hidden", true);
      }
      // Update the visible outputs for next time
      lastKnownVisibleOutputs = visibleOutputs;
    }
    // sendOutputHiddenState gets called each time DOM elements are shown or
    // hidden. This can be in the hundreds or thousands of times at startup.
    // We'll debounce it, so that we do the actual work once per tick.
    const sendOutputHiddenStateDebouncer = new Debouncer(
      null,
      doSendOutputHiddenState,
      0
    );

    function sendOutputHiddenState() {
      sendOutputHiddenStateDebouncer.normalCall();
    }
    // We need to make sure doSendOutputHiddenState actually gets called before
    // the inputBatchSender sends data to the server. The lastChanceCallback
    // here does that - if the debouncer has a pending call, flush it.
    inputBatchSender.lastChanceCallback.push(function () {
      if (sendOutputHiddenStateDebouncer.isPending())
        sendOutputHiddenStateDebouncer.immediateCall();
    });

    // Given a namespace and a handler function, return a function that invokes
    // the handler only when e's namespace matches. For example, if the
    // namespace is "bs", it would match when e.namespace is "bs" or "bs.tab".
    // If the namespace is "bs.tab", it would match for "bs.tab", but not "bs".
    function filterEventsByNamespace(namespace, handler, ...args) {
      namespace = namespace.split(".");

      return function (e) {
        const eventNamespace = e.namespace.split(".");

        // If any of the namespace strings aren't present in this event, quit.
        for (let i = 0; i < namespace.length; i++) {
          if (eventNamespace.indexOf(namespace[i]) === -1) return;
        }

        handler.apply(this, [namespace, handler, ...args]);
      };
    }

    // The size of each image may change either because the browser window was
    // resized, or because a tab was shown/hidden (hidden elements report size
    // of 0x0). It's OK to over-report sizes because the input pipeline will
    // filter out values that haven't changed.
    $(window).resize(debounce(500, sendImageSize));
    // Need to register callbacks for each Bootstrap 3 class.
    const bs3classes = [
      "modal",
      "dropdown",
      "tab",
      "tooltip",
      "popover",
      "collapse",
    ];

    $.each(bs3classes, function (idx, classname) {
      $(document.body).on(
        "shown.bs." + classname + ".sendImageSize",
        "*",
        filterEventsByNamespace("bs", sendImageSize)
      );
      $(document.body).on(
        "shown.bs." +
          classname +
          ".sendOutputHiddenState " +
          "hidden.bs." +
          classname +
          ".sendOutputHiddenState",
        "*",
        filterEventsByNamespace("bs", sendOutputHiddenState)
      );
    });

    // This is needed for Bootstrap 2 compatibility and for non-Bootstrap
    // related shown/hidden events (like conditionalPanel)
    $(document.body).on("shown.sendImageSize", "*", sendImageSize);
    $(document.body).on(
      "shown.sendOutputHiddenState hidden.sendOutputHiddenState",
      "*",
      sendOutputHiddenState
    );

    // Send initial pixel ratio, and update it if it changes
    initialValues[".clientdata_pixelratio"] = pixelRatio();
    $(window).resize(function () {
      inputs.setInput(".clientdata_pixelratio", pixelRatio());
    });

    // Send initial URL
    initialValues[".clientdata_url_protocol"] = window.location.protocol;
    initialValues[".clientdata_url_hostname"] = window.location.hostname;
    initialValues[".clientdata_url_port"] = window.location.port;
    initialValues[".clientdata_url_pathname"] = window.location.pathname;

    // Send initial URL search (query string) and update it if it changes
    initialValues[".clientdata_url_search"] = window.location.search;

    $(window).on("pushstate", function (e) {
      inputs.setInput(".clientdata_url_search", window.location.search);
    });

    $(window).on("popstate", function (e) {
      inputs.setInput(".clientdata_url_search", window.location.search);
    });

    // This is only the initial value of the hash. The hash can change, but
    // a reactive version of this isn't sent because watching for changes can
    // require polling on some browsers. The JQuery hashchange plugin can be
    // used if this capability is important.
    initialValues[".clientdata_url_hash_initial"] = window.location.hash;
    initialValues[".clientdata_url_hash"] = window.location.hash;

    $(window).on("hashchange", function (e) {
      inputs.setInput(".clientdata_url_hash", window.location.hash);
    });

    // The server needs to know what singletons were rendered as part of
    // the page loading
    const singletonText = (initialValues[".clientdata_singletons"] = $(
      'script[type="application/shiny-singletons"]'
    ).text());

    singletons.registerNames(singletonText.split(/,/));

    const dependencyText = $(
      'script[type="application/html-dependencies"]'
    ).text();

    $.each(dependencyText.split(/;/), function (i, depStr) {
      const match = /\s*^(.+)\[(.+)\]\s*$/.exec(depStr);

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
    if (
      !window.Shiny ||
      !window.Shiny.shinyapp ||
      !window.Shiny.shinyapp.isConnected()
    ) {
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
      const $el = $(el);

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

  // "reactlog.js"
  $(document).on("keydown", function (e) {
    if (e.which !== 114 || (!e.ctrlKey && !e.metaKey) || e.shiftKey || e.altKey)
      return;
    const url =
      "reactlog?w=" +
      window.escape(Shiny.shinyapp.config.workerId) +
      "&s=" +
      window.escape(Shiny.shinyapp.config.sessionId);

    window.open(url);
    e.preventDefault();
  });

  $(document).on("keydown", function (e) {
    if (
      // if not one of the key combos below
      !(
        // cmd/ctrl + fn + f4
        (
          (e.which === 115 &&
            (e.ctrlKey || e.metaKey) &&
            !e.shiftKey &&
            !e.altKey) ||
          // cmd/ctrl + shift + fn + f3
          (e.which === 114 &&
            (e.ctrlKey || e.metaKey) &&
            e.shiftKey &&
            !e.altKey)
        )
      )
    ) {
      return;
    }

    const url =
      "reactlog/mark?w=" +
      window.escape(Shiny.shinyapp.config.workerId) +
      "&s=" +
      window.escape(Shiny.shinyapp.config.sessionId);

    // send notification
    $.get(url, function (result) {
      if (result !== "marked") return;

      const html =
        '<span id="shiny-reactlog-mark-text">Marked time point in reactlog</span>';

      Shiny.notifications.show({
        html: html,
        closeButton: true,
      });
    }).fail(function () {
      // found returned error while marking, should open webpage
      window.open(url);
    });

    e.preventDefault();
  });

  // "_end.js"
  // √
}

export { main };
