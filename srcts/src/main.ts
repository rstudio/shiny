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
  isBS3,
} from "./utils";

import { isQt, isIE, IEVersion } from "./utils/browser";

import { FileProcessor, FileUploader } from "./file/FileProcessor";

import { Shiny } from "./shiny";

import { inputBindings, outputBindings } from "./bindings";

function main(): void {
  // "_start.js"
  // √

  // "utils.js"
  // √

  // "browser.js"
  // √

  // "input_rate.js"
  // √; ./time/invoke.ts
  // √; ./time/debounce.ts
  // √; ./time/throttle.ts

  // √; ./shinyapp/inputBatchSender.ts
  // √; ./shinyapp/inputNoResendDecorator.ts
  // √; ./shinyapp/inputEventDecorator.ts
  // √; ./shinyapp/inputRateDecorator.ts
  // √; ./shinyapp/inputDeferDecorator.ts
  // √; ./shinyapp/inputValidateDecorator.ts

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
      let $liTag = $aTag.parent();

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

        $tabset.find("a[data-toggle='tab']").each(function () {
          const $tab = $(this);

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
      const inputBinding = $tabset.data("shiny-input-binding");

      // Use the getValue() method to avoid duplicating the CSS selector
      // for querying the DOM for the currently active tab
      if (!inputBinding.getValue($tabset)) {
        // Note: destTabValue may be null. We still want to proceed
        // through the below logic and setValue so that the input
        // value for the tabset gets updated (i.e. input$tabsetId
        // should be null if there are no tabs).
        const destTabValue = getFirstTab($tabset);
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
  // √; shiny/notifications.ts

  // "modal.js"
  // √; shiny/modal.ts

  // "file_processor.js"
  // √

  // "binding_registry.js"
  // √; ./bindings/registry.ts

  // √; ./bindings/input/index.ts
  // const inputBindings = (Shiny.inputBindings = new BindingRegistry());

  // √; ./bindings/output/index.ts
  // const outputBindings = (Shiny.outputBindings = new BindingRegistry());

  // "output_binding.js"
  // √; ./bindings/output/index.ts

  // "output_binding_text.js"
  // √; ./bindings/output/text.ts

  // "output_binding_image.js"
  // √; ./bindings/output/image.ts

  // √; ./imageutils/index.ts
  // √; ./imageutils/disableDrag.ts
  // √; ./imageutils/initPanelScales.ts
  // √; ./imageutils/initCoordmap.ts
  // √; ./imageutils/findbox.ts
  // √; ./imageutils/shiftToRange.ts
  // √; ./imageutils/createClickInfo.ts
  // √; ./imageutils/createHandlers.ts
  // √; ./imageutils/createBrush.ts
  // √; ./imageutils/resetBrush.ts

  // "output_binding_html.js"
  // √; ./bindings/output/html.ts

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

            // We can remove the old styles immediately because the new styles
            // should have been applied synchronously.
            oldStyle.remove();
            removeSheet(oldSheet);
            sendImageSize2();
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

        // Removes the stylesheet from document.styleSheets, and also removes
        // the owning <link> element, if present.
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

            // This part is a bit tricky. The link's onload callback will be
            // invoked after the file is loaded, but it can be _before_ the
            // styles are actually applied. The amount of time it takes for the
            // style to be applied is not predictable. We need to make sure the
            // styles are applied before we send updated size/style information
            // to the server.
            //
            // We do this by adding _another_ link, with CSS content
            // base64-encoded and inlined into the href. We also add a dummy DOM
            // element that the CSS applies to. The dummy CSS includes a
            // transition, and when the `transitionend` event happens, we call
            // sendImageSize2() and remove the old sheet. We also remove the
            // dummy DOM element and dummy CSS content.
            //
            // The reason this works is because (we assume) that if multiple
            // <link> tags are added, they will be applied in the same order
            // that they are loaded. This seems to be true in the browsers we
            // have tested.
            //
            // Because it is common for multiple stylesheets to arrive close
            // together, but not on exactly the same tick, we call
            // sendImageSize2(), which is debounced. Otherwise, it can result in
            // the same plot being redrawn multiple times with different
            // styling.
            link.attr("onload", () => {
              const dummy_id = "dummy-" + Math.floor(Math.random() * 999999999);
              const css_string =
                "#" +
                dummy_id +
                " { " +
                "color: #a7c920 !important; " + // An arbitrary color for the transition
                "transition: 0.1s all !important; " +
                "visibility: hidden !important; " +
                "position: absolute !important; " +
                "top: -1000px !important; " +
                "left: 0 !important; }";
              const base64_css_string =
                "data:text/css;base64," + btoa(css_string);

              let $dummy_link = $("<link rel='stylesheet' type='text/css' />");

              $dummy_link.attr("href", base64_css_string);

              let $dummy_el = $("<div id='" + dummy_id + "'></div>");

              $dummy_el.one("transitionend", () => {
                $dummy_el.remove();
                removeSheet(findSheet($dummy_link.attr("href")));
                removeSheet(oldSheet);
                sendImageSize2();
              });
              $(document.body).append($dummy_el);

              // Need to add the CSS with a setTimeout 0, to ensure that it
              // takes effect _after_ the DOM element has been added. This is
              // necessary to ensure that the transition actually occurs.
              setTimeout(() => $head.append($dummy_link), 0);
            });

            $head.append(link);
          }
        });
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
  // √; ./bindings/output/downloadlink.ts

  // "output_binding_datatable.js"
  // √; ./bindings/output/datatable.ts

  // "output_binding_adapter.js"
  // √; ./bindings/output_adapter.ts

  // "input_binding.js"
  // √; ./bindings/input/index.ts

  // "input_binding_text.js"
  // √; ./bindings/input/text.ts

  // "input_binding_textarea.js"
  // √; ./bindings/input/textarea.ts

  // "input_binding_password.js"
  // √; ./bindings/input/password.ts

  // "input_binding_number.js"
  // √; ./bindings/input/number.ts

  // "input_binding_checkbox.js"
  // √; ./bindings/input/checkbox.ts

  // "input_binding_slider.js"
  // √; ./bindings/input/slider.ts

  // "input_binding_date.js"
  // √; ./bindings/input/date.ts

  // "input_binding_daterange.js"
  // √; ./bindings/input/daterange.ts

  // "input_binding_select.js"

  // "input_binding_radio.js"
  // √; ./bindings/input/radio.ts

  // "input_binding_checkboxgroup.js"
  // √; ./bindings/input/checkboxgroup.ts

  // "input_binding_actionbutton.js"
  // √; ./bindings/input/actionbutton.ts

  // "input_binding_tabinput.js"
  // √; ./bindings/input/tabinput.ts

  // "input_binding_fileinput.js"
  // √; ./bindings/input/fileinput.ts

  // This function gets defined in initShiny() and 'hoisted' so it can be reused
  // (to send CSS info) inside of Shiny.renderDependencies()
  let sendImageSize;
  let sendImageSize2;

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

    // todo make sure allowDeferred can NOT be supplied and still work
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

    sendImageSize = function () {
      sendImageSizeDebouncer.normalCall();
    };
    // Make sure sendImageSize actually gets called before the inputBatchSender
    // sends data to the server.
    inputBatchSender.lastChanceCallback.push(function () {
      if (sendImageSizeDebouncer.isPending())
        sendImageSizeDebouncer.immediateCall();
    });

    // A version of sendImageSize which debounces for longer.
    sendImageSize2 = debounce(200, sendImageSize);

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
  // √; ./shiny/reactlog.ts

  // "_end.js"
  // √
}

export { main };
