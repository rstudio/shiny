import $ from "jquery";
import type { InputBinding } from "../bindings";
import type { OutputBindingAdapter } from "../bindings/outputAdapter";
import { showErrorInClientConsole } from "../components/errorConsole";
import type {
  ShinyEventError,
  ShinyEventMessage,
  ShinyEventUpdateInput,
  ShinyEventValue,
} from "../events/shinyEvents";
import type { UploadEndValue, UploadInitValue } from "../file/fileProcessor";
import { resetBrush } from "../imageutils/resetBrush";
import { $escape, hasOwnProperty, randomId, scopeExprToFunc } from "../utils";
import { AsyncQueue } from "../utils/asyncQueue";
import { isQt } from "../utils/browser";
import { indirectEval } from "../utils/eval";
import {
  getShinyCreateWebsocket,
  getShinyOnCustomMessage,
  setShinyUser,
  shinyForgetLastInputValue,
  shinyUnbindAll,
} from "./initedMethods";
import { removeModal, showModal } from "./modal";
import { removeNotification, showNotification } from "./notifications";
import { hideReconnectDialog, showReconnectDialog } from "./reconnectDialog";
import type { HtmlDep } from "./render";
import { renderContentAsync, renderHtmlAsync } from "./render";
import type { WherePosition } from "./singletons";

import { OutputProgressReporter } from "./outputProgress";

type ResponseValue = UploadEndValue | UploadInitValue;
type Handler = (message: any) => Promise<void> | void;

type ShinyWebSocket = WebSocket & {
  allowReconnect?: boolean;
};

type ErrorsMessageValue = {
  message: string;
  call: string[];
  type?: string[];
};

type OnSuccessRequest = (value: ResponseValue) => void;
type OnErrorRequest = (err: string) => void;
type InputValues = { [key: string]: unknown };

type MessageValue = Parameters<WebSocket["send"]>[0];

//// 2021/03 - TypeScript conversion note:
// These four variables were moved from being internally defined to being defined globally within the file.
// Before the TypeScript conversion, the values where attached to `window.Shiny.addCustomMessageHandler()`.
// This prevents multiple instances of `ShinyApp` from existing independently. :-/
// This behavior is also exhibited on `Shiny.progressHandlers`, however there are no instances of use on GitHub. So moving the assignment to within `initShiny()`.

// Records insertion order of handlers. Maps number to name. This is so
// we can dispatch messages to handlers in the order that handlers were
// added.
const messageHandlerOrder: string[] = [];
// Keep track of handlers by name. Maps name to handler function.
const messageHandlers: { [key: string]: Handler } = {};

// Two categories of message handlers: those that are from Shiny, and those
// that are added by the user. The Shiny ones handle messages in
// msgObj.values, msgObj.errors, and so on. The user ones handle messages
// in msgObj.custom.foo and msgObj.custom.bar.
const customMessageHandlerOrder: string[] = [];
const customMessageHandlers: { [key: string]: Handler } = {};

// Adds Shiny (internal) message handler
function addMessageHandler(type: string, handler: Handler) {
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
function addCustomMessageHandler(type: string, handler: Handler): void {
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

//// End message handler variables

class ShinyApp {
  $socket: ShinyWebSocket | null = null;

  // An asynchronous queue of functions. This is sort of like an event loop for
  // Shiny, to allow scheduling async callbacks so that they can run in order
  // without overlapping. This is used for handling incoming messages from the
  // server and scheduling outgoing messages to the server, and can be used for
  // other things tasks as well.
  taskQueue = new AsyncQueue<() => Promise<void> | void>();

  config: {
    workerId: string;
    sessionId: string;
  } | null = null;

  // Cached input values
  $inputValues: InputValues = {};

  // Input values at initialization (and reconnect)
  $initialInput: InputValues | null = null;

  // Output bindings
  $bindings: { [key: string]: OutputBindingAdapter } = {};

  // Output progress states
  $outputProgress = new OutputProgressReporter();

  // Cached values/errors
  $values: { [key: string]: any } = {};
  $errors: { [key: string]: ErrorsMessageValue } = {};

  // Conditional bindings (show/hide element based on expression)
  $conditionals = {};

  $pendingMessages: MessageValue[] = [];
  $activeRequests: {
    [key: number]: { onSuccess: OnSuccessRequest; onError: OnErrorRequest };
  } = {};
  $nextRequestId = 0;

  $allowReconnect: boolean | "force" = false;

  constructor() {
    this._init();
  }

  connect(initialInput: InputValues): void {
    if (this.$socket)
      throw "Connect was already called on this application object";

    this.$socket = this.createSocket();
    this.$initialInput = initialInput;
    $.extend(this.$inputValues, initialInput);

    this.$updateConditionals();
  }

  isConnected(): boolean {
    return !!this.$socket;
  }

  private scheduledReconnect: number | undefined = undefined;

  reconnect(): void {
    // This function can be invoked directly even if there's a scheduled
    // reconnect, so be sure to clear any such scheduled reconnects.
    clearTimeout(this.scheduledReconnect);

    if (this.isConnected())
      throw "Attempted to reconnect, but already connected.";

    this.$socket = this.createSocket();
    this.$initialInput = $.extend({}, this.$inputValues);
    this.$updateConditionals();
  }

  createSocket(): ShinyWebSocket {
    const createSocketFunc: () => ShinyWebSocket =
      getShinyCreateWebsocket() ||
      (() => {
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

        const ws: ShinyWebSocket = new WebSocket(
          protocol + "//" + window.location.host + defaultPath
        );

        ws.binaryType = "arraybuffer";

        return ws;
      });

    const socket = createSocketFunc();
    let hasOpened = false;

    socket.onopen = () => {
      hasOpened = true;

      $(document).trigger({
        type: "shiny:connected",
        // @ts-expect-error; Can not remove info on a established, malformed Event object
        socket: socket,
      });

      this.onConnected();

      socket.send(
        JSON.stringify({
          method: "init",
          data: this.$initialInput,
        })
      );

      while (this.$pendingMessages.length) {
        const msg = this.$pendingMessages.shift();

        socket.send(msg as string);
      }

      // This launches the action queue loop, which just runs in the background,
      // so we don't need to await it.
      /* eslint-disable-next-line @typescript-eslint/no-floating-promises */
      this.startActionQueueLoop();
    };
    socket.onmessage = (e) => {
      this.taskQueue.enqueue(async () => await this.dispatchMessage(e.data));
    };
    // Called when a successfully-opened websocket is closed, or when an
    // attempt to open a connection fails.
    socket.onclose = (e) => {
      const restarting = e.code === 1012; // Uvicorn sets this code when autoreloading
      // These things are needed only if we've successfully opened the
      // websocket.
      if (hasOpened) {
        $(document).trigger({
          type: "shiny:disconnected",
          // @ts-expect-error; Can not remove info on a established, malformed Event object
          socket: socket,
        });

        this.$notifyDisconnected();
      }

      this.onDisconnected(restarting); // Must be run before this.$removeSocket()
      this.$removeSocket();
    };
    return socket;
  }

  async startActionQueueLoop(): Promise<void> {
    // eslint-disable-next-line no-constant-condition
    while (true) {
      try {
        const action = await this.taskQueue.dequeue();

        await action();
      } catch (e) {
        showErrorInClientConsole(e);
        console.error(e);
      }
    }
  }

  sendInput(values: InputValues): void {
    const msg = JSON.stringify({
      method: "update",
      data: values,
    });

    this.$sendMsg(msg);

    $.extend(this.$inputValues, values);
    this.$updateConditionals();
  }

  $notifyDisconnected(): void {
    if (window.parent) {
      window.parent.postMessage("disconnected", "*");
    }
  }

  $removeSocket(): void {
    this.$socket = null;
  }

  $scheduleReconnect(delay: Parameters<typeof setTimeout>[1]): void {
    this.scheduledReconnect = window.setTimeout(() => {
      this.reconnect();
    }, delay);
  }

  // How long should we wait before trying the next reconnection?
  // The delay will increase with subsequent attempts.
  // .next: Return the time to wait for next connection, and increment counter.
  // .reset: Reset the attempt counter.
  reconnectDelay = (function () {
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

  onDisconnected(reloading = false): void {
    // Add gray-out overlay, if not already present
    if ($("#shiny-disconnected-overlay").length === 0) {
      $(document.body).append('<div id="shiny-disconnected-overlay"></div>');
    }
    $("#shiny-disconnected-overlay").toggleClass("reloading", reloading);

    // To try a reconnect, both the app (this.$allowReconnect) and the
    // server (this.$socket.allowReconnect) must allow reconnections, or
    // session$allowReconnect("force") was called. The "force" option should
    // only be used for testing.
    if (
      (this.$allowReconnect === true &&
        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
        this.$socket!.allowReconnect === true) ||
      this.$allowReconnect === "force"
    ) {
      const delay = this.reconnectDelay.next();

      /* eslint-disable-next-line @typescript-eslint/no-floating-promises */
      showReconnectDialog(delay);
      this.$scheduleReconnect(delay);
    }
  }

  onConnected(): void {
    $("#shiny-disconnected-overlay").remove();
    hideReconnectDialog();
    this.reconnectDelay.reset();
  }

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
  makeRequest(
    method: string,
    args: unknown[],
    onSuccess: OnSuccessRequest,
    onError: OnErrorRequest,
    blobs: Array<ArrayBuffer | Blob | string> | undefined
  ): void {
    let requestId = this.$nextRequestId;

    while (this.$activeRequests[requestId]) {
      requestId = (requestId + 1) % 1000000000;
    }
    this.$nextRequestId = requestId + 1;

    this.$activeRequests[requestId] = {
      onSuccess: onSuccess,
      onError: onError,
    };

    let msg: Blob | string = JSON.stringify({
      method: method,
      args: args,
      tag: requestId,
    });

    if (blobs) {
      // We have binary data to transfer; form a different kind of packet.
      // Start with a 4-byte signature, then for each blob, emit 4 bytes for
      // the length followed by the blob. The json payload is UTF-8 encoded
      // and used as the first blob.

      const uint32ToBuf = function (val: number) {
        const buffer = new ArrayBuffer(4);
        const view = new DataView(buffer);

        view.setUint32(0, val, true); // little-endian
        return buffer;
      };

      const payload = [];

      payload.push(uint32ToBuf(0x01020202)); // signature

      const jsonBuf: Blob = new Blob([msg]);

      payload.push(uint32ToBuf(jsonBuf.size));
      payload.push(jsonBuf);

      for (let i = 0; i < blobs.length; i++) {
        const blob = blobs[i];

        payload.push(
          uint32ToBuf(
            (blob as ArrayBuffer).byteLength || (blob as Blob).size || 0
          )
        );
        payload.push(blob);
      }

      const blob: Blob = new Blob(payload);

      msg = blob;
    }

    this.$sendMsg(msg);
  }

  $sendMsg(msg: MessageValue): void {
    if (this.$socket && this.$socket.readyState) {
      this.$socket.send(msg);
    } else {
      this.$pendingMessages.push(msg);
    }
  }

  receiveError(name: string, error: ErrorsMessageValue): void {
    if (this.$errors[name] === error) return;

    this.$errors[name] = error;
    delete this.$values[name];

    const binding = this.$bindings[name];
    const evt: ShinyEventError = $.Event("shiny:error");

    evt.name = name;
    evt.error = error;
    evt.binding = binding;
    $(binding ? binding.el : document).trigger(evt);
    if (!evt.isDefaultPrevented() && binding && binding.onValueError) {
      binding.onValueError(evt.error);
    }
  }

  async receiveOutput<T>(name: string, value: T): Promise<T | undefined> {
    const binding = this.$bindings[name];
    const evt: ShinyEventValue = $.Event("shiny:value");

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
      await binding.onValueChange(evt.value);
    }

    return value;
  }

  async bindOutput(
    id: string,
    binding: OutputBindingAdapter
  ): Promise<OutputBindingAdapter> {
    if (!id) throw new Error("Can't bind an element with no ID");
    this.$bindings[id] = binding;

    if (this.$values[id] !== undefined)
      await binding.onValueChange(this.$values[id]);
    else if (this.$errors[id] !== undefined)
      binding.onValueError(this.$errors[id]);

    return binding;
  }

  unbindOutput(id: string, binding: OutputBindingAdapter): boolean {
    if (this.$bindings[id] === binding) {
      delete this.$bindings[id];
      return true;
    } else {
      return false;
    }
  }

  // Narrows a scopeComponent -- an input or output object -- to one constrained
  // by nsPrefix. Returns a new object with keys removed and renamed as
  // necessary.
  private _narrowScopeComponent<T>(
    scopeComponent: { [key: string]: T },
    nsPrefix: string
  ) {
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
  private _narrowScope(
    scope: {
      input: InputValues;
      output: { [key: string]: any };
    },
    nsPrefix: string
  ) {
    if (nsPrefix) {
      return {
        input: this._narrowScopeComponent(scope.input, nsPrefix),
        output: this._narrowScopeComponent(scope.output, nsPrefix),
      };
    }
    return scope;
  }

  $updateConditionals(): void {
    // @ts-expect-error; TODO-barret; Could this be transformed into `.trigger(TYPE)`?
    $(document).trigger({
      type: "shiny:conditional",
    });

    const inputs: InputValues = {};

    // Input keys use "name:type" format; we don't want the user to
    // have to know about the type suffix when referring to inputs.
    for (const name in this.$inputValues) {
      if (hasOwnProperty(this.$inputValues, name)) {
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
        const condExpr = el.attr("data-display-if") as string;

        condFunc = scopeExprToFunc(condExpr);
        el.data("data-display-if-func", condFunc);
      }

      const nsPrefix = el.attr("data-ns-prefix") as string;
      const nsScope = this._narrowScope(scope, nsPrefix);
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
  }

  // Message handler management functions =================================

  // // Added in shiny init method
  // Shiny.addCustomMessageHandler = addCustomMessageHandler;

  async dispatchMessage(data: ArrayBufferLike | string): Promise<void> {
    let msgObj: ShinyEventMessage["message"] = {};

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
      // @ts-expect-error; `custom` value is of unknown type. So setting within it is not allowed
      msgObj.custom[type] = data;
    }

    const evt: ShinyEventMessage = $.Event("shiny:message");

    evt.message = msgObj;
    $(document).trigger(evt);
    if (evt.isDefaultPrevented()) return;

    // Before passing the message to handlers, use it to update output progress state
    this.$outputProgress.updateStateFromMessage(evt.message);

    // Send msgObj.foo and msgObj.bar to appropriate handlers
    await this._sendMessagesToHandlers(
      evt.message,
      messageHandlers,
      messageHandlerOrder
    );

    this.$updateConditionals();
  }

  // Message handlers =====================================================

  // A function for sending messages to the appropriate handlers.
  // - msgObj: the object containing messages, with format {msgObj.foo, msObj.bar
  private async _sendMessagesToHandlers(
    msgObj: { [key: string]: unknown },
    handlers: { [key: string]: Handler },
    handlerOrder: string[]
  ): Promise<void> {
    // Dispatch messages to handlers, if handler is present
    for (let i = 0; i < handlerOrder.length; i++) {
      const msgType = handlerOrder[i];

      if (hasOwnProperty(msgObj, msgType)) {
        // Execute each handler with 'this' referring to the present value of
        // 'this'
        await handlers[msgType].call(this, msgObj[msgType]);
      }
    }
  }

  // Call showProgress() on any output bindings that have changed their
  // recalculating status since the last call to takeChanges().
  // Note that we only need to call this function when a "flush" (i.e. "values")
  // message or a "progress" message is received since these are the only
  // two types of messages that can change the recalculating status. For more,
  // see the state machine diagram in outputProgress.ts.
  private _updateProgress() {
    const changed = this.$outputProgress.takeChanges();
    for (const [name, recalculating] of changed.entries()) {
      if (hasOwnProperty(this.$bindings, name)) {
        this.$bindings[name].showProgress(recalculating);
      }
    }
  }

  private _init() {
    // Dev note:
    // * Use arrow functions to allow the Types to propagate.
    // * However, `_sendMessagesToHandlers()` will adjust the `this` context to the same _`this`_.

    addMessageHandler("values", async (message: { [key: string]: any }) => {
      this._updateProgress();

      for (const key in message) {
        if (hasOwnProperty(message, key)) {
          await this.receiveOutput(key, message[key]);
        }
      }
    });

    addMessageHandler(
      "errors",
      (message: { [key: string]: ErrorsMessageValue }) => {
        for (const key in message) {
          if (hasOwnProperty(message, key)) {
            this.receiveError(key, message[key]);
          }
        }
      }
    );

    addMessageHandler(
      "inputMessages",
      async (message: Array<{ id: string; message: unknown }>) => {
        // inputMessages should be an array
        for (let i = 0; i < message.length; i++) {
          const $obj = $(".shiny-bound-input#" + $escape(message[i].id));
          const inputBinding: InputBinding = $obj.data("shiny-input-binding");

          // Dispatch the message to the appropriate input object
          if ($obj.length > 0) {
            if (!$obj.attr("aria-live")) $obj.attr("aria-live", "polite");
            const el = $obj[0];
            const evt: ShinyEventUpdateInput = $.Event("shiny:updateinput");

            evt.message = message[i].message;
            evt.binding = inputBinding;
            $(el).trigger(evt);
            if (!evt.isDefaultPrevented()) {
              try {
                await inputBinding.receiveMessage(el, evt.message);
              } catch (error) {
                console.error(
                  "[shiny] Error in inputBinding.receiveMessage()",
                  { error, binding: inputBinding, message: evt.message }
                );
              }
            }
          }
        }
      }
    );

    addMessageHandler("javascript", (message: string) => {
      /*jshint evil: true */
      indirectEval(message);
    });

    addMessageHandler("console", (message: unknown[]) => {
      for (let i = 0; i < message.length; i++) {
        if (console.log) console.log(message[i]);
      }
    });

    addMessageHandler(
      "progress",
      async (message: { type: string; message: { id: string } }) => {
        if (message.type && message.message) {
          // @ts-expect-error; Unknown values handled with followup if statement
          const handler = await this.progressHandlers[message.type];

          if (handler) handler.call(this, message.message);
        }
      }
    );

    addMessageHandler(
      "notification",
      async (
        message:
          | { type: "remove"; message: string }
          | { type: "show"; message: Parameters<typeof showNotification>[0] }
          | { type: void }
      ) => {
        if (message.type === "show") await showNotification(message.message);
        else if (message.type === "remove") removeNotification(message.message);
        else throw "Unkown notification type: " + message.type;
      }
    );

    addMessageHandler(
      "modal",
      async (
        message:
          | { type: "remove"; message: string }
          | { type: "show"; message: Parameters<typeof showModal>[0] }
          | { type: void }
      ) => {
        if (message.type === "show") await showModal(message.message);
        // For 'remove', message content isn't used
        else if (message.type === "remove") removeModal();
        else throw "Unkown modal type: " + message.type;
      }
    );

    addMessageHandler(
      "response",
      (message: { tag: number; value?: ResponseValue; error?: string }) => {
        const requestId = message.tag;
        const request = this.$activeRequests[requestId];

        if (request) {
          delete this.$activeRequests[requestId];
          if ("value" in message)
            request.onSuccess(message.value as UploadInitValue);
          else request.onError(message.error as string);
        }
      }
    );

    addMessageHandler("allowReconnect", (message: "force" | false | true) => {
      switch (message) {
        case true:
        case false:
        case "force":
          this.$allowReconnect = message;
          break;
        default:
          throw "Invalid value for allowReconnect: " + message;
      }
    });

    addMessageHandler("custom", async (message: { [key: string]: unknown }) => {
      // For old-style custom messages - should deprecate and migrate to new
      // method
      const shinyOnCustomMessage = getShinyOnCustomMessage();

      if (shinyOnCustomMessage) await shinyOnCustomMessage(message);

      // Send messages.foo and messages.bar to appropriate handlers
      await this._sendMessagesToHandlers(
        message,
        customMessageHandlers,
        customMessageHandlerOrder
      );
    });

    addMessageHandler(
      "config",
      (message: { workerId: string; sessionId: string; user?: string }) => {
        this.config = {
          workerId: message.workerId,
          sessionId: message.sessionId,
        };
        if (message.user) setShinyUser(message.user);
        $(document).trigger("shiny:sessioninitialized");
      }
    );

    addMessageHandler("busy", (message: "busy" | "idle") => {
      if (message === "busy") {
        $(document.documentElement).addClass("shiny-busy");
        $(document).trigger("shiny:busy");
      } else if (message === "idle") {
        $(document.documentElement).removeClass("shiny-busy");
        $(document).trigger("shiny:idle");
      }
    });

    addMessageHandler(
      "recalculating",
      (message: {
        name?: string;
        status?: "recalculated" | "recalculating";
      }) => {
        if (
          hasOwnProperty(message, "name") &&
          hasOwnProperty(message, "status")
        ) {
          const binding = this.$bindings[message.name as string];

          if (binding) {
            $(binding.el).trigger("shiny:" + message.status);
          } else {
            $().trigger("shiny:" + message.status);
          }
        }
      }
    );

    addMessageHandler("reload", (message: true) => {
      window.location.reload();
      return;
      message;
    });

    addMessageHandler(
      "shiny-insert-ui",
      async (message: {
        selector: string;
        content: { html: string; deps: HtmlDep[] };
        multiple: boolean;
        where: WherePosition;
      }): Promise<void> => {
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
          await renderHtmlAsync(
            message.content.html,
            $([]),
            message.content.deps
          );
        } else {
          for (const target of targets) {
            await renderContentAsync(target, message.content, message.where);
            // If multiple is false, only render to the first target.
            if (message.multiple === false) break;
          }
        }
      }
    );

    addMessageHandler(
      "shiny-remove-ui",
      (message: { selector: string; multiple: boolean }) => {
        const els = $(message.selector);

        els.each(function (i, el) {
          shinyUnbindAll(el, true);
          $(el).remove();
          // If `multiple` is false, returning false terminates the function
          // and no other elements are removed; if `multiple` is true,
          // returning nothing continues removing all remaining elements.
          return message.multiple === false ? false : undefined;
        });
      }
    );

    addMessageHandler("frozen", (message: { ids: string[] }) => {
      for (let i = 0; i < message.ids.length; i++) {
        shinyForgetLastInputValue(message.ids[i]);
      }
    });

    function getTabset(id: string) {
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

    function getTabContent($tabset: JQuery<HTMLElement>) {
      const tabsetId = $tabset.attr("data-tabsetid") as string;
      const $tabContent = $(
        "div.tab-content[data-tabsetid='" + $escape(tabsetId) + "']"
      );

      return $tabContent;
    }

    function getTargetTabs(
      $tabset: JQuery<HTMLElement>,
      $tabContent: JQuery<HTMLElement>,
      target: string
    ) {
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
      const $liTags: Array<JQuery<HTMLElement>> = [];
      const $divTags: Array<JQuery<HTMLElement>> = [];

      if ($aTag.attr("data-toggle") === "dropdown") {
        // dropdown
        const $dropdownTabset = $aTag.find("+ ul.dropdown-menu");
        const dropdownId = $dropdownTabset.attr("data-tabsetid") as string;

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

    addMessageHandler(
      "shiny-insert-tab",
      async (message: {
        inputId: string;
        divTag: { html: string; deps: HtmlDep[] };
        liTag: { html: string; deps: HtmlDep[] };
        target?: string;
        position: "after" | "before" | void;
        select: boolean;
        menuName: string;
      }): Promise<void> => {
        const $parentTabset = getTabset(message.inputId);
        let $tabset = $parentTabset;
        const $tabContent = getTabContent($tabset);
        let tabsetId = $parentTabset.attr("data-tabsetid");

        const $divTag = $(message.divTag.html);
        const $liTag = $(message.liTag.html);
        const $aTag = $liTag.find("> a");

        // Unless the item is being prepended/appended, the target tab
        // must be provided
        let $targetLiTag: JQuery<HTMLElement> | null = null;

        if (message.target !== null) {
          const targetInfo = getTargetTabs(
            $tabset,
            $tabContent,
            message.target as string
          );

          $targetLiTag = targetInfo.$liTag;
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
          // In the BS4+ case, the server will be generating "top-level" nav markup
          // (i.e., `li.nav-item a.nav-link`), but when inserting inside a dropdown we
          // need `li a.dropdown-item` for correct styling
          // https://getbootstrap.com/docs/5.0/components/navs-tabs/#tabs-with-dropdowns
          $liTag
            .removeClass("nav-item")
            .find(".nav-link")
            .removeClass("nav-link")
            .addClass("dropdown-item");
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
            $tabset.prepend($liTag);
          }
        } else if (message.position === "after") {
          if ($targetLiTag) {
            $targetLiTag.after($liTag);
          } else {
            $tabset.append($liTag);
          }
        }

        await renderContentAsync($liTag[0], {
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
        await renderContentAsync(
          $tabContent[0],
          { html: "", deps: message.divTag.deps },
          // @ts-expect-error; TODO-barret; There is no usage of beforeend
          "beforeend"
        );
        for (const el of $divTag.get()) {
          // Must not use jQuery for appending el to the doc, we don't want any
          // scripts to run (since they will run when renderContent takes a crack).
          $tabContent[0].appendChild(el);
          // If `el` itself is a script tag, this approach won't work (the script
          // won't be run), since we're only sending innerHTML through renderContent
          // and not the whole tag. That's fine in this case because we control the
          // R code that generates this HTML, and we know that the element is not
          // a script tag.
          await renderContentAsync(el, el.innerHTML || el.textContent);
        }

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
        function getTabIndex(
          $tabset: JQuery<HTMLElement>,
          tabsetId: string | undefined
        ) {
          // The 0 is to ensure this works for empty tabsetPanels as well
          const existingTabIds = [0];
          // loop through all existing tabs, find the one with highest id
          // (since this is based on a numeric counter), and increment

          $tabset.find("> li").each(function () {
            const $tab = $(this).find("> a[data-toggle='tab']");

            if ($tab.length > 0) {
              // remove leading url if it exists. (copy of bootstrap url stripper)
              // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
              const href = $tab.attr("href")!.replace(/.*(?=#[^\s]+$)/, "");
              // remove tab id to get the index
              // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
              const index = href!.replace("#tab-" + tabsetId + "-", "");

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
              "a.dropdown-toggle[data-value='" +
                $escape(message.menuName) +
                "']"
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
          } else if (message.target !== null && $targetLiTag !== null) {
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
      }
    );

    // If the given tabset has no active tabs, select the first one
    function ensureTabsetHasVisibleTab($tabset: JQuery<HTMLElement>) {
      const inputBinding = $tabset.data("shiny-input-binding");

      // Use the getValue() method to avoid duplicating the CSS selector
      // for querying the DOM for the currently active tab
      if (!inputBinding.getValue($tabset)) {
        // Note: destTabValue may be null. We still want to proceed
        // through the below logic and setValue so that the input
        // value for the tabset gets updated (i.e. input$tabsetId
        // should be null if there are no tabs).
        const destTabValue = getFirstTab($tabset);
        const evt: ShinyEventUpdateInput = $.Event("shiny:updateinput");

        evt.binding = inputBinding;
        $tabset.trigger(evt);
        inputBinding.setValue($tabset[0], destTabValue);
      }
    }

    // Given a tabset ul jquery object, return the value of the first tab
    // (in document order) that's visible and able to be selected.
    function getFirstTab($ul: JQuery<HTMLElement>) {
      return (
        $ul
          .find("li:visible a[data-toggle='tab']")
          .first()
          .attr("data-value") || null
      );
    }

    function tabApplyFunction(
      target: ReturnType<typeof getTargetTabs>,
      func: ($el: JQuery<HTMLElement>) => void,
      liTags = false
    ) {
      $.each(target, function (key, el) {
        if (key === "$liTag") {
          // $liTag is always just one jQuery element
          func(el as ReturnType<typeof getTargetTabs>["$liTag"]);
        } else if (key === "$divTags") {
          // $divTags is always an array (even if length = 1)
          $.each(
            el as ReturnType<typeof getTargetTabs>["$divTags"],
            function (i, div) {
              func(div);
            }
          );
        } else if (liTags && key === "$liTags") {
          // $liTags is always an array (even if length = 0)
          $.each(
            el as ReturnType<typeof getTargetTabs>["$liTags"],
            function (i, div) {
              func(div);
            }
          );
        }
      });
    }

    addMessageHandler(
      "shiny-remove-tab",
      (message: { inputId: string; target: string; type: never }) => {
        const $tabset = getTabset(message.inputId);
        const $tabContent = getTabContent($tabset);
        const target = getTargetTabs($tabset, $tabContent, message.target);

        tabApplyFunction(target, removeEl);

        ensureTabsetHasVisibleTab($tabset);

        function removeEl($el: JQuery<HTMLElement>) {
          shinyUnbindAll($el, true);
          $el.remove();
        }
      }
    );

    addMessageHandler(
      "shiny-change-tab-visibility",
      (message: {
        inputId: string;
        target: string;
        type: "hide" | "show" | null;
      }) => {
        const $tabset = getTabset(message.inputId);
        const $tabContent = getTabContent($tabset);
        const target = getTargetTabs($tabset, $tabContent, message.target);

        tabApplyFunction(target, changeVisibility, true);

        ensureTabsetHasVisibleTab($tabset);

        function changeVisibility($el: JQuery<HTMLElement>) {
          if (message.type === "show") $el.css("display", "");
          else if (message.type === "hide") {
            $el.hide();
            $el.removeClass("active");
          }
        }
      }
    );

    addMessageHandler(
      "updateQueryString",
      (message: { mode: unknown | "replace"; queryString: string }) => {
        // leave the bookmarking code intact
        if (message.mode === "replace") {
          // @ts-expect-error; No title value being supplied
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
        // @ts-expect-error; No title value being supplied
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
      }
    );

    addMessageHandler(
      "resetBrush",
      (message: { brushId: Parameters<typeof resetBrush>[0] }) => {
        resetBrush(message.brushId);
      }
    );
  }

  // Progress reporting ====================================================

  progressHandlers = {
    // Progress for a particular object
    binding: function (
      this: ShinyApp,
      message: { id: string; persistent: boolean }
    ): void {
      const key = message.id;
      const binding = this.$bindings[key];

      if (binding) {
        $(binding.el).trigger({
          type: "shiny:outputinvalidated",
          // @ts-expect-error; Can not remove info on a established, malformed Event object
          binding: binding,
          name: key,
        });
      }

      this._updateProgress();
    },

    // Open a page-level progress bar
    open: async function (message: {
      style: "notification" | "old";
      id: string;
    }): Promise<void> {
      if (message.style === "notification") {
        // For new-style (starting in Shiny 0.14) progress indicators that use
        // the notification API.

        // Progress bar starts hidden; will be made visible if a value is provided
        // during updates.
        await showNotification({
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

        if ($progressBar) {
          $progressBar.css(
            "top",
            depth * ($progressBar.height() as number) + "px"
          );

          // Stack text objects
          const $progressText = $progress.find(".progress-text");

          $progressText.css(
            "top",
            3 * ($progressBar.height() as number) +
              depth * ($progressText.outerHeight() as number) +
              "px"
          );

          $progress.hide();
        }
      }
    },

    // Update page-level progress bar
    update: function (message: {
      style: "notification" | "old";
      id: string;
      message?: string;
      detail?: string;
      value?: number;
    }): void {
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
    close: function (message: { style: "notification"; id: string }): void {
      if (message.style === "notification") {
        removeNotification(message.id);
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

  //// 2021/03: TypeScript Conversion
  // Added in `./shiny/init.ts` as there are no instances of progressHandlers being used right away on GitHub
  // Shiny.progressHandlers = this.progressHandlers;

  // Returns a URL which can be queried to get values from inside the server
  // function. This is enabled with `options(shiny.testmode=TRUE)`.
  getTestSnapshotBaseUrl({ fullUrl = true } = {}): string {
    const loc = window.location;
    let url = "";

    if (fullUrl) {
      // Strip off everything after last slash in path, like dirname() in R
      url = loc.origin + loc.pathname.replace(/\/[^/]*$/, "");
    }
    url +=
      "/session/" +
      // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
      encodeURIComponent(this.config!.sessionId) +
      "/dataobj/shinytest?w=" +
      // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
      encodeURIComponent(this.config!.workerId) +
      "&nonce=" +
      randomId();

    return url;
  }
}

export { ShinyApp, addCustomMessageHandler };
export type { Handler, ErrorsMessageValue, ShinyWebSocket };
