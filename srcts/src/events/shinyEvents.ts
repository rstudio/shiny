import type { InputBinding } from "../bindings/input/inputBinding";
import type { OutputBindingAdapter } from "../bindings/outputAdapter";
import type { EventPriority } from "../inputPolicies/inputPolicy";
import type { ErrorsMessageValue } from "../shiny/shinyapp";
import type { EvtFn } from "./jQueryEvents";
import $ from "jquery";

/**
 * Shiny Event Base
 *
 * This class implements a common interface for all Shiny events, and provides a
 * layer of abstraction between the Shiny event and the underlying jQuery event
 * object. We use a new class, rather than extending JQuery.Event, because
 * JQuery.Event is an old function-style class. Each Event class has a
 * corresponding ShinyEvent interface that describes the event object that is
 * emitted. At the end of this file, we extend JQuery's `on()` method to
 * associate the ShinyEvent interfaces with their corresponding event string.
 *
 * @class EventBase
 * @typedef {EventBase}
 */
class EventBase {
  /**
   * The underlying jQuery event object wrapped by `EventBase`.
   * @type {JQuery.Event}
   */
  event: JQuery.Event;

  /**
   * Creates an instance of EventBase.
   *
   * @constructor
   * @param {string} type - The event type.
   */
  constructor(type: string) {
    this.event = $.Event(type);
  }

  /**
   * Trigger the event on an element or the document.
   *
   * @example
   * // Instead of this...
   * el.trigger(shinyEvent);
   * // ...do this
   * shinyEvent.triggerOn(el);
   *
   * @param {(HTMLElement | JQuery<HTMLElement> | typeof document | null)} el -
   *   The element to trigger the event on, or `null` to trigger on `document`.
   */
  triggerOn(
    el: HTMLElement | JQuery<HTMLElement> | typeof document | null
  ): void {
    $(el || window.document).trigger(this.event);
  }

  /**
   * Proxy for `event.preventDefault()`.
   *
   * @returns {boolean} `true` if the default action was prevented, `false`
   *   otherwise.
   */
  isDefaultPrevented(): boolean {
    return this.event.isDefaultPrevented();
  }
}

/**
 * A common interface for most Shiny events.
 *
 * @interface ShinyEventCommon
 * @typedef {ShinyEventCommon}
 * @extends {JQuery.Event}
 */
interface ShinyEventCommon extends JQuery.Event {
  /**
   * The event name.
   * @type {string}
   */
  name: string;
  /**
   * Event value containing arbitrary event data.
   * @type {*}
   */
  value: any;
}

/**
 * Create a common Shiny event.
 *
 * @class EventCommon
 * @typedef {EventCommon}
 * @extends {EventBase}
 */
class EventCommon extends EventBase {
  /**
   * The actual event object.
   * @type {ShinyEventCommon}
   */
  declare event: ShinyEventCommon;

  /**
   * Creates an instance of EventCommon.
   *
   * @constructor
   * @param {ShinyEventCommon["type"]} type - The Shiny custom event type, e.g.
   *   `shiny:value`.
   * @param {ShinyEventCommon["name"]} name - The event name.
   * @param {ShinyEventCommon["value"]} value - The event value, or arbitrary
   *   data included with the event.
   */
  constructor(
    type: ShinyEventCommon["type"],
    name: ShinyEventCommon["name"],
    value: ShinyEventCommon["value"]
  ) {
    super(type);
    this.event.name = name;
    this.event.value = value;
  }

  /**
   * Get the event name.
   * @readonly
   * @type {ShinyEventCommon["name"]}
   */
  get name(): ShinyEventCommon["name"] {
    return this.event.name;
  }

  /**
   * Get the event value.
   * @readonly
   * @type {ShinyEventCommon["value"]}
   */
  get value(): ShinyEventCommon["value"] {
    return this.event.value;
  }
}

/**
 * An interface for the `shiny:inputchanged` event.
 *
 * @interface ShinyEventInputChanged
 * @typedef {ShinyEventInputChanged}
 * @extends {ShinyEventCommon}
 */
interface ShinyEventInputChanged extends ShinyEventCommon {
  /**
   * The input element whose value has changed.
   * @type {(HTMLElement | null)}
   */
  el: HTMLElement | null;
  /**
   * The input binding for the changed input.
   * @type {(InputBinding | null)}
   */
  binding: InputBinding | null;
  /**
   * The input type.
   * @type {string}
   */
  inputType: string;
  /**
   * The input event priority.
   * @type {?EventPriority}
   */
  priority?: EventPriority;
}

/**
 * Create a custom `shiny:inputchanged` event as an instance of
 * EventInputChanged.
 *
 * @class EventInputChanged
 * @typedef {EventInputChanged}
 * @extends {EventCommon}
 */
class EventInputChanged extends EventCommon {
  /**
   * The `ShinyEventInputChanged` event object.
   * @type {ShinyEventInputChanged}
   */
  declare event: ShinyEventInputChanged;

  /**
   * Creates an instance of EventInputChanged.
   *
   * @constructor
   * @param {{
      name: ShinyEventInputChanged["name"];
      value: ShinyEventInputChanged["value"];
      el: ShinyEventInputChanged["el"];
      binding: ShinyEventInputChanged["binding"];
      inputType: ShinyEventInputChanged["inputType"];
      priority?: ShinyEventInputChanged["priority"];
    }} {
      name,
      value,
      el,
      binding,
      inputType,
      priority,
    }
   */
  constructor({
    name,
    value,
    el,
    binding,
    inputType,
    priority,
  }: {
    name: ShinyEventInputChanged["name"];
    value: ShinyEventInputChanged["value"];
    el: ShinyEventInputChanged["el"];
    binding: ShinyEventInputChanged["binding"];
    inputType: ShinyEventInputChanged["inputType"];
    priority?: ShinyEventInputChanged["priority"];
  }) {
    super("shiny:inputchanged", name, value);
    this.event.el = el;
    this.event.binding = binding;
    this.event.inputType = inputType;
    if (priority) {
      this.event.priority = priority;
    }
  }

  /**
   * Get the input element whose value has changed.
   * @readonly
   * @type {ShinyEventInputChanged["el"]}
   */
  get el(): ShinyEventInputChanged["el"] {
    return this.event.el;
  }

  /**
   * Get the input binding for the changed input.
   * @readonly
   * @type {ShinyEventInputChanged["binding"]}
   */
  get binding(): ShinyEventInputChanged["binding"] {
    return this.event.binding;
  }

  /**
   * Get the input type.
   * @readonly
   * @type {ShinyEventInputChanged["inputType"]}
   */
  get inputType(): ShinyEventInputChanged["inputType"] {
    return this.event.inputType;
  }

  /**
   * Get the input event priority.
   * @readonly
   * @type {ShinyEventInputChanged["priority"]}
   */
  get priority(): ShinyEventInputChanged["priority"] {
    return this.event.priority;
  }
}

/**
 * A interface for the custom `shiny:updateinput` event.
 *
 * @interface ShinyEventUpdateInput
 * @typedef {ShinyEventUpdateInput}
 * @extends {ShinyEventCommon}
 */
interface ShinyEventUpdateInput extends ShinyEventCommon {
  /**
   * Arbitrary message data, typically sent from the server, to be processed by
   * the `receiveMessage` method of the input binding.
   * @type {?*}
   */
  message?: any;
  /**
   * The input binding for the input.
   * @type {InputBinding}
   */
  binding: InputBinding;
}

/**
 * Create a shiny custom `shiny:updateinput` event as an instance of
 * EventUpdateInput. This event carries message data from the server, sent via
 * `session$sendInputMessage()`, to the input binding's `receiveMessage` method.
 *
 * @class EventUpdateInput
 * @typedef {EventUpdateInput}
 * @extends {EventBase}
 */
class EventUpdateInput extends EventBase {
  /**
   * The `ShinyEventUpdateInput` event object.
   * @type {ShinyEventUpdateInput}
   */
  declare event: ShinyEventUpdateInput;

  /**
   * Creates an instance of EventUpdateInput.
   *
   * @constructor
   * @param {{
      message?: ShinyEventUpdateInput["message"];
      binding: ShinyEventUpdateInput["binding"];
    }} {
      message,
      binding,
    }
   */
  constructor({
    message,
    binding,
  }: {
    message?: ShinyEventUpdateInput["message"];
    binding: ShinyEventUpdateInput["binding"];
  }) {
    super("shiny:updateinput");
    if (message) {
      this.event.message = message;
    }
    this.event.binding = binding;
  }

  /**
   * Get the `shiny:updateinput` message data.
   * @readonly
   * @type {ShinyEventUpdateInput["message"]}
   */
  get message(): ShinyEventUpdateInput["message"] {
    return this.event.message;
  }

  /**
   * Get the input binding for the input.
   * @readonly
   * @type {ShinyEventUpdateInput["binding"]}
   */
  get binding(): ShinyEventUpdateInput["binding"] {
    return this.event.binding;
  }
}

/**
 * A interface for the custom `shiny:value` event.
 *
 * @interface ShinyEventValue
 * @typedef {ShinyEventValue}
 * @extends {ShinyEventCommon}
 */
interface ShinyEventValue extends ShinyEventCommon {
  /**
   * The output binding for the output that updated.
   * @type {OutputBindingAdapter}
   */
  binding: OutputBindingAdapter;
}

/**
 * Create a shiny custom `shiny:value` event as an instance of EventValue. This
 * event is triggered when an output's value changes.
 *
 * @class EventValue
 * @typedef {EventValue}
 * @extends {EventCommon}
 */
class EventValue extends EventCommon {
  /**
   * The `ShinyEventValue` event object.
   * @type {ShinyEventValue}
   */
  declare event: ShinyEventValue;

  /**
   * Creates an instance of EventValue.
   *
   * @constructor
   * @param {{
      name: ShinyEventValue["name"];
      value: ShinyEventValue["value"];
      binding: ShinyEventValue["binding"];
    }} {
      name,
      value,
      binding,
    }
   */
  constructor({
    name,
    value,
    binding,
  }: {
    name: ShinyEventValue["name"];
    value: ShinyEventValue["value"];
    binding: ShinyEventValue["binding"];
  }) {
    super("shiny:value", name, value);
    this.event.binding = binding;
  }

  /**
   * Get the output binding for the output that updated.
   * @readonly
   * @type {ShinyEventValue["binding"]}
   */
  get binding(): ShinyEventValue["binding"] {
    return this.event.binding;
  }
}

/**
 * A interface for the custom `shiny:error` event.
 *
 * @interface ShinyEventError
 * @typedef {ShinyEventError}
 * @extends {ShinyEventCommon}
 */
interface ShinyEventError extends ShinyEventCommon {
  /**
   * The output binding for the output where the error occurred.
   * @type {OutputBindingAdapter}
   */
  binding: OutputBindingAdapter;
  /**
   * The error message.
   * @type {ErrorsMessageValue}
   */
  error: ErrorsMessageValue;
}

/**
 * Create a shiny custom `shiny:error` event as an instance of EventError. This
 * event is triggered when an error occurs while processing the reactive
 * expression that produces the output.
 *
 * @class EventError
 * @typedef {EventError}
 * @extends {EventCommon}
 */
class EventError extends EventCommon {
  /**
   * The `ShinyEventError` event object.
   * @type {ShinyEventError}
   */
  declare event: ShinyEventError;

  /**
   * Creates an instance of EventError.
   *
   * @constructor
   * @param {{
      name: ShinyEventError["name"];
      binding: ShinyEventError["binding"];
      error: ShinyEventError["error"];
    }} {
      name,
      binding,
      error,
    }
   */
  constructor({
    name,
    binding,
    error,
  }: {
    name: ShinyEventError["name"];
    binding: ShinyEventError["binding"];
    error: ShinyEventError["error"];
  }) {
    super("shiny:error", name, null);
    this.event.binding = binding;
    this.event.error = error;
  }

  /**
   * Get the output binding for the output where the error occurred.
   * @readonly
   * @type {ShinyEventError["binding"]}
   */
  get binding(): ShinyEventError["binding"] {
    return this.event.binding;
  }

  /**
   * Get the error message.
   * @readonly
   * @type {ShinyEventError["error"]}
   */
  get error(): ShinyEventError["error"] {
    return this.event.error;
  }
}

/**
 * A interface for the custom `shiny:message` event.
 *
 * @interface ShinyEventMessage
 * @typedef {ShinyEventMessage}
 * @extends {JQuery.Event}
 */
interface ShinyEventMessage extends JQuery.Event {
  /**
   * Arbitrary message data from the server. This data will ultimately be
   * handled by the function provided to `Shiny.addCustomMessageHandler()` for
   * the given message type.
   *
   * @type {{ [key: string]: unknown }}
   */
  message: { [key: string]: unknown };
}

/**
 * Create a shiny custom `shiny:message` event as an instance of EventMessage.
 * This message is triggered when the server sends a custom message to the app
 * via `session$sendCustomMessage()`.
 *
 * @class EventMessage
 * @typedef {EventMessage}
 * @extends {EventBase}
 */
class EventMessage extends EventBase {
  /**
   * The `ShinyEventMessage` event object.
   * @type {ShinyEventMessage}
   */
  declare event: ShinyEventMessage;

  /**
   * Creates an instance of EventMessage.
   *
   * @constructor
   * @param {ShinyEventMessage["message"]} message
   */
  constructor(message: ShinyEventMessage["message"]) {
    super("shiny:message");
    this.event.message = message;
  }

  /**
   * Get the message data from the event.
   * @readonly
   * @type {ShinyEventMessage["message"]}
   */
  get message(): ShinyEventMessage["message"] {
    return this.event.message;
  }
}

// Augment the JQuery interface ----------------------------------------------
// This allows extensions to use .on() in Typescript with Shiny's custom events.
// E.g. in {bslib}, we can use the following with complete type information:
//
// ```
// $(document).on("shiny:value", function(event: ShinyEventValue) { })
// ```
declare global {
  interface JQuery {
    on(
      events: "shiny:inputchanged",
      handler: EvtFn<ShinyEventInputChanged>
    ): this;

    on(
      events: "shiny:updateinput",
      handler: EvtFn<ShinyEventUpdateInput>
    ): this;

    on(events: "shiny:value", handler: EvtFn<ShinyEventValue>): this;
    on(events: "shiny:error", handler: EvtFn<ShinyEventError>): this;
    on(events: "shiny:message", handler: EvtFn<ShinyEventMessage>): this;
  }
}

export {
  EventCommon,
  EventInputChanged,
  EventUpdateInput,
  EventValue,
  EventError,
  EventMessage,
};

export type {
  ShinyEventInputChanged,
  ShinyEventUpdateInput,
  ShinyEventValue,
  ShinyEventError,
  ShinyEventMessage,
};
