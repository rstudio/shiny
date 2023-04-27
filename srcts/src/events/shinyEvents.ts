import type { InputBinding } from "../bindings/input/inputBinding";
import type { OutputBindingAdapter } from "../bindings/outputAdapter";
import type { EventPriority } from "../inputPolicies/inputPolicy";
import type { ErrorsMessageValue } from "../shiny/shinyapp";
import type { EvtFn } from "./jQueryEvents";
import $ from "jquery";

// This class implements a common interface for all Shiny events, and provides
// a layer of abstraction between the Shiny's event and the underlying jQuery
// event object.
class EventBase {
  event: JQuery.Event;

  constructor(type: string) {
    this.event = $.Event(type);
  }

  triggerOn(
    el: HTMLElement | JQuery<HTMLElement> | typeof document | null
  ): void {
    $(el || window.document).trigger(this.event);
  }

  isDefaultPrevented(): boolean {
    return this.event.isDefaultPrevented();
  }
}

interface ShinyEventCommon extends JQuery.Event {
  name: string;
  value: any;
}

class EventCommon extends EventBase {
  declare event: ShinyEventCommon;

  constructor(
    type: ShinyEventCommon["type"],
    name: ShinyEventCommon["name"],
    value: ShinyEventCommon["value"]
  ) {
    super(type);
    this.event.name = name;
    this.event.value = value;
  }

  get name(): ShinyEventCommon["name"] {
    return this.event.name;
  }

  get value(): ShinyEventCommon["value"] {
    return this.event.value;
  }
}

interface ShinyEventInputChanged extends ShinyEventCommon {
  el: HTMLElement | null;
  binding: InputBinding | null;
  inputType: string;
  priority?: EventPriority;
}

class EventInputChanged extends EventCommon {
  declare event: ShinyEventInputChanged;

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

  get el(): ShinyEventInputChanged["el"] {
    return this.event.el;
  }

  get binding(): ShinyEventInputChanged["binding"] {
    return this.event.binding;
  }

  get inputType(): ShinyEventInputChanged["inputType"] {
    return this.event.inputType;
  }

  get priority(): ShinyEventInputChanged["priority"] {
    return this.event.priority;
  }
}

interface ShinyEventUpdateInput extends ShinyEventCommon {
  message?: any;
  binding: InputBinding;
}

class EventUpdateInput extends EventBase {
  declare event: ShinyEventUpdateInput;

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

  get message(): ShinyEventUpdateInput["message"] {
    return this.event.message;
  }

  get binding(): ShinyEventUpdateInput["binding"] {
    return this.event.binding;
  }
}

interface ShinyEventValue extends ShinyEventCommon {
  binding: OutputBindingAdapter;
}

class EventValue extends EventCommon {
  declare event: ShinyEventValue;

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

  get binding(): ShinyEventValue["binding"] {
    return this.event.binding;
  }
}

interface ShinyEventError extends ShinyEventCommon {
  binding: OutputBindingAdapter;
  error: ErrorsMessageValue;
}

class EventError extends EventCommon {
  declare event: ShinyEventError;

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

  get binding(): ShinyEventError["binding"] {
    return this.event.binding;
  }

  get error(): ShinyEventError["error"] {
    return this.event.error;
  }
}

interface ShinyEventMessage extends JQuery.Event {
  message: { [key: string]: unknown };
}

class EventMessage extends EventBase {
  declare event: ShinyEventMessage;

  constructor(message: ShinyEventMessage["message"]) {
    super("shiny:message");
    this.event.message = message;
  }

  get message(): ShinyEventMessage["message"] {
    return this.event.message;
  }
}

// Augment the JQuery interface ----------------------------------------------
// This allows extensions to use .on() with Shiny's custom events.
// E.g. in {bslib}, we use `$(document).on("shiny:value", ...)`
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
