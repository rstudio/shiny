import type { InputBinding } from "../bindings/input/inputBinding";
import type { OutputBindingAdapter } from "../bindings/outputAdapter";
import type { EventPriority } from "../inputPolicies/inputPolicy";
import type { ErrorsMessageValue } from "../shiny/shinyapp";
import type { EvtFn } from "./jQueryEvents";
import "jquery";

// This class implements a common interface for all Shiny events, and provides
// a layer of abstraction between the Shiny's event and the underlying jQuery
// event object.
class EventBase {
  event: JQuery.Event;

  constructor(type: string) {
    this.event = $.Event(type);
  }

  triggerOn(el: HTMLElement | typeof document | null): void {
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
  name: string;
  value: any;

  constructor(type: string, name: string, value: any) {
    super(type);
    this.event.name = this.name = name;
    this.event.value = this.value = value;
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
  el: HTMLElement | null;
  binding: InputBinding | null;
  inputType: string;
  priority?: EventPriority;

  constructor({
    name,
    value,
    el,
    binding,
    inputType,
    priority,
  }: {
    name: string;
    value: any;
    el: HTMLElement | null;
    binding: InputBinding | null;
    inputType: string;
    priority?: EventPriority;
  }) {
    super("shiny:inputchanged", name, value);
    this.el = this.event.el = el;
    this.binding = this.event.binding = binding;
    this.inputType = this.event.inputType = inputType;
    if (priority) {
      this.priority = this.event.priority = priority;
    }
  }
}

interface ShinyEventUpdateInput extends ShinyEventCommon {
  message?: any;
  binding: InputBinding;
}

class EventUpdateInput extends EventBase {
  declare event: ShinyEventUpdateInput;
  message?: any;
  binding: InputBinding;

  constructor({ message, binding }: { message?: any; binding: InputBinding }) {
    super("shiny:updateinput");
    if (message) {
      this.message = this.event.message = message;
    }
    this.binding = this.event.binding = binding;
  }
}

interface ShinyEventValue extends ShinyEventCommon {
  binding: OutputBindingAdapter;
}

class EventValue extends EventCommon {
  declare event: ShinyEventValue;
  binding: OutputBindingAdapter;

  constructor({
    name,
    value,
    binding,
  }: {
    name: string;
    value: any;
    binding: OutputBindingAdapter;
  }) {
    super("shiny:value", name, value);
    this.binding = this.event.binding = binding;
  }
}

interface ShinyEventError extends ShinyEventCommon {
  binding: OutputBindingAdapter;
  error: ErrorsMessageValue;
}

class EventError extends EventCommon {
  declare event: ShinyEventError;
  declare value: null;
  binding: OutputBindingAdapter;
  error: ErrorsMessageValue;

  constructor({
    name,
    binding,
    error,
  }: {
    name: string;
    binding: OutputBindingAdapter;
    error: ErrorsMessageValue;
  }) {
    super("shiny:error", name, null);
    this.binding = this.event.binding = binding;
    this.error = this.event.error = error;
  }
}

interface ShinyEventMessage extends JQuery.Event {
  message: { [key: string]: unknown };
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
  }
}

export {
  EventCommon,
  EventInputChanged,
  EventUpdateInput,
  EventValue,
  EventError,
};

export type {
  ShinyEventInputChanged,
  ShinyEventUpdateInput,
  ShinyEventValue,
  ShinyEventError,
  ShinyEventMessage,
};
