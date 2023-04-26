import type { InputBinding } from "../bindings/input/inputBinding";
import type { OutputBindingAdapter } from "../bindings/outputAdapter";
import type { EventPriority } from "../inputPolicies/inputPolicy";
import type { ErrorsMessageValue } from "../shiny/shinyapp";

import "jquery";

type EvtClassFn<T extends ShinyEventCommon> =
  | ((evt: T["evt"]) => void)
  | null
  | undefined;

interface ShinyEventCommon extends JQuery.Event {
  name: string;
  value: any;
}

// This class implements a common interface for all Shiny events, and provides
// a layer of abstraction between the Shiny's event and the underlying jQuery
// event object.
class EventCommon {
  evt: ShinyEventCommon;
  name: string;
  value: any;

  constructor(type: string, name: string, value: any) {
    this.evt = $.Event(type) as ShinyEventCommon;
    this.evt.name = this.name = name;
    this.evt.value = this.value = value;
  }

  triggerOn(el: HTMLElement | typeof document | null): void {
    $(el || window.document).trigger(this.evt);
  }

  isDefaultPrevented(): boolean {
    return this.evt.isDefaultPrevented();
  }
}

interface ShinyEventInputChanged extends ShinyEventCommon {
  el: HTMLElement | null;
  binding: InputBinding | null;
  inputType: string;
  priority?: EventPriority;
}

class EventInputChanged extends EventCommon {
  declare evt: ShinyEventInputChanged;
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
    this.el = this.evt.el = el;
    this.binding = this.evt.binding = binding;
    this.inputType = this.evt.inputType = inputType;
    if (priority) {
      this.priority = this.evt.priority = priority;
    }
  }
}

interface ShinyEventUpdateInput extends ShinyEventCommon {
  message: unknown;
  binding: InputBinding;
}
interface ShinyEventValue extends ShinyEventCommon {
  value: unknown;
  binding: OutputBindingAdapter;
}

interface ShinyEventError extends ShinyEventCommon {
  binding: OutputBindingAdapter;
  error: ErrorsMessageValue;
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
      handler: EvtClassFn<ShinyEventInputChanged>
    ): this;
  }
}

export { EventCommon, EventInputChanged };

export type {
  ShinyEventInputChanged,
  ShinyEventUpdateInput,
  ShinyEventValue,
  ShinyEventError,
  ShinyEventMessage,
};
