import $ from "jquery";
import type { EventPriority, InputPolicy } from "./inputPolicy";

type MaybeInputOpts = {
  priority?: EventPriority;
  binding?: unknown;
  el?: HTMLElement;
};

// Merge opts with defaults, and return a new object.
function addDefaultInputOpts<T>(opts?: T & MaybeInputOpts): T & {
  priority: EventPriority;
  binding: unknown;
  el?: HTMLElement;
} {
  const newOpts = $.extend(
    {
      priority: "immediate",
      binding: null,
      el: null,
    },
    opts
  );

  if (newOpts && typeof newOpts.priority !== "undefined") {
    switch (newOpts.priority) {
      case "deferred":
      case "immediate":
      case "event":
        break;
      default:
        throw new Error(
          "Unexpected input value mode: '" + newOpts.priority + "'"
        );
    }
  }

  return newOpts;
}

class InputValidateDecorator {
  target;

  constructor(target: InputPolicy) {
    this.target = target;
  }

  setInput = function <T>(
    nameType: string,
    value: unknown,
    opts?: T & MaybeInputOpts
  ): void {
    if (!nameType) throw "Can't set input with empty name.";

    const newOpts = addDefaultInputOpts(opts);

    this.target.setInput(nameType, value, newOpts);
  };
}

export { InputValidateDecorator, addDefaultInputOpts };
