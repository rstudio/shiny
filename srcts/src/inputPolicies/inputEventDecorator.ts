import $ from "jquery";
import type { ShinyEventInputChanged } from "../events/shinyEvents";
import type { InputPolicy, InputPolicyOpts } from "./inputPolicy";
import { splitInputNameType } from "./splitInputNameType";

class InputEventDecorator implements InputPolicy {
  target: InputPolicy;

  constructor(target: InputPolicy) {
    this.target = target;
  }

  setInput(nameType: string, value: unknown, opts: InputPolicyOpts): void {
    const evt = $.Event("shiny:inputchanged") as ShinyEventInputChanged;

    const input = splitInputNameType(nameType);

    evt.name = input.name;
    evt.inputType = input.inputType;
    evt.value = value;
    evt.binding = opts.binding || null;
    evt.el = opts.el || null;
    evt.priority = opts.priority;

    // The `shiny:inputchanged` JavaScript event now triggers on the related
    // input element instead of `document`. Existing event listeners bound to
    // `document` will still detect the event due to event bubbling. #2446
    // If no `el` exists, use `document` instead. #3584
    $(opts.el || window.document).trigger(evt);

    if (!evt.isDefaultPrevented()) {
      let name = evt.name;

      if (evt.inputType !== "") name += ":" + evt.inputType;

      // Most opts aren't passed along to lower levels in the input decorator
      // stack.
      this.target.setInput(name, evt.value, { priority: opts.priority });
    }
  }
}

export { InputEventDecorator };
