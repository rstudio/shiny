import $ from "jquery";
import { splitInputNameType } from "./splitInputNameType";

class InputEventDecorator {
  target;

  constructor(target) {
    this.target = target;
  }

  setInput(nameType, value, opts): void {
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
  }
}

export { InputEventDecorator };
