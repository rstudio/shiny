import $ from "jquery";
import { InputPolicy, priorityType } from ".";
import { InputBinding } from "../bindings";
import { ShinyEventInputChanged } from "../events/shiny_inputchanged";
import { splitInputNameType } from "./splitInputNameType";

class InputEventDecorator extends InputPolicy {
  constructor(target: InputPolicy) {
    super();
    this.target = target;
  }

  setInput(
    nameType: string,
    value: unknown,
    opts: {
      el: HTMLElement;
      priority: priorityType;
      binding: InputBinding;
    }
  ): void {
    const evt = jQuery.Event("shiny:inputchanged") as ShinyEventInputChanged;

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
