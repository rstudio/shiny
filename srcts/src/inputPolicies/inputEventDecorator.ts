import type { InputPolicy, InputPolicyOpts } from "./inputPolicy";
import { EventInputChanged } from "../events/shinyEvents";
import { splitInputNameType } from "./splitInputNameType";

class InputEventDecorator implements InputPolicy {
  target: InputPolicy;

  constructor(target: InputPolicy) {
    this.target = target;
  }

  setInput(nameType: string, value: unknown, opts: InputPolicyOpts): void {
    const input = splitInputNameType(nameType);

    const inputChangedEvent = new EventInputChanged({
      name: input.name,
      value,
      el: opts.el || null,
      binding: opts.binding || null,
      inputType: input.inputType,
      priority: opts.priority,
    });

    // The `shiny:inputchanged` JavaScript event now triggers on the related
    // input element instead of `document`. Existing event listeners bound to
    // `document` will still detect the event due to event bubbling. #2446
    // If no `el` exists, use `document` instead. #3584
    inputChangedEvent.triggerOn(opts.el || window.document);

    if (!inputChangedEvent.isDefaultPrevented()) {
      let name = inputChangedEvent.name;

      if (inputChangedEvent.inputType !== "")
        name += ":" + inputChangedEvent.inputType;

      // Most opts aren't passed along to lower levels in the input decorator
      // stack.
      this.target.setInput(name, inputChangedEvent.value, {
        priority: opts.priority,
      });
    }
  }
}

export { InputEventDecorator };
