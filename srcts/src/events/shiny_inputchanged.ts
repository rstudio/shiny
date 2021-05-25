import $ from "jquery";
import { InputBinding } from "../bindings";
import { FileInputBinding } from "../bindings/input/fileinput";
import { OutputBindingAdapter } from "../bindings/output_adapter";
import { priorityType } from "../inputPolicies";

interface ShinyEventCommon extends JQuery.Event {
  name: string;
  value;
  el: HTMLElement;
}
interface ShinyEventInputChanged extends ShinyEventCommon {
  value;
  binding: InputBinding;
  inputType: string;
  priority: priorityType;
}
interface ShinyEventUpdateInput extends ShinyEventCommon {
  message: any;
  binding: InputBinding;
}
interface ShinyEventValue extends ShinyEventCommon {
  value;
  binding: OutputBindingAdapter;
}

interface ShinyEventError extends ShinyEventCommon {
  binding: OutputBindingAdapter;
  error: string;
}
interface ShinyEventMessage extends JQuery.Event {
  message: any;
}

function triggerFileInputChanged(
  name: string,
  value,
  binding: FileInputBinding,
  el: HTMLElement,
  inputType: string,
  onEl: typeof document
): ShinyEventInputChanged {
  const evt = $.Event("shiny:inputchanged") as ShinyEventInputChanged;

  evt.name = name;
  evt.value = value;
  evt.binding = binding;
  evt.el = el;
  evt.inputType = inputType;

  $(onEl).trigger(evt);

  return evt;
}

export type {
  ShinyEventInputChanged,
  ShinyEventError,
  ShinyEventValue,
  ShinyEventMessage,
  ShinyEventUpdateInput,
};
export { triggerFileInputChanged };
