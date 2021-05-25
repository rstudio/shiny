import $ from "jquery";
import { FileInputBinding } from "../bindings/input/fileinput";
import { priorityType } from "../inputPolicies";

interface ShinyEventInputChanged extends JQuery.Event {
  name: string;
  value: any;
  binding: any;
  el: HTMLElement;
  inputType: string;
  priority: priorityType;
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

export type { ShinyEventInputChanged };
export { triggerFileInputChanged };
