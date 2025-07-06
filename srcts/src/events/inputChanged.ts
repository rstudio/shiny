import $ from "jquery";
import type { FileInputBinding } from "../bindings/input/fileinput";
import type { ShinyEventInputChanged } from "./shinyEvents";

function triggerFileInputChanged(
  name: string,
  value: unknown,
  binding: FileInputBinding,
  el: HTMLElement,
  inputType: string,
  onEl: typeof document,
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

export { triggerFileInputChanged };
