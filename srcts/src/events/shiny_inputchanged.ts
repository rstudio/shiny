import $ from "jquery";
import { FileInputBinding } from "../bindings/input/fileinput";

interface ShinyEventInputChanged extends JQuery.Event {
  name: string;
  value: any;
  binding: any;
  el: HTMLElement;
  inputType: string;
}

function triggerFileInputChanged(
  name: string,
  value: any,
  binding: FileInputBinding,
  el: HTMLElement,
  inputType: string,
  onEl: typeof document
): ShinyEventInputChanged {
  const evt = $.Event("shiny:inputchanged") as ShinyEventInputChanged;

  evt.name = this.id;
  evt.value = value;
  evt.binding = binding;
  evt.el = this.el;
  evt.inputType = "shiny.fileupload";

  $(onEl).trigger(evt);

  return evt;
}

export type { ShinyEventInputChanged };
export { triggerFileInputChanged };
