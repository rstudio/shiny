import type { FileInputBinding } from "../bindings/input/fileinput";
import type { ShinyEventInputChanged } from "./shinyEvents";
declare function triggerFileInputChanged(name: string, value: unknown, binding: FileInputBinding, el: HTMLElement, inputType: string, onEl: typeof document): ShinyEventInputChanged;
export { triggerFileInputChanged };
