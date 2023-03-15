import { BindingRegistry } from "../registry";
import { InputBinding } from "./inputBinding";
import { FileInputBinding } from "./fileinput";
type InitInputBindings = {
    inputBindings: BindingRegistry<InputBinding>;
    fileInputBinding: FileInputBinding;
};
declare function initInputBindings(): InitInputBindings;
export { initInputBindings, InputBinding };
