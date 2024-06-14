import { BindingRegistry } from "../registry";
import { InputBinding } from "./inputBinding";
import { FileInputBinding } from "./fileinput";
declare function initInputBindings(): {
    inputBindings: BindingRegistry<InputBinding>;
    fileInputBinding: FileInputBinding;
};
export { initInputBindings, InputBinding };
