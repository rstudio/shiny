import { BindingRegistry } from "../registry";
import { OutputBinding } from "./OutputBinding";
declare type InitOutputBindings = {
    outputBindings: BindingRegistry<OutputBinding>;
};
declare function initOutputBindings(): InitOutputBindings;
export { OutputBinding, initOutputBindings };
