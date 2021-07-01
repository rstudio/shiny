import { BindingRegistry } from "../registry";
import { OutputBinding } from "./outputBinding";
declare type InitOutputBindings = {
    outputBindings: BindingRegistry<OutputBinding>;
};
declare function initOutputBindings(): InitOutputBindings;
export { OutputBinding, initOutputBindings };
