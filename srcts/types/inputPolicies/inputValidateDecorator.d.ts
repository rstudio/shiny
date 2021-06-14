import { priorityType, InputPolicy } from "./InputPolicy";
declare type MaybeInputOpts = {
    priority?: priorityType;
    binding?: unknown;
    el?: HTMLElement;
};
declare function addDefaultInputOpts<T>(opts?: T & MaybeInputOpts): T & {
    priority: priorityType;
    binding: unknown;
    el?: HTMLElement;
};
declare class InputValidateDecorator {
    target: any;
    constructor(target: InputPolicy);
    setInput: <T>(nameType: string, value: unknown, opts?: T & MaybeInputOpts) => void;
}
export { InputValidateDecorator, addDefaultInputOpts };
