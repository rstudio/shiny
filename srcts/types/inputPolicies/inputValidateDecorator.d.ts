import type { EventPriority, InputPolicy } from "./inputPolicy";
declare type MaybeInputOpts = {
    priority?: EventPriority;
    binding?: unknown;
    el?: HTMLElement;
};
declare function addDefaultInputOpts<T>(opts?: MaybeInputOpts & T): T & {
    priority: EventPriority;
    binding: unknown;
    el?: HTMLElement;
};
declare class InputValidateDecorator {
    target: any;
    constructor(target: InputPolicy);
    setInput: <T>(nameType: string, value: unknown, opts?: MaybeInputOpts & T) => void;
}
export { InputValidateDecorator, addDefaultInputOpts };
