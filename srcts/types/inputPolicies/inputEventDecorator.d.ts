import type { priorityType } from "./InputPolicy";
import { InputPolicy } from "./InputPolicy";
import type { InputBinding } from "../bindings";
declare class InputEventDecorator extends InputPolicy {
    constructor(target: InputPolicy);
    setInput(nameType: string, value: unknown, opts: {
        el: HTMLElement;
        priority: priorityType;
        binding: InputBinding;
    }): void;
}
export { InputEventDecorator };
