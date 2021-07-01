import type { EventPriority } from "./inputPolicy";
import { InputPolicy } from "./inputPolicy";
import type { InputBinding } from "../bindings";
declare class InputEventDecorator extends InputPolicy {
    constructor(target: InputPolicy);
    setInput(nameType: string, value: unknown, opts: {
        el: HTMLElement;
        priority: EventPriority;
        binding: InputBinding;
    }): void;
}
export { InputEventDecorator };
