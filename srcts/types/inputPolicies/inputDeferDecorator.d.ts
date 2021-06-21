import type { EventPriority } from "./inputPolicy";
import { InputPolicy } from "./inputPolicy";
declare class InputDeferDecorator extends InputPolicy {
    pendingInput: Record<string, {
        value: unknown;
        opts: {
            priority: EventPriority;
        };
    }>;
    constructor(target: InputPolicy);
    setInput(nameType: string, value: unknown, opts: {
        priority: EventPriority;
    }): void;
    submit(): void;
}
export { InputDeferDecorator };
