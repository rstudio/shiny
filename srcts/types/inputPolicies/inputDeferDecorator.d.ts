import { priorityType, InputPolicy } from "./InputPolicy";
declare class InputDeferDecorator extends InputPolicy {
    pendingInput: Record<string, {
        value: unknown;
        opts: {
            priority: priorityType;
        };
    }>;
    constructor(target: InputPolicy);
    setInput(nameType: string, value: unknown, opts: {
        priority: priorityType;
    }): void;
    submit(): void;
}
export { InputDeferDecorator };
