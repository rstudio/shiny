import type { EventPriority, InputPolicy, InputPolicyOpts } from "./inputPolicy";
declare class InputDeferDecorator implements InputPolicy {
    pendingInput: {
        [key: string]: {
            value: unknown;
            opts: {
                priority: EventPriority;
            };
        };
    };
    target: InputPolicy;
    constructor(target: InputPolicy);
    setInput(nameType: string, value: unknown, opts: InputPolicyOpts): void;
    submit(): void;
}
export { InputDeferDecorator };
