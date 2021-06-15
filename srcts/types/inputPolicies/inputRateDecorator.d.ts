import { priorityType, InputPolicy } from "./InputPolicy";
declare type RatePolicyModes = "direct" | "debounce" | "throttle";
declare class InputRateDecorator extends InputPolicy {
    inputRatePolicies: {};
    constructor(target: InputPolicy);
    setInput(nameType: string, value: unknown, opts: {
        priority: priorityType;
    }): void;
    setRatePolicy(nameType: string, mode: RatePolicyModes, millis?: number): void;
    private $ensureInit;
    private $doSetInput;
}
export { InputRateDecorator };
export type { RatePolicyModes };
