import type { EventPriority } from "./inputPolicy";
import { InputPolicy } from "./inputPolicy";
declare type RatePolicyModes = "direct" | "debounce" | "throttle";
declare class InputRateDecorator extends InputPolicy {
    inputRatePolicies: {};
    constructor(target: InputPolicy);
    setInput(nameType: string, value: unknown, opts: {
        priority: EventPriority;
    }): void;
    setRatePolicy(nameType: string, mode: RatePolicyModes, millis?: number): void;
    private _ensureInit;
    private _doSetInput;
}
export { InputRateDecorator };
export type { RatePolicyModes };
