import type { InputPolicy, InputPolicyOpts } from "./inputPolicy";
import type { InputRatePolicy } from "./inputRatePolicy";
type RatePolicyModes = "debounce" | "direct" | "throttle";
declare class InputRateDecorator implements InputPolicy {
    target: InputPolicy;
    inputRatePolicies: {
        [key: string]: InputRatePolicy<InputRateDecorator["_doSetInput"]>;
    };
    constructor(target: InputPolicy);
    setInput(nameType: string, value: unknown, opts: InputPolicyOpts): void;
    setRatePolicy(nameType: string, mode: RatePolicyModes, millis?: number): void;
    private _ensureInit;
    private _doSetInput;
}
export { InputRateDecorator };
export type { RatePolicyModes };
