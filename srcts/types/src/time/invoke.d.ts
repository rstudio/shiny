import type { InputPolicy } from "../inputPolicies";
import type { InputRatePolicy } from "../inputPolicies/inputRatePolicy";
import type { AnyVoidFunction } from "../utils/extraTypes";
declare class Invoker<X extends AnyVoidFunction> implements InputRatePolicy<X> {
    target: InputPolicy;
    func: X;
    constructor(target: InputPolicy, func: X);
    normalCall(...args: Parameters<X>): void;
    immediateCall(...args: Parameters<X>): void;
}
export { Invoker };
