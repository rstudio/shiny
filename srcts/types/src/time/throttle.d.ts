/// <reference types="node" />
import type { InputPolicy } from "../inputPolicies";
import type { InputRatePolicy } from "../inputPolicies/inputRatePolicy";
import type { AnyVoidFunction } from "../utils/extraTypes";
declare class Throttler<X extends AnyVoidFunction> implements InputRatePolicy<X> {
    target: InputPolicy;
    func: X;
    delayMs: number | undefined;
    timerId: NodeJS.Timeout | null;
    args: Parameters<X> | null;
    constructor(target: InputPolicy, func: X, delayMs: number | undefined);
    normalCall(...args: Parameters<X>): void;
    immediateCall(...args: Parameters<X>): void;
    isPending(): boolean;
    $clearTimer(): void;
    $invoke(): void;
}
export { Throttler, };
