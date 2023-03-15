import type { InputPolicy } from "../inputPolicies";
import type { InputRatePolicy } from "../inputPolicies/inputRatePolicy";
import type { AnyVoidFunction } from "../utils/extraTypes";
declare class Debouncer<X extends AnyVoidFunction> implements InputRatePolicy<X> {
    target: InputPolicy | null;
    func: X;
    delayMs: number | undefined;
    timerId: ReturnType<typeof setTimeout> | null;
    args: Parameters<X> | null;
    constructor(target: InputPolicy | null, func: X, delayMs: number | undefined);
    normalCall(...args: Parameters<X>): void;
    immediateCall(...args: Parameters<X>): void;
    isPending(): boolean;
    $clearTimer(): void;
    $invoke(): void;
}
declare function debounce<T extends (...args: unknown[]) => void>(threshold: number | undefined, func: T): (...args: Parameters<T>) => void;
export { Debouncer, debounce };
