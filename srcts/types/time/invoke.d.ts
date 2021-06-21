import type { InputPolicy } from "../inputPolicies";
declare class Invoker<T> {
    target: InputPolicy;
    func: () => void;
    constructor(target: InputPolicy, func: (...args: T[]) => void);
    normalCall(...args: T[]): void;
    immediateCall(...args: T[]): void;
}
export { Invoker };
