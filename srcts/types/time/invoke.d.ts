import { InputPolicy } from "../inputPolicies";
declare class Invoker<T> {
    target: InputPolicy;
    func: () => void;
    constructor(target: InputPolicy, func: (...args: Array<T>) => void);
    normalCall(...args: Array<T>): void;
    immediateCall(...args: Array<T>): void;
}
export { Invoker };
