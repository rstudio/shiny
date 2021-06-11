import { InputPolicy } from "../inputPolicies";
declare class Invoker {
    target: InputPolicy;
    func: () => void;
    constructor(target: InputPolicy, func: (...args: Array<any>) => void);
    normalCall(...args: Array<any>): void;
    immediateCall(...args: Array<any>): void;
}
export { Invoker };
