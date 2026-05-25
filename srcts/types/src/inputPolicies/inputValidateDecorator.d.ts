import type { InputPolicy, InputPolicyOpts } from "./inputPolicy";
declare function addDefaultInputOpts(opts: Partial<InputPolicyOpts>): InputPolicyOpts;
declare class InputValidateDecorator implements InputPolicy {
    target: InputPolicy;
    constructor(target: InputPolicy);
    setInput(nameType: string, value: unknown, opts?: Partial<InputPolicyOpts>): void;
}
export { addDefaultInputOpts, InputValidateDecorator };
