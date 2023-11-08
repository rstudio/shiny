import type { InputPolicy, InputPolicyOpts } from "./inputPolicy";
declare class InputEventDecorator implements InputPolicy {
    target: InputPolicy;
    constructor(target: InputPolicy);
    setInput(nameType: string, value: unknown, opts: InputPolicyOpts): void;
}
export { InputEventDecorator };
