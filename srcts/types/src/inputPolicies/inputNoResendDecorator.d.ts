import type { InputPolicy, InputPolicyOpts } from "./inputPolicy";
type LastSentValues = {
    [key: string]: {
        [key: string]: string;
    };
};
declare class InputNoResendDecorator implements InputPolicy {
    target: InputPolicy;
    lastSentValues: LastSentValues;
    constructor(target: InputPolicy, initialValues?: LastSentValues);
    setInput(nameType: string, value: unknown, opts: InputPolicyOpts): void;
    reset(values?: LastSentValues): void;
    forget(name: string): void;
}
export { InputNoResendDecorator };
