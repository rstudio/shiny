import { priorityType, InputPolicy } from "./InputPolicy";
declare type lastSentValuesType = Record<string, Record<string, string>>;
declare class InputNoResendDecorator extends InputPolicy {
    lastSentValues: lastSentValuesType;
    constructor(target: InputPolicy, initialValues?: lastSentValuesType);
    setInput(nameType: string, value: unknown, opts: {
        priority: priorityType;
    }): void;
    reset(values?: {}): void;
    forget(name: string): void;
}
export { InputNoResendDecorator };
