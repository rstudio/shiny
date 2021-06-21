import type { EventPriority } from "./inputPolicy";
import { InputPolicy } from "./inputPolicy";
declare type LastSentValues = Record<string, Record<string, string>>;
declare class InputNoResendDecorator extends InputPolicy {
    lastSentValues: LastSentValues;
    constructor(target: InputPolicy, initialValues?: LastSentValues);
    setInput(nameType: string, value: unknown, opts: {
        priority: EventPriority;
    }): void;
    reset(values?: {}): void;
    forget(name: string): void;
}
export { InputNoResendDecorator };
