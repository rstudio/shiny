import type { InputBinding } from "../bindings";
type EventPriority = "deferred" | "event" | "immediate";
type InputPolicyOpts = {
    priority: EventPriority;
    el?: HTMLElement;
    binding?: InputBinding;
};
interface InputPolicy {
    target: InputPolicy;
    setInput(name: string, value: unknown, opts: InputPolicyOpts): void;
}
export type { InputPolicy, EventPriority, InputPolicyOpts };
