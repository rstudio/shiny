declare type EventPriority = "deferred" | "event" | "immediate";
declare class InputPolicy {
    target: InputPolicy;
    setInput(name: string, value: unknown, opts: {
        priority: EventPriority;
    }): void;
}
export { InputPolicy };
export type { EventPriority };
