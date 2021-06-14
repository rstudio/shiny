declare type priorityType = "immediate" | "deferred" | "event";
declare class InputPolicy {
    target: InputPolicy;
    setInput(name: string, value: unknown, opts: {
        priority: priorityType;
    }): void;
}
export { InputPolicy };
export type { priorityType };
