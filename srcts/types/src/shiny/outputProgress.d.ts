declare enum OutputStates {
    Initial = "initial",
    Running = "running",
    Idle = "idle",
    Value = "value",
    Error = "error",
    Cancel = "cancel",
    Persisting = "persisting",
    Invalidated = "invalidated"
}
type Message = {
    [key: string]: unknown;
};
declare class OutputProgressState {
    #private;
    outputStates: Map<string, OutputStates>;
    isRecalculating(name: string): boolean;
    processMessage(message: Message): void;
}
export { OutputProgressState };
