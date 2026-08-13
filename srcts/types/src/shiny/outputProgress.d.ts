type Message = {
    [key: string]: unknown;
};
declare class OutputProgressReporter {
    #private;
    private outputStates;
    private changedOutputs;
    takeChanges(): Map<string, boolean>;
    reset(): void;
    isRecalculating(name: string): boolean;
    updateStateFromMessage(message: Message): void;
}
export { OutputProgressReporter };
