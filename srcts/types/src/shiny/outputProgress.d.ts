type Message = {
    [key: string]: unknown;
};
declare class OutputProgressReporter {
    #private;
    private outputStates;
    isRecalculating(name: string): boolean;
    updateStateFromMessage(message: Message): void;
}
export { OutputProgressReporter };
