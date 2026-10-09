type InputValues = {
    [key: string]: unknown;
};
type FirstMessage = {
    method: "init";
    data: InputValues;
} | {
    method: "resume";
    data: {
        token?: string;
        inputs: InputValues;
        current: InputValues;
    };
};
declare class ReconnectState {
    private token;
    private pendingToken;
    private snapshot;
    private isReconnecting;
    private isAnswered;
    get resumeToken(): string | null;
    get reconnecting(): boolean;
    get answered(): boolean;
    onConfig(token: string | null): void;
    onResumed(): void;
    onAnswered(): void;
    onSocketClosed(hadOpened: boolean, inputs: InputValues): void;
    beginReconnect(): void;
    firstMessage(initial: InputValues, current: InputValues): FirstMessage;
}
export { ReconnectState };
export type { FirstMessage, InputValues as ReconnectInputValues };
