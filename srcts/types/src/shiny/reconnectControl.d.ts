type ReconnectPermission = boolean | "force";
declare class ReconnectControl {
    private setTimer;
    private clearTimer;
    private permission;
    private timer;
    private stopped;
    private generation;
    constructor(setTimer?: (fn: () => void, delay: number) => unknown, clearTimer?: (handle: unknown) => void);
    get allow(): ReconnectPermission;
    setAllow(value: ReconnectPermission): void;
    shouldRetry(socketAllows: boolean): boolean;
    schedule(delay: number, reconnect: () => void): void;
    cancel(): void;
    permitsManualRetry(): boolean;
}
declare function peekAllowReconnect(data: unknown): ReconnectPermission | undefined;
export { ReconnectControl, peekAllowReconnect };
export type { ReconnectPermission };
