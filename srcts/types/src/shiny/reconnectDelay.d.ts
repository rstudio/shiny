declare const maxAttempts = 10;
declare class ReconnectDelay {
    private attempts;
    next(): number;
    exhausted(): boolean;
    reset(): void;
}
export { ReconnectDelay, maxAttempts };
