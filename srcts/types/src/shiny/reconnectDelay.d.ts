declare class ReconnectDelay {
    private attempts;
    next(): number;
    reset(): void;
}
export { ReconnectDelay };
