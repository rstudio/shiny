/// <reference types="node" />
declare class Debouncer {
    target: unknown;
    func: (...args: Array<unknown>) => void;
    delayMs: number;
    timerId: NodeJS.Timeout;
    args: Array<unknown>;
    constructor(target: unknown, func: (...args: Array<unknown>) => void, delayMs: number);
    normalCall(...args: Array<unknown>): void;
    immediateCall(...args: Array<unknown>): void;
    isPending(): boolean;
    $clearTimer(): void;
    $invoke(): void;
}
declare function debounce<T>(threshold: number, func: (...args: Array<T>) => void): (...args: Array<T>) => void;
export { Debouncer, debounce };
