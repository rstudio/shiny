/// <reference types="node" />
declare class Throttler<T = unknown> {
    target: unknown;
    func: (...args: Array<T>) => void;
    delayMs: number;
    timerId: NodeJS.Timeout;
    args: Array<T>;
    constructor(target: unknown, func: (...args: Array<T>) => void, delayMs: number);
    normalCall(...args: Array<T>): void;
    immediateCall(...args: Array<T>): void;
    isPending(): boolean;
    $clearTimer(): void;
    $invoke(): void;
}
declare function throttle<T>(threshold: number, func: (...args: Array<T>) => void): (...args: Array<T>) => void;
export { Throttler, throttle };
