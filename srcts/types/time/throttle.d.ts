/// <reference types="node" />
declare class Throttler<T = unknown> {
    target: unknown;
    func: (...args: T[]) => void;
    delayMs: number;
    timerId: NodeJS.Timeout;
    args: T[];
    constructor(target: unknown, func: (...args: T[]) => void, delayMs: number);
    normalCall(...args: T[]): void;
    immediateCall(...args: T[]): void;
    isPending(): boolean;
    $clearTimer(): void;
    $invoke(): void;
}
declare function throttle<T>(threshold: number, func: (...args: T[]) => void): (...args: T[]) => void;
export { Throttler, throttle };
