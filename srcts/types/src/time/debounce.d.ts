/// <reference types="node" />
declare class Debouncer {
    target: unknown;
    func: (...args: unknown[]) => void;
    delayMs: number;
    timerId: NodeJS.Timeout;
    args: unknown[];
    constructor(target: unknown, func: (...args: unknown[]) => void, delayMs: number);
    normalCall(...args: unknown[]): void;
    immediateCall(...args: unknown[]): void;
    isPending(): boolean;
    $clearTimer(): void;
    $invoke(): void;
}
declare function debounce<T>(threshold: number, func: (...args: T[]) => void): (...args: T[]) => void;
export { Debouncer, debounce };
