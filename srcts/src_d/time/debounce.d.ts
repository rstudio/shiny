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
declare function debounce(threshold: number, func: (...args: Array<any>) => void): (...args: Array<any>) => void;
export { Debouncer, debounce };
