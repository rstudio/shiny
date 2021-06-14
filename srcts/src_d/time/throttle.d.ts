/// <reference types="node" />
declare class Throttler {
    target: any;
    func: (...args: Array<any>) => void;
    delayMs: number;
    timerId: NodeJS.Timeout;
    args: Array<any>;
    constructor(target: any, func: (...args: Array<any>) => void, delayMs: number);
    normalCall(...args: Array<any>): void;
    immediateCall(...args: Array<any>): void;
    isPending(): boolean;
    $clearTimer(): void;
    $invoke(): void;
}
declare function throttle(threshold: number, func: (...args: Array<any>) => void): (...args: Array<any>) => void;
export { Throttler, throttle };
