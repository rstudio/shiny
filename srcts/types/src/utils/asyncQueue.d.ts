export declare class AsyncQueue<T> {
    private $promises;
    private $resolvers;
    private _add;
    enqueue(x: T): void;
    dequeue(): Promise<T>;
    isEmpty(): boolean;
    isBlocked(): boolean;
    get length(): number;
}
