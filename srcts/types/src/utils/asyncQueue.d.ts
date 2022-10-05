export declare class AsyncQueue<T> {
    private _promises;
    private _resolvers;
    private _add;
    enqueue(x: T): void;
    dequeue(): Promise<T>;
    isEmpty(): boolean;
    isBlocked(): boolean;
    get length(): number;
}
