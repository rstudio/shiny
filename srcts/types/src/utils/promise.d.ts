export declare function promiseWithResolvers<T>(): {
    promise: Promise<T>;
    resolve: (value: PromiseLike<T> | T) => void;
    reject: (reason?: any) => void;
};
export interface InitStatusPromise<T> extends Promise<T> {
    promise: Promise<T>;
    resolve(x: T): void;
    resolved(): boolean;
}
export declare function createInitStatus<T>(): InitStatusPromise<T>;
