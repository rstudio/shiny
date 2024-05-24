export declare function promiseWithResolvers<T>(): {
    promise: Promise<T>;
    resolve: (value: PromiseLike<T> | T) => void;
    reject: (reason?: any) => void;
};
