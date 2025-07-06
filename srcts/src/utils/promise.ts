// A shim for Promise.withResolvers. Once browser support is widespread, we can
// remove this.
export function promiseWithResolvers<T>(): {
  promise: Promise<T>;
  resolve: (value: PromiseLike<T> | T) => void;
  reject: (reason?: any) => void;
} {
  let resolve: (value: PromiseLike<T> | T) => void;
  let reject: (reason?: any) => void;
  const promise = new Promise(
    (res: (value: PromiseLike<T> | T) => void, rej: (reason?: any) => void) => {
      resolve = res;
      reject = rej;
    },
  );

  return { promise, resolve: resolve!, reject: reject! };
}

export interface InitStatusPromise<T> extends Promise<T> {
  promise: Promise<T>;
  resolve(x: T): void;
  resolved(): boolean;
}

export function createInitStatus<T>(): InitStatusPromise<T> {
  const { promise, resolve } = promiseWithResolvers<T>();
  // eslint-disable-next-line @typescript-eslint/naming-convention
  let _resolved = false;

  return {
    promise,
    resolve(x: T) {
      _resolved = true;
      resolve(x);
    },
    then: promise.then.bind(promise),
    catch: promise.catch.bind(promise),
    finally: promise.finally.bind(promise),
    [Symbol.toStringTag]: "InitStatus",
    resolved() {
      return _resolved;
    },
  };
}
