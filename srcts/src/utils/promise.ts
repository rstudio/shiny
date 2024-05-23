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
    }
  );

  // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
  return { promise, resolve: resolve!, reject: reject! };
}
