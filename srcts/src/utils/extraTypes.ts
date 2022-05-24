type AnyFunction = (...args: any[]) => any;
type AnyVoidFunction = (...args: any[]) => void;
type MapValuesUnion<T> = T[keyof T];
type MapWithResult<X, R> = {
  [Property in keyof X]: R;
};

export type { AnyFunction, AnyVoidFunction, MapValuesUnion, MapWithResult };
