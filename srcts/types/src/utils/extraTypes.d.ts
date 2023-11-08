type AnyFunction = (...args: any[]) => any;
type AnyVoidFunction = (...args: any[]) => void;
type MapValuesUnion<T> = T[keyof T];
type MapWithResult<X, R> = {
    [Property in keyof X]: R;
};
/**
 * Exclude undefined from T
 */
type NotUndefined<T> = T extends undefined ? never : T;
export type { AnyFunction, AnyVoidFunction, MapValuesUnion, MapWithResult, NotUndefined, };
