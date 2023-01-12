declare type AnyFunction = (...args: any[]) => any;
declare type AnyVoidFunction = (...args: any[]) => void;
declare type MapValuesUnion<T> = T[keyof T];
declare type MapWithResult<X, R> = {
    [Property in keyof X]: R;
};
/**
 * Exclude undefined from T
 */
declare type NotUndefined<T> = T extends undefined ? never : T;
export type { AnyFunction, AnyVoidFunction, MapValuesUnion, MapWithResult, NotUndefined, };
