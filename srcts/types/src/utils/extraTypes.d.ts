declare type AnyFunction = (...args: any[]) => any;
declare type AnyVoidFunction = (...args: any[]) => void;
declare type MapValuesUnion<T> = T[keyof T];
declare type MapWithResult<X, R> = {
    [Property in keyof X]: R;
};
export type { AnyFunction, AnyVoidFunction, MapValuesUnion, MapWithResult };
