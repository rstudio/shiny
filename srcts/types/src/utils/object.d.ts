import type { NotUndefined } from "./extraTypes";
declare function hasOwnProperty<Prop extends keyof X, X extends {
    [key: string]: any;
}>(obj: X, prop: Prop): obj is X & {
    [key in NonNullable<Prop>]: X[key];
};
declare function hasDefinedProperty<Prop extends keyof X, X extends {
    [key: string]: any;
}>(obj: X, prop: Prop): obj is X & {
    [key in NonNullable<Prop>]: NotUndefined<X[key]>;
};
declare function ifUndefined<X extends NotUndefined<any>, Y>(value: X, alternate: Y): NotUndefined<X>;
declare function ifUndefined<X extends undefined, Y>(value: X, alternate: Y): Y;
export { hasDefinedProperty, hasOwnProperty, ifUndefined };
