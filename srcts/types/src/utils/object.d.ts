declare function hasOwnProperty<Prop extends keyof X, X extends {
    [key: string]: any;
}>(obj: X, prop: Prop): obj is X & {
    [key in NonNullable<Prop>]: X[key];
};
export { hasOwnProperty };
