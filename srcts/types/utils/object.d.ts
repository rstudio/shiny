/**
 * Wrapper around `Object.prototype.hasOwnProperty.call(x,y)`
 * @param x Object to inspect
 * @param y Key to inspect in `x`
 * @returns Whether object `x` actually has property `y`.
 * */
declare function hasOwnProperty(x: {
    [key: string]: unknown;
}, y: string): boolean;
export { hasOwnProperty };
