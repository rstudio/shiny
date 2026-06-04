/**
 * Thrown by a `dom/*` dispatch function when the requested operation
 * requires jQuery and `window.jQuery` is not available.
 *
 * The wrapper layer (see spec 2026-06-03-jquery-wrapper-design.md) treats
 * jQuery as an optional peer dependency. Most operations have a native
 * fallback; some — notably the jQuery-plugin widgets — do not, and throw
 * this error with a descriptive `feature` string and (optionally) the
 * `constructor.name` of the binding that triggered the call.
 */
export declare class JQueryRequiredError extends Error {
    readonly feature: string;
    readonly binding?: string;
    constructor(feature: string, binding?: string);
}
