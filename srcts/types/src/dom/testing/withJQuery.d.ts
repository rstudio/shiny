/**
 * Toggle `window.jQuery` for the duration of `fn` and restore it afterward
 * (even on throw). Used by dual-path adapter parity tests.
 *
 * When `present` is true, installs the real `jquery` package as
 * `window.jQuery`; when false, sets `window.jQuery` to undefined.
 *
 * Requires a DOM (call `setupDom()` first).
 */
export declare function withJQuery<T>(present: boolean, fn: () => T): T;
