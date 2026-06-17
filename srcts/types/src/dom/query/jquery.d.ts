/**
 * jQuery adapter for `dom/query`.
 *
 * Preserves jQuery selector extensions (e.g. `:visible`) for callers that
 * happen to use them.
 *
 * Note: jQuery's `find()` only accepts `Document | Element` as context.
 * Shiny's call sites all pass `Document | Element` in practice, so we
 * cast `ParentNode` to `Document | Element` without coercion. Passing a
 * `DocumentFragment` is not supported and may behave inconsistently with
 * the native adapter.
 */
export declare function select(scope: ParentNode, selector: string): Element[];
export declare function closest(el: Element, selector: string): Element | null;
export declare function matches(el: Element, selector: string): boolean;
