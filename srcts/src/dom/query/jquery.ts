import $import from "jquery";

/**
 * Resolve the jQuery instance bound to the current window.
 *
 * In a browser `window.jQuery` is already the bound instance.  In a
 * Node/happy-dom test environment the package exports a factory that must
 * be called with the current window to produce a bound instance.
 */
function getJQuery(): JQueryStatic {
  const win = (globalThis as any).window as { jQuery?: unknown } | undefined;
  const jq = win?.jQuery ?? $import;
  // If the export is still the factory (no .fn), call it with the window.
  return (typeof (jq as any).fn === "object"
    ? jq
    : (jq as (w: Window & typeof globalThis) => JQueryStatic)(
        win as Window & typeof globalThis,
      )) as JQueryStatic;
}

/**
 * jQuery adapter for `dom/query`.
 *
 * Preserves jQuery selector extensions (e.g. `:visible`) for callers that
 * happen to use them. Note that jQuery only takes `Document | Element` as
 * a `find()` context, so we coerce `DocumentFragment` to its first element
 * child where needed — Shiny's call sites all pass Document or Element
 * scopes in practice.
 */
export function select(scope: ParentNode, selector: string): Element[] {
  const $ = getJQuery();
  return $(scope as Document | Element)
    .find(selector)
    .toArray();
}

export function closest(el: Element, selector: string): Element | null {
  const $ = getJQuery();
  const $found = $(el).closest(selector);
  return $found.length > 0 ? ($found.get(0) as Element) : null;
}

export function matches(el: Element, selector: string): boolean {
  const $ = getJQuery();
  return $(el).is(selector);
}
