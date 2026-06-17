import $import from "jquery";

/**
 * Resolve the jQuery instance bound to the current window.
 *
 * In the browser bundle, esbuild rewrites `import $import from "jquery"`
 * to `window.jQuery` (see `srcts/build/shiny.ts`), so `$import` and
 * `window.jQuery` reference the same already-bound instance. In a
 * Node/happy-dom test environment the package exports a factory that
 * must be called with the current window to produce a bound instance;
 * the factory path below handles that case.
 *
 * This function is only invoked from adapter functions, which are only
 * reached when `hasJQuery()` in `./index.ts` has already confirmed that
 * `window.jQuery` is a function — the `?? $import` fallback is therefore
 * unreachable in normal flow and is present only as a defensive default.
 */
function getJQuery(): JQueryStatic {
  const win = (globalThis as any).window as { jQuery?: unknown } | undefined;
  const jq = win?.jQuery ?? $import;
  // If the export is still the factory (no .fn), call it with the window.
  return (
    typeof (jq as any).fn === "object"
      ? jq
      : (jq as (w: Window & typeof globalThis) => JQueryStatic)(
          win as Window & typeof globalThis,
        )
  ) as JQueryStatic;
}

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
