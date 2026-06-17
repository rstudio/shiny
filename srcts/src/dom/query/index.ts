import * as jqueryAdapter from "./jquery";
import * as nativeAdapter from "./native";

function hasJQuery(): boolean {
  return typeof (globalThis as any).window?.jQuery === "function";
}

export function select(scope: ParentNode, selector: string): Element[] {
  return hasJQuery()
    ? jqueryAdapter.select(scope, selector)
    : nativeAdapter.select(scope, selector);
}

export function closest(el: Element, selector: string): Element | null {
  return hasJQuery()
    ? jqueryAdapter.closest(el, selector)
    : nativeAdapter.closest(el, selector);
}

export function matches(el: Element, selector: string): boolean {
  return hasJQuery()
    ? jqueryAdapter.matches(el, selector)
    : nativeAdapter.matches(el, selector);
}
