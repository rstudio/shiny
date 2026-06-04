/**
 * Native (DOM-only) adapter for `dom/query`.
 * No jQuery imports or jQuery type references may appear in this file.
 */
export function select(scope: ParentNode, selector: string): Element[] {
  return Array.from(scope.querySelectorAll(selector));
}

export function closest(el: Element, selector: string): Element | null {
  return el.closest(selector);
}

export function matches(el: Element, selector: string): boolean {
  return el.matches(selector);
}
