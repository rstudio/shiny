/**
 * Native (DOM-only) adapter for `dom/query`.
 * No jQuery imports or jQuery type references may appear in this file.
 */
export declare function select(scope: ParentNode, selector: string): Element[];
export declare function closest(el: Element, selector: string): Element | null;
export declare function matches(el: Element, selector: string): boolean;
