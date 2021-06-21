import type { BindScope } from "./bind";
declare const knownSingletons: Record<string, boolean>;
declare type WherePosition = "replace" | "afterBegin" | "beforeBegin" | "afterEnd" | "beforeEnd";
declare function renderHtml(html: string, el: BindScope, where: WherePosition): ReturnType<typeof processHtml>;
declare function registerNames(s: string | Array<string>): void;
declare function processHtml(val: string): {
    html: string;
    head: string;
    singletons: typeof knownSingletons;
};
export { renderHtml, registerNames };
export type { WherePosition };
