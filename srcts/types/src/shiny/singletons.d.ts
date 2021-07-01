import type { BindScope } from "./bind";
declare const knownSingletons: {
    [key: string]: boolean;
};
declare type WherePosition = "afterBegin" | "afterEnd" | "beforeBegin" | "beforeEnd" | "replace";
declare function renderHtml(html: string, el: BindScope, where: WherePosition): ReturnType<typeof processHtml>;
declare function registerNames(s: string[] | string): void;
declare function processHtml(val: string): {
    html: string;
    head: string;
    singletons: typeof knownSingletons;
};
export { renderHtml, registerNames };
export type { WherePosition };
