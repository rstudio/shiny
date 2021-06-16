import { bindScope } from "./bind";
declare const knownSingletons: Record<string, boolean>;
declare type WherePosition = "replace" | "afterBegin" | "beforeBegin" | "afterEnd" | "beforeEnd";
declare function renderHtml(html: string, el: bindScope, where: WherePosition): ReturnType<typeof _processHtml>;
declare function registerNames(s: string | Array<string>): void;
declare function _processHtml(val: string): {
    html: string;
    head: string;
    singletons: typeof knownSingletons;
};
export { renderHtml, registerNames };
export type { WherePosition };
