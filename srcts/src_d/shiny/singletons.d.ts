import { bindScope } from "./bind";
declare const knownSingletons: Record<string, boolean>;
declare type WherePosition = "replace" | "beforeBegin" | "afterEnd";
declare type RenderHtmlWherePosition = "replace" | InsertPosition;
declare function renderHtml(html: string, el: bindScope, where: RenderHtmlWherePosition): ReturnType<typeof _processHtml>;
declare function registerNames(s: string | Array<string>): void;
declare function _processHtml(val: string): {
    html: string;
    head: string;
    singletons: typeof knownSingletons;
};
export { renderHtml, registerNames };
export type { WherePosition, RenderHtmlWherePosition };
