import { bindScope } from "./bind";
import { renderHtml as singletonsRenderHtml } from "./singletons";
declare function renderDependencies(dependencies: any): void;
declare type RenderWhereType = "beforeBegin" | "afterEnd" | "replace";
declare function renderContent(el: bindScope, content: null | string | {
    html: any;
    deps?: any;
}, where?: RenderWhereType): void;
declare function renderHtml(html: any, el: bindScope, dependencies: any, where?: RenderWhereType): ReturnType<typeof singletonsRenderHtml>;
declare function registerDependency(name: any, version: any): void;
export { renderDependencies, renderContent, renderHtml, registerDependency };
export type { RenderWhereType };
