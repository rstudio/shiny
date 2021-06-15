import { bindScope } from "./bind";
import { renderHtml as singletonsRenderHtml } from "./singletons";
import type { WherePosition, RenderHtmlWherePosition } from "./singletons";
declare function renderDependencies(dependencies: null | Array<HtmlDep>): void;
declare function renderContent(el: bindScope, content: null | string | {
    html: string;
    deps?: Array<HtmlDep>;
}, where?: WherePosition): void;
declare function renderHtml(html: string, el: bindScope, dependencies: Array<HtmlDep>, where?: RenderHtmlWherePosition): ReturnType<typeof singletonsRenderHtml>;
declare type HtmlDepName = string;
declare type HtmlDepVersion = string;
declare type HtmlDep = {
    name: HtmlDepName;
    version: HtmlDepVersion;
    restyle?: boolean;
    src?: {
        href: string;
    };
    meta?: string | Array<string>;
    stylesheet?: string | Array<string>;
    script?: string | Array<string> | Record<string, string> | Array<Record<string, string>>;
    attachment?: string | Array<string> | Record<string, string>;
    head?: string;
};
declare function registerDependency(name: HtmlDepName, version: HtmlDepVersion): void;
export { renderDependencies, renderContent, renderHtml, registerDependency };
export type { HtmlDep };
