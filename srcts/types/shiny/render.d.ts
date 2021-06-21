import type { BindScope } from "./bind";
import { renderHtml as singletonsRenderHtml } from "./singletons";
import type { WherePosition } from "./singletons";
declare function renderDependencies(dependencies: null | HtmlDep[]): void;
declare function renderContent(el: BindScope, content: null | string | {
    html: string;
    deps?: HtmlDep[];
}, where?: WherePosition): void;
declare function renderHtml(html: string, el: BindScope, dependencies: HtmlDep[], where?: WherePosition): ReturnType<typeof singletonsRenderHtml>;
declare type HtmlDepVersion = string;
declare type HtmlDep = {
    name: string;
    version: HtmlDepVersion;
    restyle?: boolean;
    src?: {
        href: string;
    };
    meta?: string | string[];
    stylesheet?: string | string[];
    script?: string | string[] | {
        [key: string]: string;
    } | Array<{
        [key: string]: string;
    }>;
    attachment?: string | string[] | {
        [key: string]: string;
    };
    head?: string;
};
declare function registerDependency(name: string, version: HtmlDepVersion): void;
export { renderDependencies, renderContent, renderHtml, registerDependency };
export type { HtmlDep };
