import type { BindScope } from "./bind";
import type { WherePosition } from "./singletons";
import { renderHtml as singletonsRenderHtml } from "./singletons";
declare function renderContentAsync(el: BindScope, content: string | {
    html: string;
    deps?: HtmlDep[];
} | null, where?: WherePosition): Promise<void>;
declare function renderContent(el: BindScope, content: string | {
    html: string;
    deps?: HtmlDep[];
} | null, where?: WherePosition): Promise<void>;
declare function renderHtmlAsync(html: string, el: BindScope, dependencies: HtmlDep[], where?: WherePosition): Promise<ReturnType<typeof singletonsRenderHtml>>;
declare function renderHtml(html: string, el: BindScope, dependencies: HtmlDep[], where?: WherePosition): ReturnType<typeof singletonsRenderHtml>;
declare function renderDependenciesAsync(dependencies: HtmlDep[] | null): Promise<void>;
declare function renderDependencies(dependencies: HtmlDep[] | null): void;
type HtmlDepVersion = string;
type MetaItem = {
    name: string;
    content: string;
    [x: string]: string;
};
type StylesheetItem = {
    href: string;
    rel?: string;
    type?: string;
};
type ScriptItem = {
    src: string;
    [x: string]: string;
};
type AttachmentItem = {
    key: string;
    href: string;
    [x: string]: string;
};
type HtmlDep = {
    name: string;
    version: HtmlDepVersion;
    restyle?: boolean;
    src?: {
        href: string;
    };
    meta?: MetaItem[] | {
        [x: string]: string;
    };
    stylesheet?: string[] | StylesheetItem | StylesheetItem[] | string;
    script?: ScriptItem | ScriptItem[] | string[] | string;
    attachment?: AttachmentItem[] | string[] | string | {
        [key: string]: string;
    };
    head?: string;
};
declare function registerDependency(name: string, version: HtmlDepVersion): void;
export { registerDependency, renderContent, renderContentAsync, renderDependencies, renderDependenciesAsync, renderHtml, renderHtmlAsync, };
export type { HtmlDep };
