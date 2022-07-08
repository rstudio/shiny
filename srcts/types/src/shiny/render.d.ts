import type { BindScope } from "./bind";
import { renderHtml as singletonsRenderHtml } from "./singletons";
import type { WherePosition } from "./singletons";
declare function renderDependencies(dependencies: HtmlDep[] | null): Promise<void>;
declare function renderContent(el: BindScope, content: string | {
    html: string;
    deps?: HtmlDep[];
} | null, where?: WherePosition): Promise<void>;
declare function renderHtml(html: string, el: BindScope, dependencies: HtmlDep[], where?: WherePosition): Promise<ReturnType<typeof singletonsRenderHtml>>;
declare type HtmlDepVersion = string;
declare type MetaItem = {
    name: string;
    content: string;
    [x: string]: string;
};
declare type StylesheetItem = {
    href: string;
    rel?: string;
    type?: string;
};
declare type ScriptItem = {
    src: string;
    [x: string]: string;
};
declare type AttachmentItem = {
    key: string;
    href: string;
    [x: string]: string;
};
declare type HtmlDep = {
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
export { renderDependencies, renderContent, renderHtml, registerDependency };
export type { HtmlDep };
