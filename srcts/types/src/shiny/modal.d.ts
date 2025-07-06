import type { HtmlDep } from "./render";
declare function show({ html, deps, }?: {
    html?: string;
    deps?: HtmlDep[];
}): Promise<void>;
declare function remove(): void;
export { remove as removeModal, show as showModal };
