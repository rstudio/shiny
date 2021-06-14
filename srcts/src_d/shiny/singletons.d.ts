import { bindScope } from "./bind";
declare function renderHtml(html: any, el: bindScope, where: any): {
    html: any;
    head: string;
    singletons: Record<string, true>;
};
declare function registerNames(s: any): void;
export { renderHtml, registerNames };
