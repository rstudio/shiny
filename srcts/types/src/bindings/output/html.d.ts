import { OutputBinding } from "./outputBinding";
import { renderContent } from "../../shiny/render";
import type { ErrorsMessageValue } from "../../shiny/shinyapp";
declare class HtmlOutputBinding extends OutputBinding {
    find(scope: HTMLElement): JQuery<HTMLElement>;
    onValueError(el: HTMLElement, err: ErrorsMessageValue): void;
    renderValue(el: HTMLElement, data: Parameters<typeof renderContent>[1]): Promise<void>;
}
export { HtmlOutputBinding };
