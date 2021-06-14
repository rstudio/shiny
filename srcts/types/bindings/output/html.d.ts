import { OutputBinding } from "./OutputBinding";
import { renderContent } from "../../shiny/render";
import type { errorsMessageValue } from "../../shiny/shinyapp";
declare class HtmlOutputBinding extends OutputBinding {
    find(scope: HTMLElement): JQuery<HTMLElement>;
    onValueError(el: HTMLElement, err: errorsMessageValue): void;
    renderValue(el: HTMLElement, data: Parameters<typeof renderContent>[1]): void;
}
export { HtmlOutputBinding };
