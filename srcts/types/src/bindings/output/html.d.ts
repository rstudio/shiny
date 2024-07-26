import { renderContentAsync } from "../../shiny/render";
import type { ErrorsMessageValue } from "../../shiny/shinyapp";
import { OutputBinding } from "./outputBinding";
declare class HtmlOutputBinding extends OutputBinding {
    find(scope: HTMLElement): JQuery<HTMLElement>;
    onValueError(el: HTMLElement, err: ErrorsMessageValue): void;
    renderValue(el: HTMLElement, data: Parameters<typeof renderContentAsync>[1]): Promise<void>;
}
export { HtmlOutputBinding };
