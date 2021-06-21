import { OutputBinding } from "./OutputBinding";
import type { ErrorsMessageValue } from "../../shiny/shinyapp";
declare class DatatableOutputBinding extends OutputBinding {
    find(scope: HTMLElement): JQuery<HTMLElement>;
    onValueError(el: HTMLElement, err: ErrorsMessageValue): void;
    renderValue(el: HTMLElement, data: null | {
        colnames?: Array<string>;
        options?: null | {
            searching?: boolean;
            search?: {
                caseInsensitive?: boolean;
            };
        };
        action?: string;
        escape?: string;
        evalOptions?: Array<string>;
        callback?: string;
        searchDelay?: number;
    }): void;
}
export { DatatableOutputBinding };
