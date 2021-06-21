import { OutputBinding } from "./outputBinding";
import type { ErrorsMessageValue } from "../../shiny/shinyapp";
declare class DatatableOutputBinding extends OutputBinding {
    find(scope: HTMLElement): JQuery<HTMLElement>;
    onValueError(el: HTMLElement, err: ErrorsMessageValue): void;
    renderValue(el: HTMLElement, data: null | {
        colnames?: string[];
        options?: null | {
            searching?: boolean;
            search?: {
                caseInsensitive?: boolean;
            };
        };
        action?: string;
        escape?: string;
        evalOptions?: string[];
        callback?: string;
        searchDelay?: number;
    }): void;
}
export { DatatableOutputBinding };
