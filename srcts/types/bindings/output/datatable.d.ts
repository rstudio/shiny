import { OutputBinding } from "./outputBinding";
import type { ErrorsMessageValue } from "../../shiny/shinyapp";
declare class DatatableOutputBinding extends OutputBinding {
    find(scope: HTMLElement): JQuery<HTMLElement>;
    onValueError(el: HTMLElement, err: ErrorsMessageValue): void;
    renderValue(el: HTMLElement, data: {
        colnames?: string[];
        options?: {
            searching?: boolean;
            search?: {
                caseInsensitive?: boolean;
            };
        } | null;
        action?: string;
        escape?: string;
        evalOptions?: string[];
        callback?: string;
        searchDelay?: number;
    } | null): void;
}
export { DatatableOutputBinding };
