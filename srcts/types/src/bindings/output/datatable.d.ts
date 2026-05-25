import type { ErrorsMessageValue } from "../../shiny/shinyapp";
import { OutputBinding } from "./outputBinding";
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
            escape?: string;
        } | null;
        escape?: string;
        action?: string;
        evalOptions?: string[];
        callback?: string;
        searchDelay?: number;
    } | null): void;
}
export { DatatableOutputBinding };
