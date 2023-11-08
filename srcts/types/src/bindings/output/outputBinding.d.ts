import type { ErrorsMessageValue } from "../../shiny/shinyapp";
declare class OutputBinding {
    name: string;
    find(scope: HTMLElement | JQuery<HTMLElement>): JQuery<HTMLElement>;
    renderValue(el: HTMLElement, data: unknown): Promise<void> | void;
    getId(el: HTMLElement): string;
    onValueChange(el: HTMLElement, data: unknown): Promise<void>;
    onValueError(el: HTMLElement, err: ErrorsMessageValue): void;
    renderError(el: HTMLElement, err: ErrorsMessageValue): void;
    clearError(el: HTMLElement): void;
    showProgress(el: HTMLElement, show: boolean): void;
}
export { OutputBinding };
