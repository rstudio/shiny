import type { errorsMessageValue } from "../../shiny/shinyapp";
declare class OutputBinding {
    name: string;
    find(scope: JQuery<HTMLElement> | HTMLElement): JQuery<HTMLElement>;
    renderValue(el: HTMLElement, data: any): void;
    getId(el: HTMLElement): string;
    onValueChange(el: HTMLElement, data: any): void;
    onValueError(el: HTMLElement, err: errorsMessageValue): void;
    renderError(el: HTMLElement, err: errorsMessageValue): void;
    clearError(el: HTMLElement): void;
    showProgress(el: HTMLElement, show: boolean): void;
}
export { OutputBinding };
