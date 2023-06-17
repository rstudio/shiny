import type { ErrorsMessageValue } from "../shiny/shinyapp";
import type { OutputBinding } from "./output";
interface OutputBindingWithResize extends OutputBinding {
    resize?: (el: HTMLElement, width: number | string, height: number | string) => void;
}
declare class OutputBindingAdapter {
    el: HTMLElement;
    binding: OutputBinding;
    constructor(el: HTMLElement, binding: OutputBindingWithResize);
    getId(): string;
    onValueChange(data: unknown): Promise<void>;
    onValueError(err: ErrorsMessageValue): void;
    showProgress(show: boolean): void;
    onResize(): void;
}
export { OutputBindingAdapter };
