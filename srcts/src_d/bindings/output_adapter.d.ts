import type { errorsMessageValue } from "../shiny/shinyapp";
import { OutputBinding } from "./output";
interface OutpuBindingWithResize extends OutputBinding {
    resize?: (el: HTMLElement, width: string | number, height: string | number) => void;
}
declare class OutputBindingAdapter {
    el: HTMLElement;
    binding: OutputBinding;
    constructor(el: HTMLElement, binding: OutpuBindingWithResize);
    getId(): string;
    onValueChange(data: unknown): void;
    onValueError(err: errorsMessageValue): void;
    showProgress(show: boolean): void;
    onResize(): void;
}
export { OutputBindingAdapter };
