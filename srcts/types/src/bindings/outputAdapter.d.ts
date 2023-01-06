import type { ErrorsMessageValue } from "../shiny/shinyapp";
import type { OutputBinding } from "./output";
interface OutpuBindingWithResize extends OutputBinding {
    resize?: (el: HTMLElement, width: number | string, height: number | string) => void;
}
declare class OutputBindingAdapter {
    el: HTMLElement;
    binding: OutputBinding;
    constructor(el: HTMLElement, binding: OutpuBindingWithResize);
    getId(): string;
    onValueChange(data: unknown): Promise<void>;
    onValueError(err: ErrorsMessageValue): void;
    showProgress(show: boolean): void;
    onResize(): void;
}
export { OutputBindingAdapter };
