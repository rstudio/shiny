import { OutputBinding } from "./OutputBinding";
declare class ImageOutputBinding extends OutputBinding {
    find(scope: HTMLElement): JQuery<HTMLElement>;
    renderValue(el: HTMLElement, data: any): void;
    renderError(el: HTMLElement, err: any): void;
    clearError(el: any): void;
    resize(el: HTMLElement, width: string | number, height: string | number): void;
}
declare const imageOutputBinding: ImageOutputBinding;
export { imageOutputBinding, ImageOutputBinding };
