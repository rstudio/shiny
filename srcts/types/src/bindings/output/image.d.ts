import type { CoordmapInit } from "../../imageutils/initCoordmap";
import type { ErrorsMessageValue } from "../../shiny/shinyapp";
import { OutputBinding } from "./outputBinding";
declare class ImageOutputBinding extends OutputBinding {
    find(scope: HTMLElement): JQuery<HTMLElement>;
    renderValue(el: HTMLElement, data: {
        coordmap: CoordmapInit;
        error?: string;
    } & {
        [key: string]: string;
    }): void;
    renderError(el: HTMLElement, err: ErrorsMessageValue): void;
    clearError(el: HTMLElement): void;
    resize(el: HTMLElement, width: number | string, height: number | string): void;
}
declare const imageOutputBinding: ImageOutputBinding;
export { imageOutputBinding, ImageOutputBinding };
