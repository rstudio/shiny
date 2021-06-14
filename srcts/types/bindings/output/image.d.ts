import { OutputBinding } from "./OutputBinding";
import type { CoordmapInitType } from "../../imageutils/initCoordmap";
import type { errorsMessageValue } from "../../shiny/shinyapp";
declare class ImageOutputBinding extends OutputBinding {
    find(scope: HTMLElement): JQuery<HTMLElement>;
    renderValue(el: HTMLElement, data: {
        coordmap: CoordmapInitType;
        error?: string;
    } & Record<string, string>): void;
    renderError(el: HTMLElement, err: errorsMessageValue): void;
    clearError(el: HTMLElement): void;
    resize(el: HTMLElement, width: string | number, height: string | number): void;
}
declare const imageOutputBinding: ImageOutputBinding;
export { imageOutputBinding, ImageOutputBinding };
