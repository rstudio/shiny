import type { ErrorsMessageValue } from "../../shiny/shinyapp";
type OutputSnapshot = {
    images?: string[];
    data?: unknown;
};
declare class OutputBinding {
    name: string;
    find(scope: HTMLElement | JQuery<HTMLElement>): JQuery<HTMLElement>;
    renderValue(el: HTMLElement, data: unknown): Promise<void> | void;
    /**
     * Optional: what `Shiny.drive.getOutput()` (and the WebMCP `get_output`
     * tool) should return for this output beyond its text, when reading the
     * page isn't enough, e.g. an image of a chart drawn in SVG or WebGL, or a
     * table's rows. Return `undefined` (the default) to read the page.
     */
    getSnapshot(el: HTMLElement): OutputSnapshot | Promise<OutputSnapshot | undefined> | undefined;
    getId(el: HTMLElement): string;
    onValueChange(el: HTMLElement, data: unknown): Promise<void>;
    onValueError(el: HTMLElement, err: ErrorsMessageValue): void;
    renderError(el: HTMLElement, err: ErrorsMessageValue): void;
    clearError(el: HTMLElement): void;
    showProgress(el: HTMLElement, show: boolean): void;
}
export { OutputBinding };
export type { OutputSnapshot };
