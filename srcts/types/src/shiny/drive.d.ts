import type { JSONSchema } from "./driveSchema";
type Access = "read-only" | "read-write" | "write-only";
type InputInfo = {
    id: string;
    label: string;
    type: string;
    access: Access;
    value?: unknown;
    schema?: JSONSchema | null;
    disabled: boolean;
    hidden?: true;
};
type OutputInfo = {
    id: string;
    type: string;
    status: "error" | "ready" | "recalculating";
    text: string;
    image?: string;
    hidden?: true;
};
type Filter = {
    ids?: string[];
    module?: string;
    includeHidden?: boolean;
};
type HiddenOpt = {
    includeHidden?: boolean;
};
declare const previewTextLength = 200;
declare class ShinyDrive {
    /** Never expose inputs using this binding (e.g. `"shiny.passwordInput"`). */
    skipInputBinding(bindingName: string): void;
    /** Never expose outputs using this binding (e.g. `"shiny.htmlOutput"`). */
    skipOutputBinding(bindingName: string): void;
    /**
     * Visible inputs, optionally filtered by exact `ids` and/or `module`
     * (ids inside that module namespace, e.g. `"filters"` for `filters-region`).
     * `includeHidden: true` also lists hidden ones (marked `hidden: true`).
     */
    listInputs(filter?: Filter): InputInfo[];
    getInput(id: string, { includeHidden }?: HiddenOpt): InputInfo;
    /**
     * Update an input's UI widget (as `update*Input()` would), send the value to
     * the server, wait for the app to update, and return the input's new state.
     * Buttons are clicked; `value` is ignored.
     */
    setInput(id: string, value?: unknown, opts?: HiddenOpt): Promise<InputInfo>;
    /**
     * Set several inputs at once, like shinytest2's `AppDriver$set_inputs()`:
     * every value is validated before any widget changes, all values reach the
     * server in one message (so one reactive flush), then waits for the app to
     * update. Buttons are clicked. Returns the inputs' new states, in order.
     */
    setInputs(values: {
        [id: string]: unknown;
    }, { includeHidden }?: HiddenOpt): Promise<InputInfo[]>;
    /**
     * Click an action button or link, wait for the app to update, and return
     * the button's new state.
     */
    click(id: string, opts?: HiddenOpt): Promise<InputInfo>;
    /**
     * Upload files to a file input, as if the user picked them, and resolve
     * once the server has received them and the app has updated. Returns the
     * input's new state (`value` lists the uploaded files).
     */
    uploadFiles(id: string, files: File[] | FileList, { includeHidden, timeout }?: HiddenOpt & {
        timeout?: number;
    }): Promise<InputInfo>;
    /**
     * Resolve once the next upload to file input `id` finishes and the app has
     * updated; reject if it fails or takes longer than `timeout` ms. Call it
     * *before* the upload starts (e.g. before attaching files via DevTools).
     */
    waitForUpload(id: string, { timeout }?: {
        timeout?: number;
    }): Promise<void>;
    /**
     * Visible outputs, optionally filtered by exact `ids` and/or
     * `module` namespace. Text is truncated to a short preview; use getOutput() for the
     * full content.
     * `includeHidden: true` also lists hidden ones (marked `hidden: true`).
     */
    listOutputs(filter?: Filter): Array<Omit<OutputInfo, "image"> & {
        hasImage: boolean;
    }>;
    /**
     * Waits (up to a timeout) for the output to finish recalculating. Hidden
     * outputs (`includeHidden`) are returned as-is: Shiny suspends them, so
     * they may be stale or `recalculating` until shown.
     */
    getOutput(id: string, { includeHidden }?: HiddenOpt): Promise<OutputInfo>;
    /** Resolve once the server has processed all sent inputs and flushed. */
    flush(): Promise<void>;
    /**
     * Like shinytest2's `AppDriver$wait_for_idle()`: after a server `flush()`,
     * resolve once the app has been idle (server not busy, no visible output
     * recalculating) for `duration` ms. Rejects after `timeout` ms.
     */
    waitForIdle({ duration, timeout, }?: {
        duration?: number;
        timeout?: number;
    }): Promise<void>;
}
export { previewTextLength, ShinyDrive };
export type { Filter, InputInfo, JSONSchema, OutputInfo };
