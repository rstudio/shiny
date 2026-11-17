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
type OutputImage = {
    index: number;
} & ({
    type: "data" | "url";
    value: string;
} | {
    type: "error";
    reason: string;
});
type OutputInfo = {
    id: string;
    type: string;
    status: "error" | "ready" | "recalculating";
    text: string;
    imageCount: number;
    imageOffset: number;
    images: OutputImage[];
    hidden?: true;
};
type OutputSummary = Omit<OutputInfo, "imageOffset" | "images">;
type Filter = {
    ids?: string[];
    module?: string;
    includeHidden?: boolean;
};
type HiddenOpt = {
    includeHidden?: boolean;
};
type ImagePage = {
    imageOffset?: number;
    imageLimit?: number;
};
declare const previewTextLength = 200;
declare const defaultImageLimit = 10;
type DriveErrorCode = "disabled" | "disconnected" | "invalid-argument" | "invalid-value" | "not-found" | "read-only" | "server-error" | "timeout" | "update-failed" | "upload-failed" | "wrong-type";
/**
 * Every error `Shiny.drive` throws. `code` says what went wrong, so callers
 * don't need to match on `message`.
 */
declare class DriveError extends Error {
    code: DriveErrorCode;
    applied?: string[];
    constructor(code: DriveErrorCode, message: string, { applied }?: {
        applied?: string[];
    });
}
declare class ShinyDrive {
    #private;
    /** API version; bumped when the API changes. */
    readonly version = 1;
    constructor(initialized: PromiseLike<void>);
    /**
     * Set how many ms every wait (`flush()`, `setInputs()`, `click()`,
     * `uploadFiles()`, `waitForIdle()`, `ready()`, ...) gives up after; `null`
     * (the default) waits as long as it takes. Methods with a `timeout`
     * option use this when it isn't given. On timeout, waits for the server
     * reject with a `"timeout"` error; waits for outputs give up quietly, and
     * the outputs then show `status: "recalculating"`.
     */
    setDefaultTimeout(timeout: number | null): void;
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
        timeout?: number | null;
    }): Promise<InputInfo>;
    /**
     * Resolve once the next upload to file input `id` finishes and the app has
     * updated; reject if it fails or takes longer than `timeout` ms. Call it
     * *before* the upload starts (e.g. before attaching files via DevTools).
     */
    waitForUpload(id: string, { timeout }?: {
        timeout?: number | null;
    }): Promise<void>;
    /**
     * Visible outputs, optionally filtered by exact `ids` and/or
     * `module` namespace. Text is truncated to a short preview; use getOutput() for the
     * full content.
     * `includeHidden: true` also lists hidden ones (marked `hidden: true`).
     * Shiny suspends hidden outputs, so their content may be stale until shown.
     */
    listOutputs(filter?: Filter): OutputSummary[];
    /**
     * Waits (up to a timeout) for the output to finish recalculating. Hidden
     * outputs (`includeHidden`) are returned as-is: Shiny suspends them, so
     * they may be stale or `recalculating` until shown.
     *
     * Returns up to `imageLimit` (default 10) of the output's images, starting
     * at `imageOffset`. More remain while `imageOffset + images.length <
     * imageCount`.
     */
    getOutput(id: string, { includeHidden, imageOffset, imageLimit, }?: HiddenOpt & ImagePage): Promise<OutputInfo>;
    /**
     * Resolve once the server has processed all sent inputs and flushed.
     * Rejects with a `"disconnected"` error if the session is or becomes
     * disconnected, and a `"timeout"` error after `timeout` ms.
     */
    flush({ timeout, }?: {
        timeout?: number | null;
    }): Promise<void>;
    /**
     * Like shinytest2's `AppDriver$wait_for_idle()`: after a server `flush()`,
     * resolve once the app has been idle (server not busy, no visible output
     * recalculating) for `duration` ms. Rejects with a `"timeout"` error after
     * `timeout` ms (default: see `setDefaultTimeout()`).
     */
    waitForIdle({ duration, timeout, }?: {
        duration?: number;
        timeout?: number | null;
    }): Promise<void>;
    /**
     * Resolve once the session has started and the app has finished its first
     * update (the server has flushed and no visible output is recalculating).
     * Safe to call at any time, even before Shiny connects; later calls cost
     * one round trip to the server. Rejects like `flush()`.
     */
    ready({ timeout, }?: {
        timeout?: number | null;
    }): Promise<void>;
}
export { defaultImageLimit, DriveError, previewTextLength, ShinyDrive };
export type { Filter, InputInfo, JSONSchema, OutputImage, OutputInfo, OutputSummary, };
