import $ from "jquery";
import type { InputBinding } from "../bindings";
import type { OutputBindingAdapter } from "../bindings/outputAdapter";
import type { ShinyEventInputChanged } from "../events/shinyEvents";
import { isVisible } from "../utils";
import type { JSONSchema } from "./driveSchema";
import { checkValue, valueSchema } from "./driveSchema";
import { shinySetInputValue, shinyShinyApp } from "./initedMethods";

// Public JS API for inspecting and driving a Shiny app's UI, exposed as
// `window.Shiny.drive`. Transport-agnostic: WebMCP
// (`Shiny.webmcp`) is built on it, and browser extensions, Playwright, or an
// MCP Apps bridge can call it directly.
//
// Only what the user can currently see is listed or settable.
//
// Per-element access: `data-shiny-drive-access="exclude" | "read-only" | "write-only"`
// on the bound element (or, for inputs, its `.shiny-input-container`, which
// is where R's pipeable helper puts it). It does not apply to descendants.
// No attribute = the binding's default (usually read-write).

type Access = "read-only" | "read-write" | "write-only";

type InputInfo = {
  id: string;
  label: string;
  type: string;
  access: Access;
  // Omitted for write-only inputs
  value?: unknown;
  // JSON Schema `value` must satisfy in setInputs(); null = button (click it).
  // Omitted for read-only inputs.
  schema?: JSONSchema | null;
  disabled: boolean;
  // Only present (true) for hidden inputs, i.e. with `includeHidden`
  hidden?: true;
};

// One `<img>` or `<canvas>` shown by an output. `index` is its position among
// all of the output's images, in page order.
type OutputImage = { index: number } & (
  | { type: "data" | "url"; value: string }
  | { type: "error"; reason: string }
);

type OutputInfo = {
  id: string;
  type: string;
  status: "error" | "ready" | "recalculating";
  text: string;
  // Number of images the output shows; `images` is one page of them
  imageCount: number;
  imageOffset: number;
  images: OutputImage[];
  // Only present (true) for hidden outputs, i.e. with `includeHidden`. Shiny
  // suspends hidden outputs, so their content may be stale.
  hidden?: true;
};

type OutputSummary = Omit<OutputInfo, "imageOffset" | "images">;

type Filter = { ids?: string[]; module?: string; includeHidden?: boolean };
type HiddenOpt = { includeHidden?: boolean };
type ImagePage = { imageOffset?: number; imageLimit?: number };

// Bindings never exposed. Extend via Shiny.drive.skip*Binding().
const skippedInputBindings = new Set<string>();
const skippedOutputBindings = new Set<string>();

// Default access when an element has no `data-shiny-drive-access` attribute: secrets
// can be typed but never read back; files can be inspected but not uploaded.
const defaultInputAccess = new Map<string, Access>([
  ["shiny.passwordInput", "write-only"],
  ["shiny.fileInputBinding", "read-only"],
]);
const accessAttr = "data-shiny-drive-access";
const accessValues = ["exclude", "read-only", "write-only"];
const maxTextLength = 20000;
const previewTextLength = 200;
const defaultImageLimit = 10;

// Bumped when the API changes, so callers can check for features without
// relying on the package version (development installs may or may not have
// a change).
const apiVersion = 1;

// Milliseconds every wait gives up after, or null to wait as long as it
// takes. Set with Shiny.drive.setDefaultTimeout().
let defaultTimeout: number | null = null;

type DriveErrorCode =
  | "disabled"
  | "disconnected"
  | "invalid-argument"
  | "invalid-value"
  | "not-found"
  | "read-only"
  | "server-error"
  | "timeout"
  | "update-failed"
  | "upload-failed"
  | "wrong-type";

/**
 * Every error `Shiny.drive` throws. `code` says what went wrong, so callers
 * don't need to match on `message`.
 */
class DriveError extends Error {
  code: DriveErrorCode;
  // For "update-failed": ids that were set before the failure
  applied?: string[];

  constructor(
    code: DriveErrorCode,
    message: string,
    { applied }: { applied?: string[] } = {},
  ) {
    super(message);
    this.name = "ShinyDriveError";
    this.code = code;
    if (applied) this.applied = applied;
  }
}

function accessAttrOf(el: HTMLElement, isInput: boolean): string | null {
  const value =
    el.getAttribute(accessAttr) ??
    (isInput
      ? (el.closest(".shiny-input-container")?.getAttribute(accessAttr) ?? null)
      : null);

  if (value !== null && !accessValues.includes(value)) {
    console.warn(
      `Unknown ${accessAttr}="${value}"; expected one of ${accessValues.join(", ")}`,
    );
  }
  return value;
}

function inputAccess(
  el: HTMLElement,
  binding: InputBinding,
): Access | "exclude" {
  const value = accessAttrOf(el, true);

  if (value === "exclude" || value === "read-only" || value === "write-only") {
    return value;
  }
  return defaultInputAccess.get(binding.name) ?? "read-write";
}

function isShownInput(el: HTMLElement): boolean {
  // Selectize/slider hide the bound element; judge by its container. A
  // closed bslib sidebar hides itself; judge it by its layout.
  return isVisible(
    (el.closest(".shiny-input-container") as HTMLElement | null) ??
      (el.classList.contains("bslib-sidebar-input") ? el.parentElement : el) ??
      el,
  );
}

// The message setInputs() passes to the binding's receiveMessage(), as the
// matching update*() function in R would send it. Most bindings take
// `{value}`; bslib's take a `method`.
function inputMessage(binding: InputBinding, value: unknown): unknown {
  switch (binding.name) {
    case "shiny.dateRangeInput":
      return Array.isArray(value)
        ? { value: { start: value[0], end: value[1] } }
        : { value };
    case "bslib.accordion":
      // Like accordion_panel_set()
      return { method: "set", values: value };
    case "bslib.sidebar":
      // Like toggle_sidebar(open = TRUE/FALSE)
      return { method: value ? "open" : "close" };
    default:
      return { value };
  }
}

// Wait for a widget whose value only changes once its animation ends, so the
// value sent to the server is the new one.
async function settleInput(
  el: HTMLElement,
  binding: InputBinding,
): Promise<void> {
  if (binding.name === "bslib.accordion") {
    // Bootstrap adds `show` to a panel only after its `collapsing` animation
    await waitForClasses(
      () => !el.querySelector(":scope > .accordion-item > .collapsing"),
      defaultTimeout,
    );
  }
}

// By default only what the user can currently see: hidden tabs, closed
// conditional panels, and not-yet-rendered dynamic UI are skipped.
// `includeHidden` lifts that, e.g. for tests that set inputs on inactive tabs.
// The WebMCP list tools pass it only to list hidden items, never to set or
// read them. Access control always applies.
function exposedInputs(
  includeHidden = false,
): Array<{ el: HTMLElement; binding: InputBinding }> {
  return $(".shiny-bound-input")
    .toArray()
    .map((el) => ({ el, binding: $(el).data("shiny-input-binding") }))
    .filter(
      ({ el, binding }) =>
        binding &&
        !skippedInputBindings.has(binding.name) &&
        inputAccess(el, binding) !== "exclude" &&
        (includeHidden || isShownInput(el)),
    );
}

function exposedOutputs(includeHidden = false): Array<{
  el: HTMLElement;
  adapter: OutputBindingAdapter;
}> {
  return $(".shiny-bound-output")
    .toArray()
    .map((el) => ({ el, adapter: $(el).data("shiny-output-binding") }))
    .filter(
      ({ el, adapter }) =>
        adapter &&
        !skippedOutputBindings.has(adapter.binding.name) &&
        // Outputs are inherently read-only; only "exclude" applies
        accessAttrOf(el, false) !== "exclude" &&
        (includeHidden || isVisible(el)),
    );
}

function findInput(
  id: string,
  includeHidden = false,
): { el: HTMLElement; binding: InputBinding } {
  const found = exposedInputs(includeHidden).find(
    ({ el, binding }) => binding.getId(el) === id,
  );

  if (!found) {
    throw new DriveError(
      "not-found",
      `No visible input with id "${id}". List the inputs to see the current ids.`,
    );
  }
  return found;
}

function findOutput(
  id: string,
  includeHidden = false,
): {
  el: HTMLElement;
  adapter: OutputBindingAdapter;
} {
  const found = exposedOutputs(includeHidden).find(
    ({ adapter }) => adapter.getId() === id,
  );

  if (!found) {
    throw new DriveError(
      "not-found",
      `No visible output with id "${id}". List the outputs to see the current ids.`,
    );
  }
  return found;
}

function checkTimeout(timeout: number | null): void {
  if (timeout !== null && !(Number.isFinite(timeout) && timeout >= 0)) {
    throw new DriveError(
      "invalid-argument",
      "`timeout` must be a non-negative number of milliseconds, or null",
    );
  }
}

// For waits made of several steps that share one `timeout`: ms left, or null
function timeLeft(timeout: number | null): () => number | null {
  const deadline = timeout === null ? null : Date.now() + timeout;

  return () => (deadline === null ? null : Math.max(0, deadline - Date.now()));
}

function timeoutError(what: string, timeout: number): DriveError {
  return new DriveError("timeout", `${what} within ${timeout}ms`);
}

// Resolve with true once `done()` is true, or false after `timeout` ms.
// Re-checks whenever a `class` attribute changes anywhere in the page (Shiny
// marks progress with `shiny-busy` and `recalculating`) instead of polling:
// browsers throttle timers in background tabs, but not MutationObservers.
function waitForClasses(
  done: () => boolean,
  timeout: number | null,
): Promise<boolean> {
  if (done()) return Promise.resolve(true);
  return new Promise((resolve) => {
    const observer = new MutationObserver(() => {
      if (done()) finish(true);
    });
    const timer =
      timeout === null ? undefined : setTimeout(() => finish(false), timeout);
    const finish = (ok: boolean) => {
      observer.disconnect();
      clearTimeout(timer);
      resolve(ok);
    };

    observer.observe(document.documentElement, {
      attributes: true,
      attributeFilter: ["class"],
      subtree: true,
    });
  });
}

// The flush barrier can't see work the server does in a *later* flush: outputs
// added by insertUI()/renderUI(), or outputs that were hidden (suspended) and
// recompute once the client reports them visible. Wait those out. Gives up
// quietly on timeout; the outputs then show `status: "recalculating"`.
async function waitForOutputs(
  els: () => HTMLElement[],
  timeout = defaultTimeout,
): Promise<void> {
  // Let the client report new visibility (debounced ~100ms) first
  await new Promise((r) => setTimeout(r, 150));
  await waitForClasses(
    () => !els().some((el) => el.classList.contains("recalculating")),
    timeout,
  );
}

function matchesFilter({ ids, module }: Filter, id: string): boolean {
  // `-` is the separator NS() uses between module namespace and id
  return (!ids || ids.includes(id)) && (!module || id.startsWith(module + "-"));
}

// While setInputs() updates widgets, swallow the bindings' own change events.
let applying = false;

// The file binding's getValue() is only for bookmark restore, and Shiny clears
// the <input type=file> once an upload finishes. Remember what was uploaded.
const uploadedFiles = new Map<string, unknown>();

$(document).on("shiny:inputchanged", (e) => {
  const evt = e as unknown as ShinyEventInputChanged;

  if (evt.inputType === "shiny.fileupload") {
    uploadedFiles.set(evt.name, evt.value);
  } else if (applying) {
    e.preventDefault();
  }
});

function inputValue(el: HTMLElement, binding: InputBinding): unknown {
  if (binding.name === "shiny.fileInputBinding") {
    return uploadedFiles.get(binding.getId(el)) ?? [];
  }
  return binding.getValue(el);
}

function inputInfo(el: HTMLElement, binding: InputBinding): InputInfo {
  const id = binding.getId(el);
  const access = inputAccess(el, binding) as Access;

  return {
    id,
    label: labelOf(el, id),
    type: binding.name,
    access,
    value: access === "write-only" ? undefined : inputValue(el, binding),
    schema: access === "read-only" ? undefined : valueSchema(el, binding),
    disabled: el.matches(":disabled"),
    hidden: isShownInput(el) ? undefined : true,
  };
}

function outputInfo(
  el: HTMLElement,
  adapter: OutputBindingAdapter,
): OutputSummary {
  const text = el.innerText ?? "";

  return {
    id: adapter.getId(),
    type: adapter.binding.name,
    status: el.classList.contains("shiny-output-error")
      ? "error"
      : el.classList.contains("recalculating")
        ? "recalculating"
        : "ready",
    text: text.length > maxTextLength ? text.slice(0, maxTextLength) : text,
    imageCount: imageEls(el).length,
    hidden: isVisible(el) ? undefined : true,
  };
}

// The output's own visible <img>/<canvas> elements, in page order. Images in
// a nested output (e.g. a plotOutput() inside a uiOutput()) belong to it.
function imageEls(
  el: HTMLElement,
): Array<HTMLCanvasElement | HTMLImageElement> {
  const shown = isVisible(el);

  return Array.from(
    el.querySelectorAll<HTMLCanvasElement | HTMLImageElement>(
      "img[src], canvas",
    ),
  ).filter(
    (x) => x.closest(".shiny-bound-output") === el && (!shown || isVisible(x)),
  );
}

function imageOf(
  x: HTMLCanvasElement | HTMLImageElement,
  index: number,
): OutputImage {
  if (x instanceof HTMLImageElement) {
    const src = x.currentSrc || x.src;

    // Only base64 data can become MCP image content; the rest is a URL
    return {
      index,
      type: /^data:image\/[^;,]+;base64,/.test(src) ? "data" : "url",
      value: src,
    };
  }

  const error = (reason: string): OutputImage => ({
    index,
    type: "error",
    reason,
  });

  if (x.width === 0 || x.height === 0) {
    return error("Canvas has zero width or height.");
  }
  try {
    if (isSolidColor(x)) {
      return error(
        "Canvas is blank (one solid color). WebGL canvases read as blank unless the widget created them with preserveDrawingBuffer: true.",
      );
    }
    return { index, type: "data", value: x.toDataURL() };
  } catch {
    return error(
      "Canvas can't be read: it draws images from another site that were served without CORS headers.",
    );
  }
}

// Read the pixels of a copy: calling getContext() on the widget's own canvas
// could create a context the widget doesn't expect. A cleared WebGL buffer
// reads as transparent, or opaque black with `alpha: false`.
function isSolidColor(canvas: HTMLCanvasElement): boolean {
  const copy = document.createElement("canvas");

  copy.width = canvas.width;
  copy.height = canvas.height;
  const ctx = copy.getContext("2d", { willReadFrequently: true });

  if (!ctx) return false;
  ctx.drawImage(canvas, 0, 0);
  const pixels = new Uint32Array(
    ctx.getImageData(0, 0, copy.width, copy.height).data.buffer,
  );

  return pixels.every((p) => p === pixels[0]);
}

function checkCount(name: string, x: number): void {
  if (!Number.isInteger(x) || x < 0) {
    throw new DriveError(
      "invalid-argument",
      `\`${name}\` must be a non-negative integer`,
    );
  }
}

function checkWritable(
  id: string,
  el: HTMLElement,
  access: string | null,
): void {
  if (access === "read-only") {
    throw new DriveError("read-only", `Input "${id}" is read-only`);
  }
  if (el.matches(":disabled")) {
    throw new DriveError("disabled", `Input "${id}" is disabled`);
  }
}

function labelOf(el: HTMLElement, id: string): string {
  const label =
    document.getElementById(id + "-label")?.textContent ??
    el.closest("label")?.textContent ??
    el.getAttribute("aria-label") ??
    (el.tagName === "BUTTON" ? el.textContent : null) ??
    "";

  return label.trim() || id;
}

class ShinyDrive {
  /** API version; bumped when the API changes. */
  readonly version = apiVersion;
  #initialized: PromiseLike<void>;

  // Resolves on the first `shiny:sessioninitialized`
  constructor(initialized: PromiseLike<void>) {
    this.#initialized = initialized;
  }

  /**
   * Set how many ms every wait (`flush()`, `setInputs()`, `click()`,
   * `uploadFiles()`, `waitForIdle()`, `ready()`, ...) gives up after; `null`
   * (the default) waits as long as it takes. Methods with a `timeout`
   * option use this when it isn't given. On timeout, waits for the server
   * reject with a `"timeout"` error; waits for outputs give up quietly, and
   * the outputs then show `status: "recalculating"`.
   */
  setDefaultTimeout(timeout: number | null): void {
    checkTimeout(timeout);
    defaultTimeout = timeout;
  }

  /** Never expose inputs using this binding (e.g. `"shiny.passwordInput"`). */
  skipInputBinding(bindingName: string): void {
    skippedInputBindings.add(bindingName);
  }

  /** Never expose outputs using this binding (e.g. `"shiny.htmlOutput"`). */
  skipOutputBinding(bindingName: string): void {
    skippedOutputBindings.add(bindingName);
  }

  /**
   * Visible inputs, optionally filtered by exact `ids` and/or `module`
   * (ids inside that module namespace, e.g. `"filters"` for `filters-region`).
   * `includeHidden: true` also lists hidden ones (marked `hidden: true`).
   */
  listInputs(filter: Filter = {}): InputInfo[] {
    return exposedInputs(filter.includeHidden)
      .filter(({ el, binding }) => matchesFilter(filter, binding.getId(el)))
      .map(({ el, binding }) => inputInfo(el, binding));
  }

  getInput(id: string, { includeHidden }: HiddenOpt = {}): InputInfo {
    const { el, binding } = findInput(id, includeHidden);

    return inputInfo(el, binding);
  }

  /**
   * Update an input's UI widget (as `update*Input()` would), send the value to
   * the server, wait for the app to update, and return the input's new state.
   * Buttons are clicked; `value` is ignored.
   */
  async setInput(
    id: string,
    value?: unknown,
    opts: HiddenOpt = {},
  ): Promise<InputInfo> {
    const [info] = await this.setInputs({ [id]: value }, opts);

    return info;
  }

  /**
   * Set several inputs at once, like shinytest2's `AppDriver$set_inputs()`:
   * every value is validated before any widget changes, all values reach the
   * server in one message (so one reactive flush), then waits for the app to
   * update. Buttons are clicked. Returns the inputs' new states, in order.
   */
  async setInputs(
    values: { [id: string]: unknown },
    { includeHidden }: HiddenOpt = {},
  ): Promise<InputInfo[]> {
    const items = Object.entries(values).map(([id, value]) => {
      const { el, binding } = findInput(id, includeHidden);

      if (binding.name === "shiny.fileInputBinding") {
        throw new DriveError(
          "wrong-type",
          `Input "${id}" is a file input; use uploadFiles()`,
        );
      }
      checkWritable(id, el, inputAccess(el, binding));
      const schema = valueSchema(el, binding);

      if (schema !== null) {
        try {
          checkValue(schema, value);
        } catch (e) {
          throw new DriveError("invalid-value", (e as Error).message);
        }
      }
      return { id, value, el, binding, schema };
    });

    if (items.length === 0) return [];

    // Update the widgets without letting their own change events send values
    // one by one (some are debounced/deferred); we send them all below.
    // No rollback if a widget throws (clicks can't be undone): the ones
    // already updated are still sent, so the page and server agree.
    let applied = 0;
    let failure: unknown;

    applying = true;
    try {
      for (const { value, el, binding, schema } of items) {
        if (schema === null) {
          el.click();
        } else {
          await binding.receiveMessage(el, inputMessage(binding, value));
          await settleInput(el, binding);
        }
        applied++;
      }
    } catch (e) {
      failure = e;
    } finally {
      applying = false;
    }
    const sent = items.slice(0, applied);

    // Queue every value, then the last one's "event" priority sends the whole
    // batch now (bypassing rate policies), so the flush below covers it.
    sent.forEach(({ id, el, binding }, i) => {
      const type = binding.getType(el);

      shinySetInputValue(type ? id + ":" + type : id, binding.getValue(el), {
        priority: i === sent.length - 1 ? "event" : "immediate",
      });
    });
    if (sent.length > 0) {
      await this.flush();
      await waitForOutputs(() => exposedOutputs().map((x) => x.el));
    }
    if (applied < items.length) {
      const reason = failure instanceof Error ? failure.message : failure;
      const done = sent.map((x) => x.id);

      throw new DriveError(
        "update-failed",
        `Input "${items[applied].id}" failed to update: ${reason}. Already applied: ${done.join(", ") || "none"}.`,
        { applied: done },
      );
    }
    return items.map(({ el, binding }) => inputInfo(el, binding));
  }

  /**
   * Click an action button or link, wait for the app to update, and return
   * the button's new state.
   */
  async click(id: string, opts: HiddenOpt = {}): Promise<InputInfo> {
    const { el, binding } = findInput(id, opts.includeHidden);

    if (valueSchema(el, binding) !== null) {
      throw new DriveError(
        "wrong-type",
        `Input "${id}" is not a button; use setInput()`,
      );
    }
    return this.setInput(id, undefined, opts);
  }

  /**
   * Upload files to a file input, as if the user picked them, and resolve
   * once the server has received them and the app has updated. Returns the
   * input's new state (`value` lists the uploaded files).
   */
  async uploadFiles(
    id: string,
    files: File[] | FileList,
    { includeHidden, timeout }: HiddenOpt & { timeout?: number | null } = {},
  ): Promise<InputInfo> {
    const { el, binding } = findInput(id, includeHidden);

    if (binding.name !== "shiny.fileInputBinding") {
      throw new DriveError("wrong-type", `Input "${id}" is not a file input`);
    }
    // File inputs are read-only by default only because agents can't send
    // file contents; an explicit `read-only` still blocks uploads.
    checkWritable(id, el, accessAttrOf(el, true));

    const done = this.waitForUpload(id, { timeout });
    const transfer = new DataTransfer();

    Array.from(files).forEach((f) => transfer.items.add(f));
    (el as HTMLInputElement).files = transfer.files;
    $(el).trigger("change");
    await done;
    return inputInfo(el, binding);
  }

  /**
   * Resolve once the next upload to file input `id` finishes and the app has
   * updated; reject if it fails or takes longer than `timeout` ms. Call it
   * *before* the upload starts (e.g. before attaching files via DevTools).
   */
  async waitForUpload(
    id: string,
    { timeout = defaultTimeout }: { timeout?: number | null } = {},
  ): Promise<void> {
    checkTimeout(timeout);
    await new Promise<void>((resolve, reject) => {
      const handler = (e: JQuery.TriggeredEvent) => {
        const evt = e as JQuery.TriggeredEvent & {
          name: string;
          error: string | null;
        };

        if (evt.name !== id) return;
        cleanup();
        if (evt.error === null) resolve();
        else {
          reject(
            new DriveError(
              "upload-failed",
              `Upload to "${id}" failed: ${evt.error}`,
            ),
          );
        }
      };
      const timer =
        timeout === null
          ? undefined
          : setTimeout(() => {
              cleanup();
              reject(timeoutError(`Upload to "${id}" did not finish`, timeout));
            }, timeout);
      const cleanup = () => {
        clearTimeout(timer);
        $(document).off("shiny:uploadend", handler);
      };

      $(document).on("shiny:uploadend", handler);
    });
    // The upload's final RPC replies before the server's reactive flush
    await this.flush({ timeout });
    await waitForOutputs(() => exposedOutputs().map((x) => x.el), timeout);
  }

  /**
   * Visible outputs, optionally filtered by exact `ids` and/or
   * `module` namespace. Text is truncated to a short preview; use getOutput() for the
   * full content.
   * `includeHidden: true` also lists hidden ones (marked `hidden: true`).
   * Shiny suspends hidden outputs, so their content may be stale until shown.
   */
  listOutputs(filter: Filter = {}): OutputSummary[] {
    return exposedOutputs(filter.includeHidden)
      .filter(({ adapter }) => matchesFilter(filter, adapter.getId()))
      .map(({ el, adapter }) => outputInfo(el, adapter))
      .map((x) => ({ ...x, text: x.text.slice(0, previewTextLength) }));
  }

  /**
   * Waits (up to a timeout) for the output to finish recalculating. Hidden
   * outputs (`includeHidden`) are returned as-is: Shiny suspends them, so
   * they may be stale or `recalculating` until shown.
   *
   * Returns up to `imageLimit` (default 10) of the output's images, starting
   * at `imageOffset`. More remain while `imageOffset + images.length <
   * imageCount`.
   */
  async getOutput(
    id: string,
    {
      includeHidden,
      imageOffset = 0,
      imageLimit = defaultImageLimit,
    }: HiddenOpt & ImagePage = {},
  ): Promise<OutputInfo> {
    checkCount("imageOffset", imageOffset);
    checkCount("imageLimit", imageLimit);
    const { el, adapter } = findOutput(id, includeHidden);

    // Unlike after setInputs(), no visibility change is pending; skip the wait
    // when the output is already settled.
    if (isVisible(el) && el.classList.contains("recalculating")) {
      await waitForOutputs(() => [el]);
    }
    return {
      ...outputInfo(el, adapter),
      imageOffset,
      images: imageEls(el)
        .slice(imageOffset, imageOffset + imageLimit)
        .map((x, i) => imageOf(x, imageOffset + i)),
    };
  }

  // TODO: The reply to `waitForFlush` stands in for "the server flushed"; it
  // carries no flush info itself. Explore an explicit server-to-client flush
  // signal instead (not test mode's empty `values` message, which has the
  // same problem).
  /**
   * Resolve once the server has processed all sent inputs and flushed.
   * Rejects with a `"disconnected"` error if the session is or becomes
   * disconnected, and a `"timeout"` error after `timeout` ms.
   */
  flush({
    timeout = defaultTimeout,
  }: { timeout?: number | null } = {}): Promise<void> {
    checkTimeout(timeout);
    return new Promise((resolve, reject) => {
      const shinyapp = shinyShinyApp();
      const disconnected = () =>
        new DriveError("disconnected", "Shiny is disconnected from the server");

      if (!shinyapp.isConnected()) {
        reject(disconnected());
        return;
      }
      // Without this, a closed socket would leave the request waiting forever
      const onDisconnect = () => finish(() => reject(disconnected()));
      const timer =
        timeout === null
          ? undefined
          : setTimeout(
              () =>
                finish(() =>
                  reject(
                    timeoutError("Shiny did not finish processing", timeout),
                  ),
                ),
              timeout,
            );
      const finish = (settle: () => void) => {
        clearTimeout(timer);
        $(document).off("shiny:disconnected", onDisconnect);
        settle();
      };

      $(document).on("shiny:disconnected", onDisconnect);
      shinyapp.makeRequest(
        "waitForFlush",
        [],
        () => finish(resolve),
        (err) => finish(() => reject(new DriveError("server-error", err))),
        undefined,
      );
    });
  }

  /**
   * Like shinytest2's `AppDriver$wait_for_idle()`: after a server `flush()`,
   * resolve once the app has been idle (server not busy, no visible output
   * recalculating) for `duration` ms. Rejects with a `"timeout"` error after
   * `timeout` ms (default: see `setDefaultTimeout()`).
   */
  async waitForIdle({
    duration = 500,
    timeout = defaultTimeout,
  }: { duration?: number; timeout?: number | null } = {}): Promise<void> {
    checkTimeout(timeout);
    const left = timeLeft(timeout);
    const busy = () =>
      document.documentElement.classList.contains("shiny-busy") ||
      exposedOutputs().some(({ el }) => el.classList.contains("recalculating"));

    await this.flush({ timeout: left() });

    // Wait until not busy, then for `duration` ms without becoming busy
    // again. Busy/idle changes are seen through class changes, not polling.
    for (;;) {
      if (!(await waitForClasses(() => !busy(), left()))) break;
      const quietFor = Math.min(duration, left() ?? duration);

      if (!(await waitForClasses(busy, quietFor))) {
        if (quietFor === duration) return;
        break;
      }
    }
    throw timeoutError("Shiny did not become idle", timeout as number);
  }

  /**
   * Resolve once the session has started and the app has finished its first
   * update (the server has flushed and no visible output is recalculating).
   * Safe to call at any time, even before Shiny connects; later calls cost
   * one round trip to the server. Rejects like `flush()`.
   */
  async ready({
    timeout = defaultTimeout,
  }: { timeout?: number | null } = {}): Promise<void> {
    checkTimeout(timeout);
    const left = timeLeft(timeout);
    let timer: ReturnType<typeof setTimeout> | undefined;

    await Promise.race([
      this.#initialized,
      new Promise<never>((resolveNever, reject) => {
        if (timeout === null) return;
        timer = setTimeout(
          () => reject(timeoutError("Shiny did not start", timeout)),
          timeout,
        );
      }),
    ]).finally(() => clearTimeout(timer));
    await this.flush({ timeout: left() });
    await waitForOutputs(() => exposedOutputs().map((x) => x.el), left());
  }
}

export { defaultImageLimit, DriveError, previewTextLength, ShinyDrive };
export type {
  Filter,
  InputInfo,
  JSONSchema,
  OutputImage,
  OutputInfo,
  OutputSummary,
};
