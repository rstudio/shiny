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
  // JSON Schema `value` must satisfy in set_input; null = button (click it).
  // Omitted for read-only inputs.
  schema?: JSONSchema | null;
  disabled: boolean;
  // Only present (true) for hidden inputs, i.e. with `includeHidden`
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

type Filter = { ids?: string[]; module?: string; includeHidden?: boolean };
type HiddenOpt = { includeHidden?: boolean };

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
const flushTimeoutMs = 10000;

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
  // Selectize/slider hide the bound element; judge by its container
  return isVisible(
    (el.closest(".shiny-input-container") as HTMLElement | null) ?? el,
  );
}

// By default only what the user can currently see: hidden tabs, closed
// conditional panels, and not-yet-rendered dynamic UI are skipped.
// `includeHidden` (JS API only, never the agent tools) lifts that, e.g. for
// tests that set inputs on inactive tabs. Access control always applies.
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
    throw new Error(
      `No visible input with id "${id}". Call list_inputs to see the current inputs.`,
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
    throw new Error(
      `No visible output with id "${id}". Call list_outputs to see the current outputs.`,
    );
  }
  return found;
}

// The flush barrier can't see work the server does in a *later* flush: outputs
// added by insertUI()/renderUI(), or outputs that were hidden (suspended) and
// recompute once the client reports them visible. Wait those out.
async function waitForOutputs(els: () => HTMLElement[]): Promise<void> {
  const deadline = Date.now() + flushTimeoutMs;

  // ponytail: polling; a 'shiny:value'/'shiny:idle' listener if it matters.
  while (Date.now() < deadline) {
    // Let the client report new visibility (debounced ~100ms) first
    await new Promise((r) => setTimeout(r, 150));
    if (!els().some((el) => el.classList.contains("recalculating"))) return;
  }
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
): OutputInfo {
  const img = el.querySelector("img");
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
    image: img?.src.startsWith("data:image/") ? img.src : undefined,
    hidden: isVisible(el) ? undefined : true,
  };
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
        throw new Error(`Input "${id}" is a file input; use uploadFiles()`);
      }
      if (inputAccess(el, binding) === "read-only") {
        throw new Error(`Input "${id}" is read-only`);
      }
      if (el.matches(":disabled")) throw new Error(`Input "${id}" is disabled`);
      const schema = valueSchema(el, binding);

      if (schema !== null) checkValue(schema, value);
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
          const msgValue =
            binding.name === "shiny.dateRangeInput" && Array.isArray(value)
              ? { start: value[0], end: value[1] }
              : value;

          await binding.receiveMessage(el, { value: msgValue });
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
      const done = sent.map((x) => x.id).join(", ") || "none";

      throw new Error(
        `Input "${items[applied].id}" failed to update: ${reason}. Already applied: ${done}.`,
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
      throw new Error(`Input "${id}" is not a button; use setInput()`);
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
    { includeHidden, timeout }: HiddenOpt & { timeout?: number } = {},
  ): Promise<InputInfo> {
    const { el, binding } = findInput(id, includeHidden);

    if (binding.name !== "shiny.fileInputBinding") {
      throw new Error(`Input "${id}" is not a file input`);
    }
    // File inputs are read-only by default only because agents can't send
    // file contents; an explicit `read-only` still blocks uploads.
    if (accessAttrOf(el, true) === "read-only") {
      throw new Error(`Input "${id}" is read-only`);
    }
    if (el.matches(":disabled")) throw new Error(`Input "${id}" is disabled`);

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
    { timeout = 30000 }: { timeout?: number } = {},
  ): Promise<void> {
    await new Promise<void>((resolve, reject) => {
      const handler = (e: JQuery.TriggeredEvent) => {
        const evt = e as JQuery.TriggeredEvent & {
          name: string;
          error: string | null;
        };

        if (evt.name !== id) return;
        cleanup();
        if (evt.error === null) resolve();
        else reject(new Error(`Upload to "${id}" failed: ${evt.error}`));
      };
      const timer = setTimeout(() => {
        cleanup();
        reject(
          new Error(`Upload to "${id}" did not finish within ${timeout}ms`),
        );
      }, timeout);
      const cleanup = () => {
        clearTimeout(timer);
        $(document).off("shiny:uploadend", handler);
      };

      $(document).on("shiny:uploadend", handler);
    });
    // The upload's final RPC replies before the server's reactive flush
    await this.flush();
    await waitForOutputs(() => exposedOutputs().map((x) => x.el));
  }

  /**
   * Visible outputs, optionally filtered by exact `ids` and/or
   * `module` namespace. Text is truncated to a short preview; use getOutput() for the
   * full content.
   * `includeHidden: true` also lists hidden ones (marked `hidden: true`).
   */
  listOutputs(
    filter: Filter = {},
  ): Array<Omit<OutputInfo, "image"> & { hasImage: boolean }> {
    return exposedOutputs(filter.includeHidden)
      .filter(({ adapter }) => matchesFilter(filter, adapter.getId()))
      .map(({ el, adapter }) => outputInfo(el, adapter))
      .map(({ image, ...x }) => ({
        ...x,
        text: x.text.slice(0, previewTextLength),
        hasImage: image !== undefined,
      }));
  }

  /**
   * Waits (up to a timeout) for the output to finish recalculating. Hidden
   * outputs (`includeHidden`) are returned as-is: Shiny suspends them, so
   * they may be stale or `recalculating` until shown.
   */
  async getOutput(
    id: string,
    { includeHidden }: HiddenOpt = {},
  ): Promise<OutputInfo> {
    const { el, adapter } = findOutput(id, includeHidden);

    // Unlike after setInputs(), no visibility change is pending; skip the wait
    // when the output is already settled.
    if (isVisible(el) && el.classList.contains("recalculating")) {
      await waitForOutputs(() => [el]);
    }
    return outputInfo(el, adapter);
  }

  // TODO: The reply to `waitForFlush` stands in for "the server flushed"; it
  // carries no flush info itself. Explore an explicit server-to-client flush
  // signal instead (not test mode's empty `values` message, which has the
  // same problem).
  /** Resolve once the server has processed all sent inputs and flushed. */
  flush(): Promise<void> {
    return new Promise((resolve, reject) => {
      const shinyapp = shinyShinyApp();

      if (!shinyapp.isConnected()) {
        reject(new Error("Shiny is disconnected from the server"));
        return;
      }
      const timer = setTimeout(
        () =>
          reject(
            new Error(
              `Shiny did not finish processing within ${flushTimeoutMs}ms`,
            ),
          ),
        flushTimeoutMs,
      );

      shinyapp.makeRequest(
        "waitForFlush",
        [],
        () => {
          clearTimeout(timer);
          resolve();
        },
        (err) => {
          clearTimeout(timer);
          reject(new Error(err));
        },
        undefined,
      );
    });
  }

  /**
   * Like shinytest2's `AppDriver$wait_for_idle()`: after a server `flush()`,
   * resolve once the app has been idle (server not busy, no visible output
   * recalculating) for `duration` ms. Rejects after `timeout` ms.
   */
  async waitForIdle({
    duration = 500,
    timeout = 10000,
  }: { duration?: number; timeout?: number } = {}): Promise<void> {
    const deadline = Date.now() + timeout;

    await this.flush();

    let idleSince = Date.now();

    while (Date.now() - idleSince < duration) {
      if (Date.now() > deadline) {
        throw new Error(`Shiny did not become idle within ${timeout}ms`);
      }
      await new Promise((r) => setTimeout(r, 50));
      const busy =
        document.documentElement.classList.contains("shiny-busy") ||
        exposedOutputs().some(({ el }) =>
          el.classList.contains("recalculating"),
        );

      if (busy) idleSince = Date.now();
    }
  }
}

export { previewTextLength, ShinyDrive };
export type { Filter, InputInfo, JSONSchema, OutputInfo };
