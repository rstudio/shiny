# jQuery Wrapper: Capability-Oriented Adapter Layer

**Status:** Design
**Date:** 2026-06-03
**Branch:** `feat/jquery-removal`

## Goal

Build a wrapper layer that contains Shiny's jQuery dependency behind small, capability-oriented TypeScript interfaces, so that:

1. jQuery's reach is bounded and auditable (one directory, build-time enforced).
2. Each operation Shiny performs has a native implementation as well as a jQuery implementation; the right one is selected at call time depending on whether `window.jQuery` is present.
3. jQuery becomes an **optional peer dependency**. Code paths that genuinely require jQuery (jQuery-plugin widgets, the legacy binding API) throw a structured, actionable error when jQuery is absent.
4. Shiny's own code can eventually be ported to native-only on a module-by-module basis without coordinated big-bang change.

Long-term default is native. jQuery support remains as long as jQuery-dependent widgets ship with Shiny.

## Non-goals

- Replacing the jQuery-plugin widgets themselves (datepicker, ion-rangeslider, selectize, DataTables). Sketched as future work in §6.
- Removing `Shiny.$` from the public API.
- Removing the deprecated jQuery-typed `InputBinding.find()` / `OutputBinding.find()` methods. They stay functional indefinitely.
- Dynamically loading jQuery from a CDN when absent. The wrapper throws; the host page decides how to provide jQuery.

## 1. Architecture

**Pattern:** Ports & Adapters (Hexagonal). Each capability is a small port — a native-typed TypeScript interface describing what Shiny needs to do. Each port has two adapters: native and jQuery-backed. A single exported dispatch function per operation performs the per-call selection:

```ts
export function setHtml(el: Element, html: string): void {
  if (typeof window.jQuery === "function") {
    jqueryAdapter.setHtml(el, html);
  } else {
    nativeAdapter.setHtml(el, html);
  }
}
```

**Location:** `srcts/src/dom/`. Subdirectories per module:

```
srcts/src/dom/
  query/       index.ts  jquery.ts  native.ts  *.test.ts
  events/     ...
  html/       ...
  ajax/       ...
  widgets/    ...
  testing/     withJQuery.ts
  errors.ts    (JQueryRequiredError)
```

**Invariant (CLEAN constraint):** outside `srcts/src/dom/*/jquery.ts`, no source file may `import $ from "jquery"` or reference `JQuery<…>` types. Enforced by build-time check (§4).

## 2. Module surface

All dispatch functions take and return native DOM types. `JQuery<T>` exists only inside `*/jquery.ts` files.

### `dom/query` — selection and traversal
```ts
select(scope: ParentNode, selector: string): Element[]
closest(el: Element, selector: string): Element | null
matches(el: Element, selector: string): boolean
```

- Native adapter: `querySelectorAll` / `Element.closest` / `Element.matches`.
- jQuery adapter: `$(scope).find(selector).toArray()` etc. Preserves jQuery selector extensions (`:visible` etc.) when callers happen to use them.

### `dom/events` — listeners, delegation, dispatch
```ts
type Unsubscribe = () => void;

on(
  target: EventTarget,
  type: string,
  handler: (e: Event) => void,
  opts?: { selector?: string; namespace?: string },
): Unsubscribe;

trigger(target: EventTarget, type: string, detail?: unknown): void;
```

- Returns an `Unsubscribe` rather than exposing a separate `off()` — namespace handling stays internal to the adapter.
- jQuery adapter preserves jQuery's `.on("ns.foo", …)` semantics so widgets that fire/listen via namespaced events keep working.
- Native adapter implements delegation by walking `event.target` up to `currentTarget` and calling `matches`.

### `dom/html` — HTML insertion with `<script>` execution
```ts
setHtml(el: Element, html: string): void
append(el: Element, html: string): void
prepend(el: Element, html: string): void
parseHtmlFragment(html: string): DocumentFragment
```

This is the load-bearing module. Several Shiny call sites rely on jQuery's behavior of executing `<script>` tags contained in inserted HTML (`singletons.ts`, `shinyapp.ts` dynamic UI).

- jQuery adapter delegates to `$(el).html()` etc.
- Native adapter parses the HTML, inserts the nodes, then walks for `<script>` elements and **re-creates** each one so the browser executes it (the browser does not execute scripts parsed from `innerHTML` without this).

### `dom/ajax` — XHR
```ts
ajax(opts: {
  url: string;
  method: "GET" | "POST" | "PUT" | "DELETE";
  data?: BodyInit | null;
  headers?: Record<string, string>;
  onProgress?: (e: ProgressEvent) => void;
  onSuccess: (response: unknown) => void;
  onError: (err: Error) => void;
}): { abort(): void };
```

- Both adapters expose the same shape.
- Native adapter uses `XMLHttpRequest` (upload-progress events are required by `fileProcessor.ts` and not available from `fetch` cleanly).
- jQuery adapter delegates to `$.ajax`; current `fileProcessor.ts` integrations with jQuery's xhr object are preserved here.

### `dom/widgets` — jQuery-plugin widget integration
```ts
type WidgetName = "datepicker" | "slider" | "selectize" | "datatable";

initWidget(name: WidgetName, el: Element, config: unknown): WidgetHandle
destroyWidget(handle: WidgetHandle): void
onWidgetChange(handle: WidgetHandle, cb: () => void): Unsubscribe
```

These widgets ARE jQuery plugins; there is no native adapter today. The module exists to give them an explicit, intent-revealing seam.

- jQuery present: delegates to the underlying jQuery plugin.
- jQuery absent: `initWidget` throws `JQueryRequiredError(feature, binding)` (see §3.5).

## 3. Public binding API (dual contract)

Today: `InputBinding.find(scope)` returns `JQuery<HTMLElement>`; `BindScope = HTMLElement | JQuery<HTMLElement>`. User-authored custom bindings rely on this.

### New native-typed methods

```ts
abstract class InputBinding {
  /** @deprecated use findElements; will warn on first use */
  find(scope: JQuery<HTMLElement>): JQuery<HTMLElement>;

  findElements(scope: ParentNode): Element[];
}
```

Same pattern for `OutputBinding`. Default `findElements` implementation in the base class falls back to calling legacy `find()` and unwrapping via `dom/query`.

### Resolution order

Shiny's internal bind loop calls `findElements` first. The base-class default detects "this subclass overrode `find` but not `findElements`" (by comparing method identity against the base class) and routes through the legacy path with a one-time `console.warn` naming the binding subclass.

### `BindScope` type

```ts
// Before:
type BindScope = HTMLElement | JQuery<HTMLElement>;

// After:
type BindScope = ParentNode;  // Document | DocumentFragment | Element
```

`bindAll`, `unbindAll`, `initializeInputs` get a `.d.ts` overload that additionally accepts `JQuery<HTMLElement>` and immediately unwraps at the entry point. Existing call sites do not break.

### Unchanged methods

`getValue`, `setValue`, `subscribe`, `unsubscribe`, `receiveMessage` already use native `HTMLElement`. No change.

## 3.5. jQuery as optional peer dependency

### `package.json`

```jsonc
"peerDependencies": {
  "jquery": ">=3.0.0"
},
"peerDependenciesMeta": {
  "jquery": { "optional": true }
}
```

`jquery` moves out of `dependencies`. The existing esbuild `external_libs` mapping (`jquery → window.jQuery`) is retained — Shiny continues to not bundle jQuery. The host page provides jQuery (R Shiny's existing HTML dependency does this) or omits it.

`@types/jquery` stays in `devDependencies` so adapter files type-check.

### Three runtime states

| State | Dispatch behavior |
|---|---|
| jQuery present, native path exists | jQuery adapter (zero-regression default) |
| jQuery absent, native path exists | Native adapter |
| jQuery absent, **no** native path | Throw `JQueryRequiredError` |

### The error

```ts
// srcts/src/dom/errors.ts
export class JQueryRequiredError extends Error {
  constructor(public feature: string, public binding?: string) {
    super(
      `Shiny feature "${feature}"` +
      (binding ? ` (used by binding "${binding}")` : "") +
      ` requires jQuery, which is not loaded on this page. ` +
      `Load jQuery 3.x before Shiny initializes, ` +
      `or replace the binding with a native-compatible alternative.`,
    );
    this.name = "JQueryRequiredError";
  }
}
```

Thrown from:
- `dom/widgets` (always, when jQuery is absent).
- Legacy `InputBinding.find()` / `OutputBinding.find()` base-class fallback, when jQuery is absent and the subclass overrode only legacy `find`. Error names the binding's `constructor.name`.
- Any `dom/events` call that uses jQuery-only namespacing or special events (callers must use the supported subset when jQuery is absent).

### Introspection helper

```ts
Shiny.requiresJQuery(): Array<{ feature: string; reason: string }>;
```

Returns the declared jQuery requirements of currently registered bindings and widgets. Tools and downstream packages can call this at boot to detect whether the loaded set is jQuery-free.

### Refined binding fallback (Section 3 + 3.5)

When a custom binding overrides only legacy `find()`:
1. jQuery present → unwrap result, one-time deprecation `console.warn`.
2. jQuery absent → throw `JQueryRequiredError(feature: "InputBinding.find", binding: this.constructor.name)`.

Better failure mode than a silent broken input.

## 4. Build-time enforcement

### Extend `srcts/build/_jquery.ts`

Existing rule (kept): files that use `$` must `import $ from "jquery"`.

New rule: `import $ from "jquery"` and `JQuery<…>` type references are only permitted in `srcts/src/dom/*/jquery.ts` adapter files.

```ts
const ADAPTER_PATH_RE = /^srcts\/src\/dom\/[^/]+\/jquery\.ts$/;

for (const file of allTsFiles) {
  const inAdapter = ADAPTER_PATH_RE.test(file);
  const importsJquery = /^import \$ from "jquery";?$/m.test(content);
  const usesJqueryType = /\bJQuery\s*</.test(content);
  const exempted = /^\/\/ shiny:jquery-allowed -- .+$/m.test(content);

  if (!inAdapter && !exempted && (importsJquery || usesJqueryType)) {
    fail(
      `${file}: jQuery imports and JQuery<> types are only allowed in ` +
      `srcts/src/dom/*/jquery.ts adapter files. Use a dom/* dispatch ` +
      `function instead.`,
    );
  }
}
```

Runs as part of `npm run bundle_shiny` (already wired) and in CI.

### Escape hatch

A single-line magic comment `// shiny:jquery-allowed -- <reason>` at the top of a file exempts it from the new rule. The build script greps for all exemption markers and prints the full list at the end of each build, so the exemption set is visible drift, not invisible.

Known exemptions at design time:
- `srcts/src/extras/globalShiny.d.ts` — declares the type of `Shiny.$`.
- Files containing the deprecated `InputBinding`/`OutputBinding.find()` signatures — they intentionally reference `JQuery<HTMLElement>` for the legacy contract.

### `Shiny.$` stays

`Shiny.$` continues to be assigned `window.jQuery` at boot if present, `undefined` otherwise. Documented as deprecated. Not removed in this design.

### esbuild config

No change. `external_libs.ts` already maps `jquery → window.jQuery`. Adapter files use that mapping; non-adapter files never import `"jquery"` so the mapping is moot for them.

## 5. Testing strategy

### Per-module dual-path tests

```ts
// srcts/src/dom/testing/withJQuery.ts
export function withJQuery(
  present: boolean,
  fn: () => void | Promise<void>,
) {
  const saved = (window as any).jQuery;
  (window as any).jQuery = present ? require("jquery") : undefined;
  try { return fn(); } finally { (window as any).jQuery = saved; }
}
```

Each module's tests assert the same observable behavior under both `withJQuery(true, ...)` and `withJQuery(false, ...)`. Where behavior legitimately diverges (e.g., jQuery `:visible`), the native test asserts a clear error rather than silent miscompare.

### Three new test categories

1. **Adapter parity tests** — observable behavior matches across adapters for every operation that has both.
2. **`<script>` execution tests** — `dom/html.setHtml` / `append` / `prepend` correctly execute embedded `<script>` tags under both adapters. This protects `singletons.ts` and the dynamic-UI insertion in `shinyapp.ts`.
3. **Error-surface tests** — `dom/widgets.initWidget("datepicker", …)` with `window.jQuery = undefined` throws `JQueryRequiredError` with the right `feature` and `binding` fields. Legacy `InputBinding.find()` fallback throws when appropriate.

### Smoketest

Add one variant of an existing `smoketests/` app that loads without `window.jQuery` and asserts that core inputs (text, checkbox, action button, slider IF native-replaced, etc.) function. Regression net for the per-call dispatch.

### No new framework

Reuse the existing `srcts/` test runner.

## 6. Migration plan

The 41 jQuery-importing files port one capability module at a time. Each step is independently mergeable.

| # | Step | Notes |
|---|---|---|
| 1 | Scaffold `srcts/src/dom/`, extend `_jquery.ts` build rule, add `JQueryRequiredError`, port `dom/query` call sites (~10–15 files). | Simplest, most repetitive jQuery use. |
| 2 | `dom/events` — port event-binding sites. | Watch for jQuery-namespaced events that widgets depend on; document any that must stay routed through `dom/widgets`. |
| 3 | `dom/html` — port `singletons.ts` and dynamic-UI insertion in `shinyapp.ts`. | **Risky.** Extra integration testing. |
| 4 | `dom/ajax` — port `fileProcessor.ts`, `downloadlink.ts`. | Small blast radius. |
| 5 | `dom/widgets` — wrap existing jQuery-plugin call sites in the widget interface (`daterange`, `slider`, `selectInput`, `datatable`). | Does not reduce jQuery use; *names* it. |
| 6 | Public binding API (§3) — add `findElements`, base-class fallback with deprecation warning, `BindScope` widening. | Can land in parallel with step 5. |
| 7 | `package.json`: jquery → optional peer dependency. | **Last.** Only after all internal jQuery usage is behind the wrapper and the smoketest passes without jQuery. |

Each step ships its own PR with: moved files, new adapter, dual-path tests, `NEWS.md` entry, and a checked build-rule audit.

## 7. Future work (outline)

Replacing the jQuery-plugin widgets is out of scope for this design. With the wrapper in place, each can be tackled independently. Order by tractability:

### a. `bootstrap-datepicker` → native `<input type="date">` (`date.ts`, `daterange.ts`)
- Modern browsers all support `type="date"` with i18n.
- Missing pieces: custom date format strings, explicit min/max constraints — both straightforward.
- Breakage profile: visual change (browser-native picker UI); apps customizing the bootstrap-datepicker DOM.
- Rollout: native implementation behind `dom/widgets.initWidget("datepicker", …)`; flag-selected for one release, then default-on, then jQuery adapter removed.

### b. `ion-rangeslider` → native `<input type="range">` + custom rendering (`slider.ts`)
- Native `range` is universally supported but visually plain and single-thumb.
- Shiny's slider supports range (two thumbs), labels, ticks, prettifiers.
- Realistic replacement: a small custom component wrapping one or two native ranges with a custom track. ~300 lines.
- Breakage profile: appearance (needs design pass); customizers of ion-rangeslider's DOM. `input$slider` value unchanged.
- Same dual-flag rollout as datepicker.

### c. `selectize.js` → headless combobox library (`selectInput.ts`)
- Deepest integration: tagging, server-side search, custom rendering.
- Modern equivalent: native `<select>` for the simple case; a headless combobox (custom or a small library like Downshift logic ported to vanilla TS) for the complex cases.
- Breakage profile: selectize-specific options (`create`, `persist`, custom `render`) won't translate 1:1. Warrants a deprecation cycle mapping common options forward and warning on the rest.
- Larger effort than (a) or (b). Own design doc.

### d. `DataTables` → server-paginated native table (`datatable.ts`)
- DataTables-the-jQuery-plugin is one of two paths for `DT::renderDataTable`.
- Practical path is **not** to replace DataTables itself (kept available for users who opt in via the `DT` R package), but to make Shiny's *bundled* table output use a native paginated/sortable component, and require DataTables only when the user explicitly opts in.
- Mostly an R-side decision (default renderer in the R package); the JS side just stops auto-loading DataTables.

### Cross-cutting prerequisite: Bootstrap version

Shiny's modals, notifications, tabs, and tooltips currently route through Bootstrap 3/4, which require jQuery. Bootstrap 5 dropped jQuery. Migrating those concerns to Bootstrap-5-compatible APIs (or direct native equivalents) is a precondition for any Shiny page being truly jQuery-free — partly a `bslib` decision, not this repo.

The wrapper accommodates either: as long as call sites go through `dom/events`, `dom/html`, etc., they don't care whether Bootstrap 3/4 (jQuery) or 5 (native) is on the page.

### Order rationale

Datepicker first (unambiguous native replacement); slider second (bounded, self-contained); selectize and DataTables last (largest design questions). Each step compounds: `Shiny.requiresJQuery()` (§3.5) reports one fewer hard requirement as each widget gains a native adapter, and downstream packages can rely on the shrinking list.
