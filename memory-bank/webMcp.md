---
title: "WebMCP and the Shiny.drive JS API"
description: "How window.Shiny.drive (public JS methods to read and drive inputs/outputs) and window.Shiny.webmcp (WebMCP tool registry and the four built-in tools) work, typed errors, timeouts and waiting, what hidden means, the data-shiny-drive-access attribute and webMcpAccess(), the server-side flush barrier, and the R-only scope. Read before changing srcts/src/shiny/drive.ts, srcts/src/shiny/webmcp.ts, or ShinySession$dispatch()."
---

# WebMCP and the `Shiny.drive` JS API

Experimental. Lets agents (through [WebMCP](https://webmcp.com/)) and any other
JS caller (Playwright, browser extensions, shinytest2) read and drive a running
app's UI.

## Scope: R only for now

This feature is R-only. Do **not** open py-shiny issues or PRs for it, and
don't mirror its tool names, attributes or access modes into py-shiny. The
maintainer will start the py-shiny work by hand later; py-shiny's own WebMCP
PR (posit-dev/py-shiny#2513) has a different shape and will be adjusted to
match R then.

Because py-shiny vendors `inst/www/shared/shiny.js`, `Shiny.drive` and
`Shiny.webmcp` will still reach py-shiny's bundle on its next upgrade. They
stay off there (py-shiny's `config` message has no `webmcp` flag), but any
`Shiny.drive` method that waits on the server needs R's `@waitForFlush` RPC,
which py-shiny doesn't have.

## Two layers

| Namespace | File | What |
|---|---|---|
| `window.Shiny.drive` | `srcts/src/shiny/drive.ts` (`ShinyDrive`) | Public JS methods. Always present; knows nothing about WebMCP |
| `window.Shiny.webmcp` | `srcts/src/shiny/webmcp.ts` (`ShinyWebMCP`) | Tool registry, mirrored into `document.modelContext`; four built-in tools that wrap `Shiny.drive` |

Both are created in `ShinyClass`'s constructor (`srcts/src/shiny/index.ts`).

## `Shiny.drive`

- **Reading:** `listInputs({ids?, module?})`, `getInput(id)`,
  `listOutputs({ids?, module?})`, `getOutput(id, {imageOffset?, imageLimit?})`.
  `module: "filters"` matches ids starting with `"filters-"` (the `NS()`
  separator).
- **Driving:** `setInput(id, value)`, `setInputs({id: value, ...})`,
  `click(id)`, `uploadFiles(id, files)`, `waitForUpload(id)`.
- **Waiting:** `ready()`, `flush()`, `waitForIdle({duration, timeout})`.
- **Config:** `skipInputBinding(name)`, `skipOutputBinding(name)`,
  `setDefaultTimeout(ms | null)`.
- **Version:** `version` (a number, `apiVersion` in `drive.ts`). Bump it
  when the API changes, so callers can check for features; development
  installs share a package version with and without a change.

Rules every method follows:

- **Only visible UI.** Inputs and outputs in hidden tabs, closed
  `conditionalPanel()`s or not-yet-rendered dynamic UI are not listed or
  settable. The `includeHidden: true` option lifts this. The WebMCP
  `list_inputs`/`list_outputs` tools take it too, so an agent learns that
  hidden items exist; `set_inputs` and `get_output` never pass it.
- **Values are checked first.** Each input row has a `schema` (JSON Schema)
  built per binding in `valueSchema()`. `setInputs()` checks every value
  before touching any widget, so one bad value changes nothing. Errors list
  the allowed values so an agent can retry.
- **Widgets are updated like `update*Input()`.** `setInputs()` calls the
  binding's `receiveMessage()` with what the R update function would send
  (`inputMessage()`: `{value}` for most; bslib's accordion and sidebar take
  `{method}`, as `accordion_panel_set()` / `toggle_sidebar()` send). A closed
  bslib sidebar hides its own `<aside>`, so `isShownInput()` judges it by its
  layout. `settleInput()` waits for accordion panels to finish animating,
  since their value changes only then. It suppresses the bindings' own
  `shiny:inputchanged` events while doing so, then sends every value in one
  websocket message (the last with `priority: "event"`), so the server
  reacts once.
- **Results are current.** After sending, it awaits `flush()` and then waits
  until no visible output has the `recalculating` class.
- **Errors are typed.** Everything throws `DriveError` (`name:
  "ShinyDriveError"`) with a `code`: `not-found`, `read-only`, `disabled`,
  `invalid-value`, `invalid-argument`, `wrong-type`, `update-failed` (with
  `applied`, the ids set before the failure), `upload-failed`,
  `disconnected`, `server-error`, `timeout`. Callers match on `code`, never
  on `message`. Messages don't name WebMCP tools: JS callers have none.

## Waiting and timeouts

- **No timeout by default.** `setDefaultTimeout(ms)` sets one for every
  wait; methods with a `timeout` option use it when the option is missing.
  Slow apps (a database warehouse waking up) can take minutes, so a fixed
  limit would report a still-running change as a failure.
- **On timeout,** waits on the server (`flush()`, `waitForIdle()`,
  `ready()`, uploads) reject with code `timeout`. Waits on outputs give up
  quietly; the outputs then show `status: "recalculating"`.
- **`flush()` rejects with `disconnected`** if the socket closes while it
  waits. Without that, a closed socket would leave it waiting forever.
- **No polling.** `waitForClasses()` re-checks on a `MutationObserver` of
  `class` changes (`shiny-busy`, `recalculating`). Browsers throttle timers
  in background tabs (to about once a minute in Chrome), but not observers.
  Two single timers remain: the 150ms wait for the client to report new
  output visibility, and `waitForIdle()`'s `duration`.
- **`ready()`** waits for Shiny's `initializedPromise` (first
  `shiny:sessioninitialized`), then `flush()` and visible outputs. It is
  passed to the `ShinyDrive` constructor.

## Hidden inputs and outputs

"Hidden" means the element has no size and it, or a parent, has
`display: none` (`isVisible()` in `srcts/src/utils/index.ts`). Inputs are
judged by their `.shiny-input-container` (selectize and sliders hide the
bound element). Common causes: an inactive tab, a closed
`conditionalPanel()` or accordion panel, a collapsed sidebar.

- Hidden items are not listed, read or set unless the caller passes
  `includeHidden: true`. Listed hidden items have `hidden: true`.
- **Hidden outputs may be stale.** Shiny suspends hidden outputs
  (`outputOptions(suspendWhenHidden = TRUE)` is the default), so their
  content is whatever they showed last, or nothing if they were never shown.
  To get a current value, show the output (switch tab, open the panel) first.
- **Only elements, not the page.** A page in a background tab, or an
  iframe the host hid, still counts as visible: browsers keep the last
  layout. Hosts that care check `document.visibilityState` themselves.

## Output images

`listOutputs()` gives each output's `imageCount`. `getOutput()` also returns
one page of `images` (`imageOffset`, default 0; `imageLimit`, default 10;
the `get_output` tool caps it at 10). More remain while
`imageOffset + images.length < imageCount`.

- **Which images:** the output's own visible `<img src>` and `<canvas>`
  elements, in page order. Images inside a nested output (a `plotOutput()` in
  a `uiOutput()`) belong to that output, so they aren't counted twice.
- **Entries:** `{index, type: "data", value}` for base64 `data:image/` URLs
  (`renderPlot()`, `renderImage()`) and readable canvases (`toDataURL()`, PNG);
  `{index, type: "url", value}` for any other `<img>` (not fetched);
  `{index, type: "error", reason}` when a canvas has zero size, is one solid
  color, or is tainted by cross-origin images (`toDataURL()` throws).
- **WebMCP:** `toToolResult()` turns each `data` entry into MCP image
  content, in order, and drops its `value` from the JSON. `url` and `error`
  entries stay in the JSON.
- **Indexes can shift** if the output re-renders between pages; compare
  `imageCount` across calls.
- **Tests:** `tests/testthat/test-zzz-st2-webmcp.R` (shinytest2, macOS CI
  only) covers each entry type, nesting, paging and the tool's image
  content, plus `callTool()` argument checks and `data-shiny-webmcp`. It
  gets a cross-origin image by loading the app's own file from `localhost`
  while the page is on `127.0.0.1`.

## Access: `data-shiny-drive-access`

Set on the bound element or its `.shiny-input-container` (where the R helper
`webMcpAccess(tag, access)` in `R/webmcp.R` puts it). It applies only to that
element, not to inputs nested inside it.

| Value | Listed | Value returned | Settable |
|---|---|---|---|
| *(none)* | yes | yes | yes |
| `read-only` | yes | yes | no |
| `write-only` | yes | no | yes |
| `exclude` | no | no | no |

Defaults without the attribute: `passwordInput()` is `write-only`,
`fileInput()` is `read-only` (agents can't send file contents; JS callers can
still use `uploadFiles()` unless the attribute is set explicitly). Outputs are
always read-only, so only `exclude` applies to them. Unknown values log a
console warning.

## `Shiny.webmcp`

- `enable()` registers four tools whose definitions never change, so the
  context cost doesn't grow with app size: `list_inputs {ids?, module?, includeHidden?}`,
  `set_inputs {inputs}`, `list_outputs {ids?, module?, includeHidden?}`,
  `get_output {id, imageOffset?, imageLimit?}`.
  `disable()` removes only those four.
- `set_inputs` takes one `{id, value?}` or a list of them (`anyOf`), so
  there is no separate single-input tool. `toInputValues()` checks the
  entries (object, string `id`, no other keys, no repeated id) and passes
  them to `drive.setInputs()`: every value is checked before any widget
  changes, and all of them, button clicks included, reach the server in
  one message. The schema's `examples` feeds `parseToolArgs()`'s example.
- `registerTool()`, `unregisterTool()`, `listTools()`, `callTool()` and a
  `toolchange` event manage the registry. Each tool is mirrored with
  `document.modelContext.registerTool(tool, {signal})` when that exists.
- `callTool()` runs `parseToolArgs()` (`driveSchema.ts`) before `execute`:
  it accepts an object or a JSON string of one (hosts differ), rejects
  unknown keys when the schema has `additionalProperties: false` (all four
  built-in tools do) and missing `required` keys. The error includes the
  schema and an example call, so the agent retries instead of giving up on
  the tool. Value checks stay in `Shiny.drive`.
- `<html data-shiny-webmcp>` shows the mirror's state: `pending`,
  `registered`, `partial` (some tools failed), `failed`, or `unavailable`
  (no `document.modelContext`). No attribute means no tools are mirrored.
- **Origin trials:** native Chrome/Edge WebMCP needs a flag or an
  origin-trial token. Tokens are per origin (each deployed app needs its
  own), and third-party tokens must be injected by a script served from the
  registered origin, so shiny can't ship one. The polyfill works without
  one.
- `webmcp_polyfill_enabled()` reads `options(shiny.webmcp.polyfill)`, then
  `SHINY_WEBMCP_POLYFILL`, else follows `webmcp_enabled()`; it is never
  `TRUE` without WebMCP. Hosts that relay `Shiny.webmcp` over their own
  transport (and offline apps) set it to `FALSE`.
- `webmcp_enabled()` (`R/webmcp.R`) reads `options(shiny.webmcp)`, falling
  back to the `SHINY_WEBMCP` env var (via `as.logical()`, so `"true"` works
  and `"1"` doesn't). Use it instead of reading the option directly.
- When enabled, the server adds `webmcp: true` to the `config`
  message; the client calls `enable()` once, on the first
  `shiny:sessioninitialized`, so a later `disable()` survives reconnects.
- Unless the polyfill is off, the same option makes `renderPage()` (`R/shinyui.R`) add
  `webMcpPolyfillDependency()` (`R/webmcp.R`): `@mcp-b/global` from unpkg,
  pinned to one version. It is a CDN `htmlDependency`, not vendored, so
  offline apps get no polyfill. It defers to a native
  `document.modelContext`.

## Tools defined in R (`R/webmcp-tools.R`)

`webMcpTool(name, fun, description, ..., arguments, value, annotations)` is
shaped like `ellmer::tool()` but is shiny's own S3 object, so it can be
documented and extended here. `name` comes first (like
`registerInputHandler(type, fun)`); `...` must be empty, so the optional
arguments are always named. `fun` takes only the agent's arguments
(`do.call(fun, args)`), inside `isolate()` and `withReactiveDomain(scope)`, so
`getDefaultReactiveDomain()` gives the calling (module) session and
`session$input` its inputs. Outputs can't be read server-side; agents use
`get_output`. `fun`'s formals without defaults must all have a type.

- **Types:** `arguments` is a named list of ellmer types and `value` an
  `ellmer::type_object()` (`value` mirrors R's "Value" doc section; it becomes
  WebMCP's `outputSchema`).
  `webMcpTypeSchema()` turns them into JSON Schema itself, reading props with
  `S7::prop()` (`@` needs R >= 4.3). It doesn't use `ellmer:::as_json()`.
  ellmer and S7 are in Suggests, checked only when types are used.
- **Checks:** `webMcpConform()` checks the agent's arguments before `fun`
  runs (arrays of scalars become atomic vectors) and the returned value
  after (arrays stay lists, so a length-1 array still serializes as an
  array). The browser's `parseToolArgs()` has already caught unknown and
  missing keys; R checks again because any client can send `@webmcpCall`.
- **Registration:** `registerWebMcpTool(tool, session)`. With no session
  (outside the server function), the tool goes in `.globals$webMcpTools`
  and `sendGlobalWebMcpTools()` sends it when each session starts, right
  after the `config` message. With a session, it goes in
  `webMcpSessionTools[[token]]` (removed `onSessionEnded`), the name gets
  `session$ns()` (so modules get `"mod-tool"`), and it is sent right away. Both
  use the `shiny.webmcp.tools` custom message (`{add, remove}`), handled by
  `Shiny.webmcp.setServerTools()`. Session tools are a no-op when WebMCP is
  off. Built-in tool names are refused.
- **Calls:** the client tool's `execute` calls `@webmcpCall(name, args)`
  (`webMcpCall()`), which looks in the session's tools first, then the global
  ones. Errors reach the agent as the RPC error; with
  `options(shiny.sanitize.errors = TRUE)` only `safeError()`s keep their
  message, and the original is logged with `message()`.
- **Tests:** `test-webmcp-tools.R` (unit; note `MockShinySession$ns()`
  prefixes `mock-session-`) and `test-zzz-st2-webmcp-tools.R` (browser).

## Server side (`R/shiny.R`)

- `ShinySession$dispatch()` accepts a promise from an `@<method>` RPC and
  sends the response when it settles.
- `@waitForFlush` resolves on the session's next `onFlushed`. R answers RPCs
  before the reactive flush, so without it a client can't tell when outputs
  from a just-sent input have been updated. `Shiny.drive.flush()` calls it.
- **TODO:** the RPC reply is a stand-in for "the server flushed" and carries
  no flush info. Explore an explicit flush signal from server to client.
  Don't copy test mode's empty `values` message; it has the same problem.

## Related client changes

- `FileUploader` (`srcts/src/file/fileProcessor.ts`) triggers
  `shiny:uploadend` (`name`, `error`) when an upload finishes or fails;
  `waitForUpload()` listens for it.
- `InputBatchSender` no longer sends an empty `update` when an `"event"` send
  emptied the batch first.

## Pitfalls

- **The flush barrier misses later flushes.** Outputs created by
  `insertUI()`/`renderUI()`, or hidden outputs that recompute once shown, update
  in a later flush; that's why `setInputs()` also waits on `recalculating`.
- **`skipOutputBinding("shiny.htmlOutput")` is coarse:** it hides `uiOutput()`
  and `tableOutput()` alike. Use the attribute for one element.
- **Selectize** keeps only the selected `<option>`s in the `<select>`; choices
  come from `el.selectize.options`, which for server-side selectize holds only
  the loaded choices.
- **Waits have no timeout by default** (see "Waiting and timeouts"). Tests
  that may hang should set one with `setDefaultTimeout()`.
- **WebGL canvases usually read as blank.** Unless the widget created its
  context with `preserveDrawingBuffer: true`, the browser clears the buffer
  after each frame, so `toDataURL()` returns an empty image with no error.
  `isSolidColor()` draws a copy and catches this (transparent, or opaque
  black with `alpha: false`), returning an `error` entry rather than a blank
  `data` one. Reading the widget's canvas through `getContext()` is avoided
  on purpose: it could create a context the widget doesn't expect.
- **`checkValue()` / `valueSchema()`** live in `driveSchema.ts`, apart from
  `drive.ts` (which listens on `document` at load), so they can be unit tested
  in Node.
