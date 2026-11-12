---
title: "WebMCP and the Shiny.drive JS API"
description: "How window.Shiny.drive (public JS methods to read and drive inputs/outputs) and window.Shiny.webmcp (WebMCP tool registry and the four built-in tools) work, the data-shiny-drive-access attribute and webMcpAccess(), the server-side flush barrier, and the R-only scope. Read before changing srcts/src/shiny/drive.ts, srcts/src/shiny/webmcp.ts, or ShinySession$dispatch()."
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
- **Waiting:** `flush()`, `waitForIdle({duration, timeout})`.
- **Config:** `skipInputBinding(name)`, `skipOutputBinding(name)`.

Rules every method follows:

- **Only visible UI.** Inputs and outputs in hidden tabs, closed
  `conditionalPanel()`s or not-yet-rendered dynamic UI are not listed or
  settable. The JS-only `includeHidden: true` option lifts this; the WebMCP
  tools never pass it.
- **Values are checked first.** Each input row has a `schema` (JSON Schema)
  built per binding in `valueSchema()`. `setInputs()` checks every value
  before touching any widget, so one bad value changes nothing. Errors list
  the allowed values so an agent can retry.
- **Widgets are updated like `update*Input()`.** `setInputs()` calls the
  binding's `receiveMessage()`, suppresses the bindings' own
  `shiny:inputchanged` events while doing so, then sends every value in one
  websocket message (the last with `priority: "event"`), so the server
  reacts once.
- **Results are current.** After sending, it awaits `flush()` and then polls
  until no visible output has the `recalculating` class.

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
  context cost doesn't grow with app size: `list_inputs {ids?, module?}`,
  `set_input {id, value?}`, `list_outputs {ids?, module?}`,
  `get_output {id, imageOffset?, imageLimit?}`.
  `disable()` removes only those four.
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
- `webmcp_enabled()` (`R/webmcp.R`) reads `options(shiny.webmcp)`, falling
  back to the `SHINY_WEBMCP` env var (via `as.logical()`, so `"true"` works
  and `"1"` doesn't). Use it instead of reading the option directly.
- When enabled, the server adds `webmcp: true` to the `config`
  message; the client calls `enable()` once, on the first
  `shiny:sessioninitialized`, so a later `disable()` survives reconnects.
- The same option makes `renderPage()` (`R/shinyui.R`) add
  `webMcpPolyfillDependency()` (`R/webmcp.R`): `@mcp-b/global` from unpkg,
  pinned to one version. It is a CDN `htmlDependency`, not vendored, so
  offline apps get no polyfill. It defers to a native
  `document.modelContext`.

## Tools defined in R (`R/webmcp-tools.R`)

`webMcpTool(name, fun, description, ..., arguments, value, annotations)` is
shaped like `ellmer::tool()` but is shiny's own S3 object, so it can be
documented and extended here. `name` comes first (like
`registerInputHandler(type, fun)`); `...` must be empty, so the optional
arguments are always named. `fun` gets the agent's arguments plus
`input` and `session` when its formals name them (any position),
like `argsForServerFunc()` does for server functions (`R/server.R`). They
come from the calling (module) session; the lookup runs per call in
`webMcpCall()` (~1µs, negligible next to the websocket round trip). The names
are reserved: `arguments` can't describe them, so agents can't set them
(plumber2 does the same for path args vs. its `query`/`body` names). Runs inside
`isolate()` and `withReactiveDomain(scope)`. No `output` (Barret's call): a
tool shouldn't render outputs; it sets a `reactiveVal()` that an output reads,
and `serviceApp()`'s `flushReact()` after the RPC message updates the page.
Outputs can't be read server-side either; agents use `get_output`. `fun`'s formals without defaults must all have a type, and `fun`
can't take `...` (agents can only send `arguments` keys, so it would only hide
typed args). Free-form data: a declared array of name-value pairs
(`type_array(type_object(name = type_enum(...), value = ...))`), fully checked.
Don't recommend open objects: ellmer 0.5.0 deprecated
`type_object(.additional_properties)` because no provider's structured output
can fill them (strict modes need closed objects). `webMcpTool()` refuses any
open object in `arguments` (`webMcpOpenObjects()` walks the whole schema,
including `type_from_schema()` JSON; an object without `additionalProperties`
counts as open, per JSON Schema). Note `type_from_schema()` values are still
not checked in R beyond that. `value` may contain open objects.

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
- **Tests:** `test-webmcp-tools-types.R` (the shape of `arguments`/`value`: defining, JSON Schema, checking JSON input and serialized output; one place per behavior), `test-webmcp-tools.R` (unit; note `MockShinySession$ns()`
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
- **Waits time out** after 10s (`flushTimeoutMs`). `flush()` rejects on
  timeout; `waitForOutputs()` gives up quietly, so results can still show
  `status: "recalculating"`.
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
