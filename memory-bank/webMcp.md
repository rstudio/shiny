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
  `listOutputs({ids?, module?})`, `getOutput(id)`. `module: "filters"`
  matches ids starting with `"filters-"` (the `NS()` separator).
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
  `set_input {id, value?}`, `list_outputs {ids?, module?}`, `get_output {id}`.
  `disable()` removes only those four.
- `registerTool()`, `unregisterTool()`, `listTools()`, `callTool()` and a
  `toolchange` event manage the registry. Each tool is mirrored with
  `document.modelContext.registerTool(tool, {signal})` when that exists.
- `options(shiny.webmcp = TRUE)` adds `webmcp: true` to the `config`
  message; the client calls `enable()` once, on the first
  `shiny:sessioninitialized`, so a later `disable()` survives reconnects.
- The same option makes `renderPage()` (`R/shinyui.R`) add
  `webMcpPolyfillDependency()` (`R/webmcp.R`): `@mcp-b/global` from unpkg,
  pinned to one version. It is a CDN `htmlDependency`, not vendored, so
  offline apps get no polyfill. It defers to a native
  `document.modelContext`.

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
- **`checkValue()` / `valueSchema()`** live in `driveSchema.ts`, apart from
  `drive.ts` (which listens on `document` at load), so they can be unit tested
  in Node.
