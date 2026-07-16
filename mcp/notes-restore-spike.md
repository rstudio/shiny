# Spike: init-args-via-RestoreContext — Findings

## Unknown 1: Restore-observer gating

**Conclusion:** Confirmed. `onRestore`/`onRestored` observers are registered ONLY inside
`createBookmarkObservers()`, which early-returns at line 1936 when
`bookmarkStore == "disable"`. Furthermore, in `R/server.R` lines 202-211,
when `bookmarkStore == "disable"` an **empty** `RestoreContext$new()` is created
(active=FALSE, empty input set), and `createBookmarkObservers()` is never called.

This means: to restore MCP init args without enabling bookmarking, Phase B must
(a) build an *active* `RestoreContext` from the args, and (b) register the
restore-callback observers independently of `bookmarkStore`.

### Exact line numbers of the three restore-related observers (R/shiny.R)

1. **Init-error notification observer** — lines 1959-1967
2. **onRestore invoke observer** (priority 1000000) — lines 1971-1991
3. **onRestored invoke** (via `self$onFlushed`) — lines 1995-2014

These three live inside the `withReactiveDomain(self, { ... })` block
(lines 1951-2016) and are only reached when `createBookmarkObservers()` does
not early-return.

### restoreInput() gating (R/bookmark-state.R line 511)

`restoreInput(id)` returns the restored value **only** when:
- `hasCurrentRestoreContext()` is TRUE (line 517), AND
- the RestoreContext's `$input` RestoreInputSet has `id` available (not yet
  consumed) — line 522.

If no active context exists, or the input is absent/consumed, `default` is returned.

---

## Unknown 2: Restore query-string encoding

**Conclusion:** Confirmed. The grammar is:

```
_inputs_&<name>=<url-encoded JSON>&<name2>=<url-encoded JSON>
```

Values are URL-encoded JSON literals (numbers bare, strings with encoded quotes).
The `_inputs_` token may optionally have a trailing `=` (line 326: `(^|&)_inputs_=?(&|$)`).

### Round-trip verification

Command:
```
R -q -e 'devtools::load_all(quiet=TRUE); rc <- shiny:::RestoreContext$new("_inputs_&n=200&note=%22hi%22"); cat("active:", rc$active, "\n"); cat("n:", rc$input$get("n"), "\n"); cat("note:", rc$input$get("note"), "\n")'
```

Output:
```
active: TRUE
n: 200
note: hi
```

Confirmed: `active=TRUE`, `n` returns numeric 200, `note` returns character "hi"
(the quotes are part of the JSON encoding, parsed by `safeFromJSON`).

### JS bridge encoding recipe

To produce a restore string from tool-call arguments `{n: 200, note: "hi"}`:
```js
"_inputs_&" + Object.entries(args)
  .map(([k, v]) => encodeURIComponent(k) + "=" + encodeURIComponent(JSON.stringify(v)))
  .join("&")
```

---

## Unknown 3: ext-apps initial-tool-input API

**Verdict: DEFINITIVE — there is NO synchronous API for the opening arguments. `hostContext.toolInfo` carries only the tool *definition* (schema), not the call arguments. The arguments arrive only via the async `ontoolinput` notification ("one-shot event") shortly after the handshake. Therefore B3 must register `ontoolinput` before `connect()` and gate Shiny's socket start on the first `ontoolinput` (or a short timeout fallback) — the promise-gated approach (strategy 2 below).**

### Evidence from the types and API

1. `McpUiInitializeResult` (spec.types.d.ts:434) includes `hostContext: McpUiHostContext`.

2. `McpUiHostContext` (spec.types.d.ts:221) contains:
   ```ts
   toolInfo?: {
     id?: RequestId;       // JSON-RPC id of the tools/call request
     tool: Tool;           // Tool definition including name, inputSchema
   };
   ```
   **However, `toolInfo` contains the tool *definition* (schema), NOT the tool call *arguments*.**

3. The tool-call arguments arrive via `McpUiToolInputNotification` (spec.types.d.ts:165):
   ```ts
   method: "ui/notifications/tool-input";
   params: { arguments?: Record<string, unknown>; };
   ```
   This is a **notification** (async, host->view), classified as a "one-shot event"
   (App class line 464: `private static readonly ONE_SHOT_EVENTS`).

4. The `App` class docs (app.d.ts:460-471) explicitly say:
   > Events the host typically sends once, shortly after the handshake.
   > Registering a handler for one of these *after* `connect()` resolves risks
   > missing the notification entirely.

   And the example (app.d.ts:225-237):
   ```ts
   // Register handlers before connecting to ensure no notifications are missed
   app.ontoolinput = (params) => { ... };
   await app.connect();
   ```

5. There is **no** synchronous `getToolInput()` / `initialArguments` / `renderData`
   method on the `App` class. The arguments are only delivered via the
   `ontoolinput` notification.

### Timing analysis for the Shiny bridge

In `srcts/src/mcp/index.ts`:
- `app.connect()` is kicked off at line 167 (the `connected` promise).
- `app.ontoolinput` is set at line 270, **after** `connect()` is initiated.
  Per the ext-apps docs, this risks missing the notification — but in practice
  the handler is registered synchronously in the same microtask as `connect()`,
  so it will be in place before the notification arrives (postMessage is async).
- `shiny.createSocket` (line 219) defers `s.start()` via `setTimeout(..., 0)`
  (line 245). This means Shiny's websocket `init` message is sent on the
  **next macrotask** — after the current microtask queue drains.

### Critical insight: the `setTimeout(() => s.start(), 0)` hook

The `setTimeout` at line 245 defers socket start to the next macrotask. This
provides **exactly the hook needed**: the bridge can listen for `ontoolinput`,
and when it fires (which happens after `connect()` resolves, during the
microtask/macrotask window before `s.start()` runs), encode the arguments into
the `clientdata_url_search` value that Shiny sends in its `init` message.

**However**, there is a race: `ontoolinput` fires "shortly after the handshake"
but the spec does not guarantee it fires before the next macrotask. If the host
delays sending `tool-input`, the socket may start before args arrive.

### Recommended approach for B3

Two viable strategies:

1. **Optimistic delay**: Replace `setTimeout(() => s.start(), 0)` with a brief
   `await connected` + short timeout (e.g. 50ms) to allow `ontoolinput` to arrive.
   If it arrives in time, encode into `clientdata_url_search`. If not, fall back
   to `mcpUpdates()` reactive path.

2. **Promise-gated start**: Create a promise that resolves on first `ontoolinput`
   OR a timeout, and only call `s.start()` after it resolves. This ensures the
   restore path is used whenever the host cooperates.

Either way, the existing `setTimeout(..., 0)` deferral is the correct extension
point — it already separates socket creation from socket start.
