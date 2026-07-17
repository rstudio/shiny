# In-place update of a running Shiny MCP App (`update_<appId>_app`)

**Date:** 2026-07-17
**Issue:** [#4415](https://github.com/rstudio/shiny/issues/4415)
**Follow-up to:** #4414 (`mcpConfigure()` / init-args-via-RestoreContext)
**Target branch:** `origin/mcp`

## Problem

When a Shiny MCP App declares `arguments` in `mcpConfigure()`, the model's
only lever on the running app is the `open_<appId>_app` tool. In a live host
(claude.ai), asking the model to "change the label" of an already-open app
causes it to **re-invoke `open_*`, rendering a fresh instance** rather than
updating the running one. The model itself explains it has no way to send new
arguments to a running instance — "the only way to modify the app is by
calling the tool again with new parameters."

The runtime-update channel built in #4414 (`mcpUpdates()`, fed by a host
`ui/notifications/tool-input` to the live iframe) is therefore **effectively
never triggered by the model**. In practice the feature behaves as
init-args-only.

**Fix:** give the model an explicit *update* verb — auto-register a companion
`update_<appId>_app` tool — whose handler pushes new arguments into the
running session's `mcpUpdates()` channel, with no re-render.

## Goals

- Model can update an already-open instance in place, targeting a specific
  running session.
- Zero author code beyond the existing `mcpConfigure(arguments = ...)` +
  single `observe(mcpUpdates())`.
- Reuse `mcpUpdates()` end-to-end so existing app code works unchanged.

## Non-goals (handled separately)

- Widget auto-restore doc corrections from #4414.
- The `resourceUri`-args init-path spike from #4414.

These are init-path concerns, tracked in another effort; not folded in here.

## Decisions

The five load-bearing decisions, as agreed during brainstorming:

1. **Targeting = explicit session handle (required).** No broadcast /
   most-recent guessing. `update_*` takes a **required** `session` argument;
   the model targets exactly one running session per call. To update several,
   the model loops over handles.
2. **Server push via a per-session `reactiveVal` (server-side merge).** Each
   registered MCP session holds a server-side `reactiveVal`. The `update_*`
   handler sets it directly. `mcpUpdates()` is rewritten to **overlay** the
   server-pushed args on top of the client-delivered init args (per-key,
   latest wins), then apply the allow-list filter. No client round-trip.
3. **Flat global session registry keyed by `session$token`.** Populated on
   session start when `isMcpSession()` is `TRUE`; removed on
   `session$onSessionEnded()`. One R process serves one `appId`, so a flat map
   suffices. Unknown/stale token → `update_*` returns a **tool error** (not a
   silent success).
4. **Auto-registered `update_<appId>_app` tool**, published **only when
   `arguments` are declared**. The handler sets the target session's
   `reactiveVal` **inside that session's reactive domain**
   (`withReactiveDomain(session, ...)`) and flushes it, so the app's
   `observe(mcpUpdates())` invalidates in the correct session context.
5. **Token delivered to the model via a reserved-key model-context
   envelope + a connect-time auto-emit.** The framework stamps a reserved
   `session` key into the model-visible structured content; the author's
   payload nests under `data`. On connect, the framework auto-emits
   `{ session: "<token>", state: "connected" }` plus a short text instruction
   so the model learns the token with zero author code.

## Architecture

### Data flow

```
open_<appId>_app                                     (model -> host)
  -> host renders iframe -> iframe connects
  -> Shiny session created; isMcpSession() == TRUE
  -> registry: token -> session                      (NEW)
  -> framework auto-emits {session, state:"connected"} to model context (NEW)

model reads token from its context
  -> update_<appId>_app(session="<token>", <args>)   (model -> /mcp)
  -> handler looks up token in registry              (NEW)
     -> unknown/stale: tool error
     -> found: withReactiveDomain(session, {
          serverUpdates(filtered args)                (NEW reactiveVal set)
          flush session
        })
  -> app's observe({ mcpUpdates() }) fires            (unchanged app code)
```

### Components

#### 1. Session registry — `R/mcp-session.R` (or a small new file)

A flat global map, mirroring `mcpConnections` (`R/mcp-tunnel.R`):

```r
mcpSessions <- NULL
on_load({ mcpSessions <- Map$new() })

mcpSessionRegister <- function(session) {
  # Called on session start when isMcpSession(session).
  # Stores token -> list(session = session, updates = reactiveVal(NULL)).
  # Registers session$onSessionEnded() to remove the entry.
}

mcpSessionGet <- function(token) { ... }  # NULL if absent
```

- Keyed by `session$token`.
- Value carries both the `ShinySession` and its server-push `reactiveVal`
  (the `reactiveVal` is created within the session's domain at registration).
- Cleanup on `session$onSessionEnded()`; reconnects register under a fresh
  token.

**Registration hook:** the point in session startup where `isMcpSession()` is
knowable and the session is live (both tunnel and direct-connect sessions
satisfy `isMcpSession()`). Investigate `ShinySession` construction /
`server`-function invocation for the right hook; the registration must run
with the session's reactive domain available so the `reactiveVal` is created
in-session.

#### 2. `mcpUpdates()` overlay — `R/mcp-session.R`

Rewrite to merge two sources, latest-wins per key, then filter:

```r
mcpUpdates <- function(session = getDefaultReactiveDomain()) {
  if (is.null(session)) stop("mcpUpdates() must be called from within a Shiny session")
  clientArgs <- mcpParseClientData(session$clientData$mcp_tool_input)
  serverArgs <- <this session's server-push reactiveVal>()   # reactive read
  merged <- modifyList(clientArgs %||% list(), serverArgs %||% list())
  mcpFilterArguments(merged)
}
```

- Both reads are reactive, so the observer fires on init, re-open (client
  channel), **and** server push (reactiveVal).
- Overlay semantics: a server push updates only the keys it carries; other
  init args persist. (Confirm this is the desired behavior vs. wholesale
  replace — leaning overlay so partial updates like "just change the label"
  don't drop `n`.)
- The server-push `reactiveVal` is looked up from the registry entry for this
  session. If the session isn't registered (non-MCP), `serverArgs` is `NULL`
  and behavior is unchanged.

#### 3. Auto-registered `update_<appId>_app` tool — `R/mcp-server.R`

- **`tools/list` (`mcpToolsList()`):** when `.globals$mcp$arguments` is
  non-empty, append a tool entry:
  - `name`: `mcpUpdateToolName()` -> `update_<appId>_app`
    (or `update_shiny_app` when no `appId`).
  - `description`: *"Change parameters of the already-open app in place. Do
    NOT re-open the app; use this to update the running instance."*
  - `inputSchema`: `mcpArgumentsSchema(.globals$mcp$arguments)` **plus** a
    required `session` string property.
- **Reserved names (`mcpReservedToolNames()`):** add the update tool name so
  `registerMcpTool()` cannot collide with it.
- **Dispatch (`mcpToolCall()`):** add a branch for the update tool name:

```r
if (identical(name, mcpUpdateToolName())) {
  args    <- body$params$arguments %||% list()
  token   <- args$session
  entry   <- mcpSessionGet(token)
  if (is.null(entry)) {
    return(mcpToolErrorResult(body$id, sprintf(
      "No running app session with id '%s'. Open the app first, or use the id it reported.",
      token %||% ""
    )))
  }
  pushed <- mcpFilterArguments(args[setdiff(names(args), "session")])
  withReactiveDomain(entry$session, {
    entry$updates(pushed)
    flushReact()  # or session-appropriate flush
  })
  return(mcpResult(body$id, list(content = list(list(
    type = "text",
    text = "Updated the running app in place."
  )))))
}
```

- Filter drops `session` and any non-declared keys before pushing.
- The push runs in the target session's domain and flushes so the app's
  observer fires promptly.

#### 4. Token delivery — `R/mcp-session.R` + `srcts/src/mcp/index.ts`

- **Envelope shape** sent from `mcpUpdateModelContext()`:
  the framework always stamps a reserved `session` key into the model-visible
  structured content; author `data` nests under `data`:

  ```
  { session: "<token>", data: { ...authorData } }   # + content[] from `text`
  ```

  Author calls can never clobber `session` (it's framework-owned).
- **Connect-time auto-emit:** on connect, for MCP sessions with declared
  `arguments`, the framework emits once:

  ```
  { session: "<token>", state: "connected" }        # structured
  + text: "This app instance's id is <token>. To change THIS instance in
           place, call update_<appId>_app with session = \"<token>\".
           Do not re-open the app."
  ```

  The text instruction is required so the model knows to echo the id;
  a bare structured id may not prompt the model to reuse it.
- The token must land in **model-visible `structuredContent`** (not a
  nonstandard top-level field a host might strip). See verification item
  below.

### Client (`srcts/src/mcp/index.ts`)

- The connect-time auto-emit is triggered from R via
  `session$sendCustomMessage(...)` and forwarded to `app.updateModelContext`
  (reuse the existing `shiny.mcp.updateModelContext` handler, or a small
  dedicated one). No new transport machinery.
- `mcpUpdateModelContext()`'s envelope reshaping is done R-side before the
  custom message is sent; the existing JS handler forwards it as-is.

## Testing

Unit tests (testthat 3):

- **Registry lifecycle:** registering on MCP session start; removal on
  `onSessionEnded()`; non-MCP sessions are not registered; reconnect gets a
  fresh token.
- **`mcpUpdates()` overlay:** client-only args; server-push-only args;
  overlay precedence (server push updates named keys, leaves others); result
  is allow-list filtered.
- **`update_*` dispatch:** tool appears in `tools/list` only when `arguments`
  declared; `inputSchema` includes required `session`; unknown/stale token
  returns a tool error; valid token filters `session` + non-declared keys and
  sets the session's `reactiveVal`; the app observer fires.
- **Reserved names:** `registerMcpTool()` rejects the update tool name.
- **Token envelope:** `mcpUpdateModelContext()` stamps `session`, nests author
  `data`; author cannot overwrite `session`.

Where a live `ShinySession` is awkward to construct in a unit test, use the
existing MCP test harness/fakes (see current `tests/testthat/test-mcp*.R`).

## Verification items / known limitations (document in `mcp/limitations.md`)

- **Host-passthrough spike (host-dependent):** confirm a real host
  (claude.ai) forwards our structured envelope (`session`/`state`) into the
  model's readable context. If a host forwards only `content`/
  `structuredContent`, the token already rides `structuredContent` — but
  verify end-to-end that the model can read and echo it into
  `update_*(session=...)`.
- **Multi-instance caveat:** with several open instances, the model sees
  several `session` tokens in one conversation and must associate each with
  the right instance. There is no server-side guarantee it picks correctly;
  the model targets/loops per handle. Documented, not solved.

## Demo updates (update existing `mcp/` apps)

- **`mcp/demo-app3/app.R` (clock):** the issue's motivating example. Its
  `label` / `paused` args should now be updatable in place via
  `update_clock_app(session=..., label=...)`. Verify the existing
  `observe(mcpUpdates())` handles the server push with no code change; add a
  brief comment noting update-in-place is now available.
- **`mcp/demo-app/app.R` and `mcp/demo-app2/app.R`:** confirm their existing
  `mcpUpdates()` observers work unchanged under server push; adjust comments
  to mention the auto `update_*` tool.

Per repo convention, mirror any MCP doc changes into the repo-root `mcp/`
copies (design docs, limitations, demos).

## Open question to confirm during implementation

- `mcpUpdates()` overlay vs. wholesale replace on server push. Spec assumes
  **overlay** (partial updates preserve untouched init args). Revisit if a
  concrete app wants replace semantics.
