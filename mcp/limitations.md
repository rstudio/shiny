# MCP Apps: known limitations

Constraints discovered while building Shiny's MCP App support, verified against
a real host (Claude Desktop, stdio transport). These shape the design in
`design-mcpConfigure.md`; record them here so we don't re-litigate settled
dead ends.

## 1. Init arguments cannot auto-restore input widgets (no flash-free restore)

**What we wanted:** the model opens the app with `open_app(n = 200)` and the
`sliderInput("n")` is *born* at 200 — no author code, no visible jump — by
reusing Shiny's bookmark `RestoreContext` / `restoreInput()`.

**Why it can't work:** normal bookmarking restores a widget because the page is
rendered **server-side with the restore context active** (from the URL query
string), so `restoreInput()` bakes the value into the widget HTML, and the
browser reports it back. For an MCP App:

- The UI is rendered at `resources/read` (via `renderMcpAppHtml()` →
  `uiHandler(mcpFakePageRequest())`), with **no query string and no arguments**
  — the tool-call arguments are not part of `resources/read`.
- The arguments reach the *client* only afterward, via the async
  `ui/notifications/tool-input` (`ontoolinput`) notification, *after* the
  widget HTML has already rendered at its defaults.
- There is no subsequent page-GET of the app to carry the args (the resource is
  shown in a sandboxed `srcdoc` iframe; direct-connect only upgrades the
  websocket, it does not navigate the frame).

So `restoreInput()` always runs against an empty context and returns the
default. Only server-side `onRestore()` could see the args, and even that
races (see below) — so we do **not** use `RestoreContext` for MCP at all.

### The `resourceUri`-with-args idea — tested, does NOT work

We tried having the `open_*` tool result return a per-call resource URI that
encodes the arguments (`ui://shiny/app?_inputs_&n=200`), hoping the host would
pass it to `resources/read` so we could render with the args. **Claude ignores
it:** it issued `resources/read` with the static `ui://shiny/app` (no query),
~4ms *before* the tool call even returned. Hosts choose the resource from the
static tool/resource `_meta`, not from the tool-call result. Dead end.

**Consequence:** init arguments are delivered through the reactive
`mcpUpdates()` channel and applied by the author with `updateXxxInput()`. A
brief flash (default → model's value) on open is unavoidable.

## 2. There is no in-place "update the running app" — hosts re-render per call

The model's only lever on the app is calling a tool. When asked to change a
parameter, Claude **re-invokes `open_*`**, and the host renders a **fresh app
instance** (a new Shiny session with a new `session$token`) for each call —
verified: two opens produced sessions `1ec8…` then `8820…`. It does not send an
in-place tool-input update to the already-rendered instance, and the model has
no "update" verb to do so.

**Consequence:**

- "Initial open" and "re-open with new args" are the *same event* to the app —
  which is why a single `mcpUpdates()` observer handles both.
- True live re-steering of a running instance requires an explicit
  server-side update tool the model can call. Tracked in
  rstudio/shiny#4415.

## 3. The init-vs-update channel was a race (now removed)

An earlier design split init args into `RestoreContext`/`onRestore` and later
args into `mcpUpdates()`, gated by a timeout on when `ontoolinput` arrived
relative to socket start. In practice the *same* opening arguments landed on
different channels purely by timing (session A → `mcpUpdates`, session B →
`onRestore`), so an author couldn't predict which fired. Resolved by routing
**every** `ontoolinput` (opening and later) to the single `mcpUpdates()`
channel.

## Summary

| Wanted | Reality |
|---|---|
| Flash-free widget auto-restore of init args | Not possible — args aren't available at render time |
| Args via a per-call `resourceUri` | Host ignores it; reads the static URI |
| Live in-place update of a running app | Host re-renders per tool call; needs an update tool (#4415) |
| Deterministic init channel | Achieved by using one channel (`mcpUpdates()`) for all args |
