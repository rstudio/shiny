# Comparing {shiny}'s MCP Apps support and {shinymcp}

Two independent efforts make R/Shiny work available inside AI chat
interfaces via the MCP Apps extension
([modelcontextprotocol/ext-apps](https://github.com/modelcontextprotocol/ext-apps)).
They start from opposite ends of the same problem and land on genuinely
different architectures. This document compares them for the team.

Compared versions (2026-07-13):

- **Ours**: rstudio/shiny PR #4407, branch `schloerke/shiny-mcp-app-protocol`
  (see `architecture.md` in this folder).
- **{shinymcp}**: [JamesHWade/shinymcp](https://github.com/JamesHWade/shinymcp)
  @ `2221af7` (0.0.0.9000, experimental). Standalone package by James Wade
  (GSK); ~8.3k lines of R plus a dependency-free JS bridge.

## One-sentence summaries

- **{shiny} PR #4407** runs the *actual, unmodified Shiny app* — live R
  session, real reactivity — and teaches the iframe to reach it by
  tunneling Shiny's websocket and HTTP side channels through MCP
  `tools/call` (with a direct-WebSocket fast path where CSP allows).
- **{shinymcp}** *re-expresses an app as stateless tools*: static HTML UI
  (bslib/shiny markup) + ellmer tool functions; its JS bridge watches form
  inputs whose ids match tool argument names, calls the tool on change
  (debounced), and routes the returned named list back into
  `mcp_text()`/`mcp_plot()` output slots.

## Architecture at a glance

| Axis | {shiny} PR #4407 | {shinymcp} |
|---|---|---|
| Server process | The real Shiny app (httpuv), sessions and all | Any R process; no Shiny runtime, no sessions |
| Programming model | Unchanged Shiny reactivity | UI + explicit tool functions (request/response) |
| Existing apps | Work as-is: add `mcpConfigure()` | Must be converted (manual, scaffolded, or AI-assisted) |
| App↔server channel | Tunneled websocket over `tools/call` long-poll, or direct WebSocket fast path | One `tools/call` per (debounced) input change |
| State | Server-side reactive session per viewer | Stateless between calls (state lives in input values) |
| Outputs | Everything Shiny can render (plots, htmlwidgets, dynamic UI, uploads, downloads, server-side selectize/DT) | What a tool returns: text, base64 PNG, HTML/table components |
| Tool definitions | app tool: `mcpConfigure(description=, arguments=)`; author tools: `registerMcpTool(ellmer::tool())` (see `design-register-mcp-tool.md`) | ellmer `tool()` objects (typed arguments) |
| Model-callable tools | The app tool + author-declared extras; the *session* is not model-drivable | Every tool is model-callable by design; same tools drive the UI |
| Non-Apps MCP clients | Author tools + app tool work as text; UI needs an Apps host | **Degrades gracefully everywhere** — tools work as ordinary text tools |
| Model context updates | `mcpUpdateModelContext()` — author opt-in, arbitrary content | Bridge auto-reports current input state on interaction |
| Transports | Streamable HTTP on the app's port + stdio, same dispatcher | stdio + plain HTTP (httpuv), same handler |
| Deployment | Verified on Posit Connect + shinyapps.io, incl. platform-specific external-URL headers for the fast path | Local/stdio focus; HTTP server exists, no platform-specific work |
| Multi-app | appId namespacing + gateway (`mcp/gateway/`) merges N apps into one connector | One `mcp_app()` bundles many tools/one UI; several apps = several servers |
| Extra hosts | basic-host / MCPJam / Claude Desktop for testing | Ships its own `preview_app()` reference host **and a Shiny/shinychat embed host** (`mcp_embed()`) |
| Bridge | TypeScript, bundles the official `@modelcontextprotocol/ext-apps` SDK (~400 KB) | Hand-rolled ES5, dependency-free, tracks apps spec `2026-01-26` |
| Conversion tooling | None (none needed) | parse→analyze→generate pipeline (`convert_app()`) + a deputy/AI skill for hard cases |
| Maturity signals | PR-stage on {shiny}; tests + multiple platform E2Es | Separate pkg: pkgdown site, codecov, examples gallery, vignettes |

## The fundamental trade-off

Both projects hit the same wall — **MCP Apps gives an iframe whose only
guaranteed channel is postMessage→`tools/call`, with no server push** —
and made opposite bets:

- We **preserved the Shiny programming model** and paid for it with
  transport machinery: a duck-typed websocket fed into the real session
  handler, long-poll emulation of push, an XHR shim for side channels,
  asset inlining so the single-file resource is self-contained, and
  deployment-header work so the direct fast path finds the app's external
  URL. Cost: a live R process holding a session per viewer; benefit: every
  existing app, every widget, zero rewrite.
- {shinymcp} **abandoned the session** and asks authors to flatten the
  reactive graph into pure functions. Cost: a rewrite (mitigated by their
  conversion pipeline), no cross-call server state, outputs limited to
  what a tool can return; benefit: stateless scaling, tools that work in
  *any* MCP client (not just Apps hosts), and a model that can genuinely
  drive the same tools the UI uses.

Neither bet dominates. They're answers to different questions:

- "*Put my existing dashboard in the chat*" → ours.
- "*Give the model (and the user) a typed R capability with a UI on
  top, that also works in text-only clients*" → shinymcp's model fits
  naturally.

## Things {shinymcp} does that we should notice

1. **Graceful degradation** is their headline feature: the same server is
   useful from any MCP client because tools return meaningful
   text/structured content without the UI. Our app tool returns "the app
   is now displayed" — useless in a non-Apps host. We could return a
   text/structured snapshot (e.g. the current `mcpUpdateModelContext()`
   payload) as the tool result.
2. **Auto-reporting input state** to the model context (their bridge
   collects all bound inputs and sends `ui/update-model-context` on
   interaction) is a nice zero-effort default; we require the author to
   call `mcpUpdateModelContext()`.
3. **`mcp_embed()` / shinychat host**: they can render MCP Apps *inside a
   Shiny app* (including their own preview host with a protocol log).
   That's a piece we don't have and wouldn't need to build differently —
   their host could in principle render our apps too, since both follow
   the ext-apps postMessage spec.
4. **ellmer `tool()` reuse** gives typed schemas for free and matches the
   ecosystem direction (mcptools is also ellmer-based). **Adopted** for
   author tools use `registerMcpTool(ellmer::tool())`, with the same
   ellmer→JSON-Schema conversion as `mcptools::tool_as_json()` — see
   `design-register-mcp-tool.md`. The app-opening tool is now configured
   via `mcpConfigure(description=, arguments=)`.
5. **Apps-spec version**: their bridge targets the `2026-01-26` revision;
   our bundled `@modelcontextprotocol/ext-apps` SDK should be checked
   against the same revision before CRAN.
6. **outputSchema** on tools (from `tool_outputs`) — richer tool metadata
   than we emit.

## Things our approach covers that {shinymcp} can't (by construction)

- Full reactivity and cross-input dependency logic without re-deriving it
  as functions; `insertUI`/`renderUI`, htmlwidgets, modules, bookmarking-
  style session state.
- File upload/download, server-side selectize/DataTables (their docs
  route these to the AI-assisted conversion path — i.e. human judgment).
- Zero-cost adoption for the existing app corpus (the "we already have
  400 internal Shiny apps" case), including deployed-in-place on Connect
  and shinyapps.io behind one flag.
- Latency profile *within* an interaction: after connect, direct-mode
  interactions are native websocket speed; even tunnel mode batches
  Shiny's own protocol. shinymcp pays a full tool round-trip (plus R
  cold-start work like re-rendering a ggplot to PNG) per debounced change.
- Multiple apps behind one connector (gateway), with per-app namespacing.

## Overlap / duplication

Both projects independently built: single-file HTML resources with
inlined assets, a postMessage JSON-RPC bridge, stdio + HTTP transports,
`ui/update-model-context` plumbing, CSP declaration support, and display
mode handling. That's the strongest argument for eventually converging on
shared infrastructure (e.g. the mcptools upstreaming issues we filed:
posit-dev/mcptools#115–#120 — a common R-side MCP server core would serve
both).

## Possible convergence paths

1. **Coexist, cross-link**: they solve different jobs; document when to
   use which. Cheapest, and honest.
2. **Share the server core**: both `serve()` implementations are small
   JSON-RPC dispatchers; a shared core (mcptools?) would remove the most
   duplicated, least differentiated code.
3. **Borrow features**: our side adopts graceful text degradation,
   optional ellmer tool input, input-state auto-reporting; their side
   could reuse our deployment-header logic if/when they push HTTP serving
   to Connect/shinyapps.io, and our gateway works for their servers today
   (it's protocol-level).
4. **{shinymcp} as the "escape hatch" story**: for apps that must scale
   statelessly or serve text-only clients, conversion is the right answer
   even in our world — their pipeline + skill is exactly that tooling.

## Bottom line

{shinymcp} is a well-built, complementary take: *tools-first, UI on top*,
strongest where statelessness, typed tools, and non-Apps clients matter.
Ours is *app-first, protocol underneath*, strongest where fidelity to
existing Shiny apps and zero conversion cost matter. The overlap is in
undifferentiated protocol plumbing, which is the natural place to
converge — not in the programming models, which are legitimately
different products.
