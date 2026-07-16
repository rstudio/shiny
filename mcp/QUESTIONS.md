# Open questions — answered 2026-07-13

Barret's decisions, with follow-up state. Original context preserved below
each item.

## 1. Naming (`mcp*` functions) — ✅ resolved

Configuration now uses `mcpConfigure()` (replaces the former options-based
surface). Exports: `mcpConfigure()`,
`registerMcpTool()`; session helpers `isMcpSession()`, `mcpUpdates()`,
`mcpHostContext()`, `mcpUpdateModelContext()`, `mcpSendMessage()`,
`mcpRequestDisplayMode()`.

## 2. `mcpConfigure(direct = TRUE)` default — ℹ️ explanation requested (below); default unchanged pending review

**What it does.** When the host reads `ui://shiny/app`, shiny declares the
app's origin in the resource's `_meta.ui.csp.connectDomains` (both `http(s)`
and `ws(s)` forms). Spec-compliant hosts compile that into the sandbox
iframe's Content-Security-Policy, which permits the iframe to open network
connections to that one origin. The bridge then attempts a real WebSocket to
`<origin>/websocket/?mcp=1` (2.5 s cap) before falling back to the
postMessage tunnel.

**Why it matters.** Transport latency differs by an order of magnitude. Over
the tunnel, every server→client reactive flush waits for the in-flight
`_shiny_receive` long-poll to resolve and re-arm, and each hop traverses
iframe → sandbox proxy → host page → HTTP POST → R. That's typically
50-300 ms per interaction (worse on remote connectors). The direct WebSocket
is ordinary Shiny: millisecond-scale, no host mediation, no tool-call
traffic. Side channels (uploads etc.) stay on the tunnel either way — going
direct for XHR would require CORS on the app (see Q3).

**How the origin is derived.** From the HTTP request the host itself made to
`/mcp`: `Host` header + `X-Forwarded-Proto`. So it's automatically right for
localhost (`ws://127.0.0.1:7788`) and for https tunnels/proxies
(`wss://x.trycloudflare.com`). The stdio transport has no request, so it
falls back to the local httpuv origin — correct for desktop hosts, whose
iframe runs on the same machine.

**Cost when it can't work (why default TRUE is safe).**
- Host ignores declared CSP (claude.ai today): the browser blocks the
  attempt *immediately* (synchronous SecurityError or instant error event).
  Fallback cost ≈ 0; one CSP-violation line in the console.
- Origin unreachable from the user's browser (e.g. app on a private network
  the user isn't on): connection-refused fails fast; a silently-dropped
  (filtered) route pays the full 2.5 s cap once, then the tunnel takes over.
- Reachability is user's-browser → app, not host-server → app. For
  cloudflared/Connect-style https deployments users can reach, direct works.

**Security delta of default-on: none.** The WebSocket endpoint accepts
browser connections regardless of this feature (that's just Shiny; browsers
don't apply CORS to WebSockets). Declaring the origin in CSP metadata only
tells the *sandbox* it may connect; it doesn't open anything new on the app.
The `?mcp=1` marker only switches session behavior (dependency inlining,
custom-message bridges).

**The sub-path caveat — ✅ resolved 2026-07-13.** Implemented per Barret's
request: the direct-connect base is now path-aware, derived from (in order)
`mcpConfigure(origin=)`, Posit Connect's `RStudio-Connect-App-Base-Url`
header (verified 2026-07-13 on a real connect.posit.it deployment — Connect
does *not* send `X-RSC-Request` to Shiny content, though that header is
still honored), a
host-matched rsconnect deployment record (`rsconnect/**/*.dcf` `url` — the
deployment output files), then the request origin. CSP still declares
origins only (host sanitizers reject paths); the full base feeds the ws
URL. Records require a Host match so local runs of deployed app dirs can't
split-brain against production.

**When to set FALSE:** deterministic tunnel-only transport for debugging, or
sub-path/unreachable deployments where the probe is known-futile.

## 3. Direct-mode side channels stay tunneled (no CORS) — ⏸ deferred

Revisit later. Current behavior stands: `_shiny_http` works without a
`connectionId`; no CORS headers are added to the app.

## 4. mcptools upstreaming — ✅ filed 2026-07-13

Parent tracking issue posit-dev/mcptools#115 with one issue per part:
#116 (resources), #117 (tool `_meta`), #118 (async tool handlers),
#119 (embeddable dispatch), #120 (non-blocking stdio). Note: true GitHub
sub-issue linking was permission-blocked for `schloerke` via API, so the
parent uses a task-list; convert in the UI if desired.

## 5. Draft PR — ✅ opened 2026-07-13

rstudio/shiny#4407 (draft). NEWS placeholder corrected to `(#4407)`.

## 6. Smaller judgment calls — ✅ fine as-is

Long-poll 15 s / GC 60 s; legacy font formats not inlined; tool result text;
Windows stdio unsupported; display modes declared all-three (basic-host
can't E2E them); `shiny.sharedSecret` unsupported over the tunnel.
