# Open questions for Barret (2026-07-10)

Small decisions I made autonomously that deserve a yes/no or a rename before
this ships. Everything works as described and is E2E-verified; these are
judgment calls, not blockers.

## 1. Naming of the option family and exported functions

Current surface, all marked experimental:

- Options: `shiny.mcp`, `shiny.mcp.stdio`, `shiny.mcp.direct`,
  `shiny.mcp.tool` (name/description/inputSchema), `shiny.mcp.tools`
  (author tools), `shiny.mcp.displayModes`.
- Exported: `isMcpSession()`, `mcpToolInput()`, `mcpHostContext()`,
  `mcpUpdateModelContext()`, `mcpSendMessage()`, `mcpRequestDisplayMode()`
  (one Rd page, `?mcp-session`).

**Question:** happy with `mcp*` camelCase + `shiny.mcp.*`? Renames are
cheap now, expensive after release. (An alternative would be a single
`shinyOptions(mcp = list(...))` umbrella.)

## 2. `shiny.mcp.direct` defaults to TRUE

The resource declares the app's origin in `_meta.ui.csp.connectDomains`
(derived per-request from `Host`/`X-Forwarded-Proto`, falling back to the
local httpuv origin for stdio). The bridge tries a real WebSocket
(`<origin>/websocket/?mcp=1`, 2.5 s timeout) and falls back to the tunnel.
On CSP-honoring hosts this makes reactivity latency native-grade.

**Question:** OK as an on-by-default? The cost on non-compliant hosts is
one blocked WS attempt before fallback. Also note: apps *deployed under a
sub-path* (e.g. Connect `/content/abc/`) will always fall back — origin
alone can't express the path. Worth a `shiny.mcp.origin` override option?

## 3. Direct-mode HTTP side channels stay tunneled (no CORS)

In direct mode I deliberately did NOT add CORS headers to the app so the
sandbox could XHR it directly — permissive CORS on a localhost app would
let any website in the user's browser read app responses. Instead,
`_shiny_http` now works without a `connectionId` (session endpoints are
still guarded by their unguessable tokens).

**Question:** agree with that trade-off, or would you rather offer opt-in
CORS pinned to a configured sandbox origin?

## 4. mcptools upstreaming

`mcptools-upstream-notes.md` documents the four gaps (resources, tool
`_meta`, async tools, embeddable dispatch) with a suggested API shape.

**Question:** should I file these as issues on posit-dev/mcptools (I
haven't touched external repos without asking)? And is "shiny delegates to
mcptools once the gaps close" the intended end state?

## 5. NEWS placeholder + PR

The NEWS bullet references `(#4404)` — a guess, since no issue/PR exists
yet. The branch is ~18 commits, every feature E2E-verified.

**Question:** open a draft PR against rstudio/shiny main now (which fixes
the real number), or keep iterating on the branch?

## 6. Smaller judgment calls (flag only if you disagree)

- Tunnel long-poll timeout 15 s, connection GC after 60 s idle.
- Font fallback formats (.ttf/.otf/.eot) are never inlined into the
  resource (browsers don't fetch them once the woff2 loads); keeps the
  resource ~2.3 MB vs ~5 MB.
- `open_shiny_app` result text is "The Shiny app is now displayed in the
  conversation." (models read this).
- Windows: stdio transport warns + no-ops (non-blocking stdin); HTTP
  transport is the documented path there.
- Display modes default declaration is all three (`inline`, `fullscreen`,
  `pip`); hosts that don't support a mode just decline
  `mcpRequestDisplayMode()`. basic-host doesn't implement display modes,
  so this is unit-tested but not E2E-verified against a real host yet.
- `shiny.sharedSecret` apps are unsupported over the tunnel (documented).
