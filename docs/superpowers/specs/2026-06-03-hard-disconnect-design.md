# Hard-Disconnect Mode — Design

**Status:** Draft
**Author:** E Nelson
**Date:** 2026-06-03
**Branch:** `feat/hard-disconnect`

## Summary

Add an opt-in `hardDisconnect` mode to `shinyApp()`. When enabled, every
websocket close — whether the browser dropped or the server called
`session$close()` — performs a complete teardown in three coordinated
tiers:

1. **In-process teardown.** Run the full root-namespace destroy walk
   *and* clear the `inputs`, `clientData`, `downloads`, and `files` Maps
   that today survive `wsClosed()`. Drop framework references to the
   `SessionProxy` so it becomes garbage-collectable.
2. **Hosting-layer signal.** Tell Shiny Server / Posit Connect at
   handshake time that sessions for this app are hard-disconnect and
   must not be held for reconnect grace periods.
3. **Client-side closed state.** Render a distinct closed-state overlay,
   skip reconnect, tear down JS Shiny state, fire a `shiny:closed`
   event.

Default behavior is unchanged. With `hardDisconnect = FALSE` (the
default), every code path is byte-for-byte identical to today.

## Motivation

Today, when a Shiny websocket closes:

- `wsClosed()` suspends outputs, fires `onSessionEnded`, and walks the
  destroy callbacks for every namespace — but it leaves the root
  `inputs`, `clientData`, `downloads`, and `files` Maps in place
  (guarded by `if (!isRoot)` in `invokeDestroyCallbacks`). The
  `SessionProxy` lingers until GC.
- On hosted deployments, Shiny Server / Connect may hold the R worker
  warm for a reconnect-token grace period, prolonging the lifetime of
  per-session state in the platform even after Shiny considers the
  session ended.
- `allowReconnect(FALSE)` already prevents the client from attempting a
  reconnect, but it does not change in-process cleanup or hosting-layer
  behavior. There is no way for an author to say "this session is truly
  done — release everything now."

Hard-disconnect mode gives authors that affordance for apps that want
predictable resource release (apps with expensive per-session state,
kiosk-style flows, apps with explicit "I'm done" semantics) and for
end-user-visible "this app has closed" experiences.

## Non-goals

- Killing the R worker process itself. The hosting platform's worker
  lifecycle decides that.
- Per-session toggling of hard mode inside the server function. Hard
  mode is a per-app policy set at `shinyApp()` construction.
- Replacing or deprecating `allowReconnect()`. The two coexist; hard
  mode supersedes reconnect signaling for the app while it is enabled.
- Changes to logging, billing, or auth in the hosting layer.

## R API

Two new arguments on `shinyApp()` (and parallel pass-through on
`runApp()`):

```r
shinyApp(
  ui, server,
  hardDisconnect = FALSE,
  hardDisconnectMessage = NULL,
  ...
)
```

- `hardDisconnect`: single logical, default `FALSE`. When `TRUE`, this
  app uses hard-disconnect semantics for every session.
- `hardDisconnectMessage`: single string or `NULL`. The text shown in
  the browser overlay after a hard disconnect. `NULL` selects the
  default, `"This app has closed."`.

These propagate via the existing `appOptions` plumbing into each
session at `ShinySession$initialize`, where they are stored as
`private$hardDisconnect` (logical) and `private$hardDisconnectMessage`
(character). No new public session methods — authors do not toggle this
per session.

## Architecture

Hard mode is a session-scoped flag, read at exactly two moments:

1. When `wsClosed()` runs, to decide soft vs. hard teardown.
2. When `session$allowReconnect()` is called, to warn-and-ignore.

No code path branches on the flag during normal request handling. The
steady-state behavior is identical between modes.

### Server-side teardown order in `wsClosed()` under hard mode

1. `self$closed <- TRUE` (as today).
2. Invoke `onSessionEnded` / `closedCallbacks` (as today — author code
   still sees a live `input` / `clientData`).
3. If this is a server-initiated close, the websocket close frame has
   already been sent with code `4001` and reason
   `"shiny-hard-disconnect"` by `session$close()`. On the natural
   client-disconnect path, the socket is already closed and this step
   is irrelevant.
4. Run the full destroy walk: invoke destroy callbacks for every
   namespace (already happens), and *additionally* clear root-level
   `inputs`, `clientData`, `downloads`, and `files`.
5. Drop the framework's reference to this `SessionProxy` from any
   registry that holds it (`appsBySessionId`-style) so GC can reclaim
   it.

Soft mode's `wsClosed()` is unchanged.

### `session$close()` under hard mode

`close()` already closes the websocket. Under hard mode it additionally
passes close code `4001` and reason `"shiny-hard-disconnect"` to
`private$websocket$close()`. The downstream `wsClosed` callback fires
as today and follows the hard path.

## Wire protocol

Three pieces, each in its natural place.

### 1. Handshake (HTTP upgrade response) → hosting layer

When `hardDisconnect = TRUE`, the websocket upgrade response includes:

```
Shiny-Hard-Disconnect: true
```

Set in the same code path that handles the upgrade request. Shiny
Server / Connect read this once at handshake and tag the worker /
session so they know not to hold it for reconnect on any disconnect.

### 2. Session init (websocket message) → client

Immediately after the existing init handshake (the same point where
`allowReconnect` is sent today), the server sends:

```json
{ "custom": { "hardDisconnectConfig": { "message": "<author text>" } } }
```

The client registers a one-shot message handler keyed on
`hardDisconnectConfig` that stashes the message on the `ShinyApp`
instance (`this.$hardDisconnectMessage`) and sets `this.$hardDisconnect
= true`. If hard mode is off, the message is not sent and the flag stays
`false`.

### 3. Termination (websocket close) → both

When the server tears down a session, it closes the websocket with:

```
code: 4001
reason: "shiny-hard-disconnect"
```

Code `4001` is in the private application range (4000–4999) and is
reserved here for "session terminated, do not reconnect or reattach."
Code only — no payload coordination needed; the author message was
pre-staged at init.

### Why this shape

- The termination path carries one signal: a close code. No race
  between a payload message and a close frame.
- The author's free-form text travels at init, where it isn't
  time-critical and has no size constraint (websocket close `reason` is
  capped at 123 bytes).
- The hosting layer learns the policy at handshake — when it actually
  needs to make the worker-pool decision — not at termination.
- Each consumer (hosting platform, client JS) gets exactly what it
  needs in the shape it natively inspects.

## Client-side behavior

The `socket.onclose` handler in `srcts/src/shiny/shinyapp.ts` gains a
check before the existing reconnect logic:

```ts
if (e.code === 4001) {
  this.$enterClosedState();
  return;
}
```

`$enterClosedState()` does:

1. **Render `#shiny-closed-overlay`** (new DOM id, separate SCSS).
   Uses `this.$hardDisconnectMessage` if set, otherwise
   `"This app has closed."`. The existing grey
   `#shiny-disconnected-overlay` is *not* shown — closed state replaces
   it.
2. **Skip the reconnect path entirely**, regardless of any
   `$allowReconnect` value. The early return takes care of it; no need
   to mutate `$allowReconnect`.
3. **Tear down JS Shiny state.** Stop the action queue loop, null the
   socket, clear any scheduled reconnect timer, dispose input/output
   bindings. The page becomes inert.
4. **Fire `shiny:closed`** on `document` with `detail: { message }`.
   Authors who want custom UX listen and replace or extend the overlay.
5. **Post `closed` to `window.parent`** in addition to the existing
   `disconnected` postMessage, so iframe embedders can distinguish hard
   from soft.

On the natural-client-disconnect path (tab closed, network drop), the
server runs hard teardown server-side as usual. There is no client to
send a close code to, and the browser is already gone or detached. No
client-side behavior needs to change for this case — it is purely
server hygiene plus the handshake-time signal that the hosting layer
already received.

## Interactions

### `allowReconnect()`

Under hard mode, `session$allowReconnect()` is a no-op with a one-time
`warning()` per session:

> `allowReconnect()` has no effect when `hardDisconnect = TRUE`.

No exception is thrown. The warning fires on the first call only;
subsequent calls within the same session are silent.

### `onSessionEnded` / `closedCallbacks`

These run *before* the hard cleanup of `inputs` / `clientData` /
`downloads` / `files`. Author teardown code can still read session
state.

### `session$destroy()` and modules

Unchanged. Modules are destroyed by the existing root-namespace destroy
walk that already runs in `wsClosed()`. Hard mode adds the cleanup of
the four Maps and the `SessionProxy` reference; module behavior is
identical.

### `startApp()` / local runs

Hard mode works the same locally. Tier 1 (in-process teardown) and tier
3 (client UX + close code) function fully. The handshake header is set
but has no consumer — locally there is no hosting layer to read it.
The feature degrades gracefully.

### Default-off invariant

Every code path in hard mode is gated by the per-session
`hardDisconnect` flag. With `hardDisconnect = FALSE` (default),
`wsClosed` is byte-for-byte identical to today: no init message is
sent, no header is set, no close code is overridden. Existing apps see
zero behavior change.

## Testing

- **R unit tests** (`tests/testthat/`): construct a session with
  `hardDisconnect = TRUE`, drive `wsClosed`, assert root `inputs` /
  `clientData` / `downloads` / `files` are empty and the `SessionProxy`
  reference is dropped from the relevant registry. Symmetric test with
  `hardDisconnect = FALSE` asserts the four Maps survive.
- **R warning test:** `session$allowReconnect(TRUE)` under hard mode
  emits one warning; second call is silent.
- **TypeScript / DOM tests:** in the existing `srcts` test harness,
  simulate `socket.onclose` with code `4001` and assert
  `#shiny-closed-overlay` appears, `#shiny-disconnected-overlay` does
  not, `shiny:closed` fires, and no reconnect is scheduled.
- **Integration test (shinytest2):** a small app with `hardDisconnect =
  TRUE`. Programmatically close the session and assert the browser
  ends up in the closed state with the author's message and no
  reconnect attempt within a generous wait window.
- **Wire snapshot test:** assert that with `hardDisconnect = TRUE` the
  upgrade response includes the `Shiny-Hard-Disconnect: true` header
  and the init message includes `hardDisconnectConfig`. With `FALSE`,
  neither appears.

## Hosting platform handoff

The brief below is what to bring to the Shiny Server / Posit Connect
teams. Shiny implements the contract; the platforms need to honor it
for the feature to deliver its full value on hosted deployments.
Without platform-side changes, tiers 1 (in-process) and 3 (client UX)
still work; tier 2 (worker-pool release) only realizes on platforms
that implement the contract.

---

> **Subject:** Honoring `hardDisconnect` in Shiny Server / Connect
>
> **Background.** Open-source Shiny is adding an opt-in
> `hardDisconnect = TRUE` mode on `shinyApp()`. When enabled, the
> app's author has declared that sessions should *not* be held for
> reconnection — every disconnect, whether client- or server-initiated,
> is terminal. This is intended for apps that want predictable resource
> release (apps that hold expensive per-session resources, kiosk apps,
> apps with explicit "I'm done" semantics) and for end-user-visible
> "this app has closed" experiences.
>
> **What Shiny does.** Three things, every one of them tied to the
> per-app flag:
>
> 1. On the websocket upgrade response, Shiny includes the HTTP header
>    `Shiny-Hard-Disconnect: true`.
> 2. Sessions terminated server-side close the websocket with code
>    `4001` and reason `shiny-hard-disconnect`.
> 3. Inside the R process, every disconnect runs the full teardown —
>    module scopes, `inputs`, `clientData`, `downloads`, `files`, and
>    the `SessionProxy` reference are all released immediately.
>
> **What we'd like Shiny Server / Connect to do.** Two changes:
>
> 1. **Read the `Shiny-Hard-Disconnect` header at handshake time** and
>    tag the worker / session record with the hard-disconnect policy.
>    Header is sent on every websocket upgrade response for a hard-mode
>    app; reading it once per session is sufficient.
>
> 2. **On any disconnect for a hard-mode session — client- *or*
>    server-initiated — skip the reconnect-token grace period.** Treat
>    the session as terminated immediately:
>    - Release any reconnect token associated with the session.
>    - Do not hold the R worker warm for a follow-up reconnect from the
>      same browser.
>    - Apply the platform's normal worker-recycling logic as if the
>      grace period had already elapsed.
>
>    The close code `4001` is informational: it confirms a
>    server-initiated hard close, but the platform decision should be
>    driven by the *handshake header*, not the close code, so that
>    natural client disconnects (tab closed, network drop) also
>    trigger immediate release.
>
> **What we are *not* asking the platform to do.**
>
> - Not asking the platform to kill the R worker process itself. The
>   platform's existing worker lifecycle (one process per session vs.
>   shared workers, idle timeouts, etc.) is the right place to make
>   that call. Hard mode just removes the "hold for reconnect" tier of
>   the lifecycle.
> - Not asking the platform to read or forward the
>   `hardDisconnectConfig` message — that's purely a client-side
>   concern.
> - Not asking for changes to logging, billing, or auth.
>
> **Compatibility.** Platforms that don't implement this contract are
> not broken — sessions just continue to be held for the reconnect
> grace period as they are today. Tiers 1 and 3 of the feature still
> function. The hard-mode value proposition is partially realized.
>
> **Open questions for the platform team:**
>
> - Is the upgrade-response header the right surface, or would Shiny
>   Server / Connect prefer the signal somewhere else (e.g., a field
>   in the session manifest the worker already publishes)?
> - Does Connect's session-pool behavior already have a "no grace"
>   path we'd be turning on, or does this require a new code path?
> - Any preference on the close-code value? `4001` is a Shiny choice;
>   we can move it.
> - Versioning: do we need a Shiny minimum version on the platform
>   side to begin reading the header?

## Open questions

- **Header name.** `Shiny-Hard-Disconnect` is descriptive but unusual;
  worth confirming with the hosting team that a custom header is the
  right surface vs. an existing manifest field.
- **Close code.** `4001` is unclaimed by any other Shiny use today, but
  no Shiny convention exists for application-range close codes yet. If
  more land later they should share a small registry.
- **`runApp()` argument forwarding.** Today `runApp()` accepts an
  `appDir` or a `shinyApp` object. When `runApp()` is called directly
  with `appDir`, the `hardDisconnect` setting must come from the
  loaded app's `shinyApp()` call, not from `runApp()` itself. The
  pass-through detail is an implementation question, not a design
  question.
