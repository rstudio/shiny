# Hard-Disconnect — Design

**Status:** Draft
**Author:** E Nelson
**Date:** 2026-06-03
**Branch:** `feat/hard-disconnect`

## Summary

Add opt-in hard-disconnect semantics to Shiny along two trigger
pathways. There is no app-wide "every disconnect is hard" mode; hard
ends are always *explicitly* requested by author code or by an
idle-timeout the author has configured.

- **Per-call trigger.** `session$close(hard = TRUE, message = ...)`
  lets an author force a hard close from server code in response to a
  user action (e.g., Submit, Log Out) — typically paired with a modal
  acknowledging the action.
- **Idle timeout.** `shinyApp(hardDisconnectAfter = N)` auto-fires a
  hard close after N seconds of client inactivity. The trigger is
  app-wide configuration; the resulting close goes through the same
  `session$close(hard = TRUE)` path as the per-call form.

In addition, `session$close()` is elevated from an effectively-internal
method to documented public API. It exists today but is not listed in
the documented session methods; this change lists it explicitly and
adds the new `hard` and `message` arguments.

A hard close performs a complete teardown in three coordinated tiers:

1. **In-process teardown.** Run the full root-namespace destroy walk
   *and* clear the `inputs`, `clientData`, `downloads`, and `files`
   Maps that today survive `wsClosed()`. Drop framework references to
   the `SessionProxy` so it becomes garbage-collectable.
2. **Hosting-layer signal.** Close the websocket with code `4001` so
   Shiny Server / Posit Connect can recognize "this session is
   terminal — don't hold the worker for a reconnect grace period."
3. **Client-side closed state.** Render a distinct closed-state
   overlay, skip reconnect, tear down JS Shiny state, fire a
   `shiny:closed` event.

Default behavior is unchanged. An author who never calls
`session$close(hard = TRUE)` and never sets `hardDisconnectAfter`
gets exactly today's behavior on every disconnect.

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
- `allowReconnect(FALSE)` already prevents the client from attempting
  a reconnect, but it does not change in-process cleanup or
  hosting-layer behavior.
- There is no way for an author to say "this user has finished what
  they came for — end the session immediately" or "auto-end this
  session after N idle seconds."

Hard-disconnect gives authors two affordances on top of today's soft
behavior:

- A per-call trigger to actively end a session in response to user
  actions ("Submit and exit," "Log out and close").
- An idle-timeout knob to bound the lifetime of inactive sessions
  without waiting for the hosting platform's default grace period.

In both cases the end user sees an explicit "this app has closed"
state instead of the ambiguous grey "disconnected" overlay.

## Non-goals

- An app-wide "every disconnect is hard" mode. (Considered and
  dropped from scope; if useful later, it can be added additively as
  a `hardDisconnect = TRUE` argument on `shinyApp()` that simply
  flips the per-session default for `close(hard = NULL)`.)
- Killing the R worker process itself. The hosting platform's worker
  lifecycle decides that. Shiny just emits the signal.
- A general `session$setHardDisconnect()` toggle that mutates session
  policy mid-server-function. The only per-event control is the
  `hard` argument on `session$close()`.
- Replacing or deprecating `allowReconnect()`. The two coexist; a
  hard close supersedes reconnect signaling for that session.
- Changes to logging, billing, or auth in the hosting layer.

## R API

### Two new arguments on `shinyApp()` / `runApp()`

```r
shinyApp(
  ui, server,
  hardDisconnectMessage = NULL,
  hardDisconnectAfter = NULL,
  ...
)
```

- `hardDisconnectMessage`: single string or `NULL`. The default text
  shown in the browser overlay after a hard disconnect, used as the
  fallback when `session$close(message=)` isn't supplied (and as the
  idle-timeout message). `NULL` selects the framework default,
  `"This app has closed."`.
- `hardDisconnectAfter`: single positive numeric (seconds) or `NULL`,
  default `NULL` (no idle timeout). When set, the session auto-fires
  a hard close after this many seconds of *client inactivity* (no
  incoming websocket message from the client). See "Idle timeout"
  below.

These propagate via the existing `appOptions` plumbing into each
session at `ShinySession$initialize` and are stored as
`private$hardDisconnectMessage` (character) and
`private$hardDisconnectAfter` (numeric or NULL).

### New arguments on `session$close()`

`session$close()` gains two arguments:

```r
session$close(hard = FALSE, message = NULL)
```

- `hard`: single logical, default `FALSE`. When `TRUE`, this close
  runs the full hard teardown (close code `4001`, in-process cleanup,
  closed-state client UX). When `FALSE`, behavior is identical to
  today's `session$close()`.
- `message`: single string or `NULL`. The text shown in the closed
  overlay for this particular close. `NULL` falls back to the
  app-level `hardDisconnectMessage`, which itself falls back to the
  framework default. Only meaningful when `hard = TRUE`; ignored on
  a soft close.

Backward compatibility: existing code that calls `session$close()`
with no arguments continues to behave exactly as today.

### Idle timeout: `hardDisconnectAfter`

The idle timeout is implemented in Shiny itself, not delegated to the
hosting platform. See "Why Shiny implements this" at the end of this
section for the rationale.

When `hardDisconnectAfter = N` is set, the session tracks the time of
the last incoming websocket message from the client. A timer (using
`later::later`) checks periodically; if `now - lastClientActivity > N`,
the session auto-invokes the equivalent of `session$close(hard = TRUE,
message = <idle message>)`.

- **What counts as activity.** Any incoming websocket message from
  client to server: input updates, custom messages from client JS,
  reactive flush acknowledgments, etc. Server-initiated pushes (output
  updates the server sends to the client) do *not* count — the timeout
  is about end-user activity, not app activity.
- **Idle message.** The closed-overlay text uses
  `hardDisconnectMessage` if set, otherwise the framework default
  `"Your session has ended due to inactivity."` This is a distinct
  framework default from the generic per-call default so the end user
  understands what happened.
- **Granularity.** The timer needn't be precise. Polling every
  `max(1, min(hardDisconnectAfter / 10, 5))` seconds keeps overhead
  negligible and bounds the actual disconnect within a small window
  of the configured threshold.
- **Reset on activity.** Each incoming message updates
  `private$lastClientActivity`. The next timer tick uses the updated
  value; no need to cancel and re-schedule the timer itself.

Example:

```r
shinyApp(
  ui, server,
  hardDisconnectAfter = 600,    # 10 minutes idle → end the session
  hardDisconnectMessage = "Your session ended after 10 minutes of inactivity."
)
```

#### Why Shiny implements this (rather than delegating to the platform)

A reasonable alternative would be to pass the timeout to Shiny Server
/ Connect via a handshake header and let the platform enforce it.
Looking at shiny-server's architecture (`lib/scheduler/worker-entry.js`),
the platform tracks only **connection counts** per worker (`httpConn`,
`sockConn`, `pendingConn`). Its `app_idle_timeout` config starts a
worker-reap timer only when those counts reach zero — it has no
mechanism today to detect "the websocket is open but the user is
inactive." Adding that would require per-session activity tracking,
frame inspection, and per-session timers in the proxy layer; a
substantial new mechanism in shiny-server.

In Shiny, by contrast, every incoming client message already lands at
a single point (`ws$onMessage` in `R/server.R`). Updating a timestamp
there is one line, and a `later::later` polling callback is trivial.
The work is small where it belongs (in the R session, which actually
sees user activity) and large where it doesn't (in the proxy, which
intentionally doesn't).

Additional reasons:

- **Works locally.** `runApp()` has no platform; Shiny-side
  implementation gives `hardDisconnectAfter` the same meaning in dev
  and prod.
- **Policy travels with the app.** The author specifies the timeout
  in `shinyApp()`; behavior doesn't depend on per-deployment platform
  config.
- **The hard-close UX needs Shiny-side participation anyway.** The
  `hardDisconnectConfig` custom message carrying the author's idle
  text has to come from R. If the platform drove the close, Shiny
  would still need a way to deliver the message in time, which
  re-introduces machinery.

If platform-side optimization ever becomes desirable (e.g., for
extremely high-density deployments where per-session timers in R
add measurable overhead), a future change can add a hint header so
the platform can elide the R-side timer. The R-side detection
remains the source of truth either way.

### Typical usage: author-triggered end-of-session

A common pattern: the user submits a form or logs out, the app shows
a modal acknowledging the action, then the session hard-closes.

```r
observeEvent(input$submit, {
  saveSubmission(input)

  showModal(modalDialog(
    title = "Thanks!",
    "Your responses have been recorded. This app will close in a few seconds.",
    footer = NULL,
    easyClose = FALSE
  ))

  later::later(
    ~ session$close(hard = TRUE, message = "Thanks for your submission."),
    delay = 3
  )
})
```

The modal renders to the client immediately. After three seconds,
`session$close(hard = TRUE)` runs hard teardown server-side and sends
the client into the closed state with the supplied message replacing
the modal.

A confirm-then-close variant uses a button in the modal:

```r
observeEvent(input$logout, {
  showModal(modalDialog(
    "Are you sure you want to log out?",
    footer = tagList(
      modalButton("Cancel"),
      actionButton("logout_confirm", "Log out", class = "btn-danger")
    )
  ))
})

observeEvent(input$logout_confirm, {
  removeModal()
  session$close(hard = TRUE, message = "You've been logged out.")
})
```

Authors who want a hard close *without* a custom message can just
call `session$close(hard = TRUE)` — the closed overlay falls back to
`hardDisconnectMessage` or the framework default.

### What's stored where

| Setting                                | Source                              | When read |
|----------------------------------------|-------------------------------------|-----------|
| `private$hardDisconnectMessage` (chr)  | `shinyApp(hardDisconnectMessage=)`  | At session init; used as fallback when `close(message=)` is `NULL` |
| `private$hardDisconnectAfter` (numeric)| `shinyApp(hardDisconnectAfter=)`    | At session init; arms an idle-timeout timer that, on expiry, invokes `close(hard = TRUE)` |
| `private$lastClientActivity` (POSIXct) | Updated on every incoming ws message| Read by the idle-timeout timer tick |
| `hard` (per-call)                      | `session$close(hard=)`              | At `close()` invocation; if `TRUE`, this close is a hard close |
| `message` (per-call)                   | `session$close(message=)`           | At `close()` invocation; overrides `hardDisconnectMessage` for this close |

The natural-client-disconnect path (browser tab closed, network drop)
has no per-call site and never triggers a hard close. Soft cleanup
runs as today.

### Documentation: elevating `session$close()`

`close()` exists today as a public R6 method on `ShinySession`, but
is not listed in the documented session methods in `?session` — only
referenced obliquely in the `destroy()` entry ("the root session is
torn down via `close()`"). This spec promotes `close()` to a fully
documented member of the public session API:

- Add an entry to the session methods list in `R/shiny.R` describing
  `close(hard = FALSE, message = NULL)`, its arguments, and its
  interaction with `onSessionEnded` / `onDestroy` callbacks.
- Add a roxygen example showing the modal-then-close pattern.
- Add an `@seealso` cross-reference between `close()`,
  `onSessionEnded()`, and `destroy()`.

This is a documentation-only change for the no-argument call;
existing code that calls `session$close()` continues to work
unchanged. The new arguments are additive.

## Architecture

Whether a given close is hard is decided at exactly one point: the
`hard` argument to `session$close()`. The idle-timer's auto-close
goes through the same function. There is no session-wide "mode" flag.

### Server-side teardown order in `wsClosed()` for a hard close

A hard close arises when `session$close(hard = TRUE)` has just been
called (either by author code or by the idle timer). The websocket
close frame is sent with code `4001` and reason
`"shiny-hard-disconnect"`. Then `wsClosed()` runs:

1. `self$closed <- TRUE` (as today).
2. Invoke `onSessionEnded` / `closedCallbacks` (as today — author
   code still sees a live `input` / `clientData`).
3. Run the full destroy walk: invoke destroy callbacks for every
   namespace (already happens), and *additionally* clear root-level
   `inputs`, `clientData`, `downloads`, and `files` (the four Maps
   guarded by `if (!isRoot)` today).
4. Drop the framework's reference to this `SessionProxy` from any
   registry that holds it (`appsBySessionId`-style) so GC can
   reclaim it.

`wsClosed()` needs to know whether the close was hard. The signal is
the close code on the websocket frame Shiny just sent — but
`wsClosed()` runs server-side and doesn't naturally see what code
was sent. Implementation: `session$close(hard, ...)` sets a
`private$wasHardClose` flag on the session *before* closing the
socket. `wsClosed()` reads the flag to decide hard vs. soft. The
flag has no other reader.

A soft `wsClosed()` (the natural-disconnect path or
`session$close()` / `session$close(hard = FALSE)`) is unchanged from
today.

### `session$close(hard, message)` flow

1. If `hard = TRUE`:
   - Set `private$wasHardClose <- TRUE`.
   - Resolve `effective_message` (per-call `message`, else
     `private$hardDisconnectMessage`, else the framework default).
   - Send the runtime config message:
     `{ "custom": { "hardDisconnectConfig": { "message": "..." } } }`
   - Close the websocket with code `4001` and reason
     `"shiny-hard-disconnect"`.
2. If `hard = FALSE` (default): close the websocket normally (no
   code), `private$wasHardClose` remains `FALSE`. Equivalent to
   today's behavior.
3. The downstream `wsClosed` callback fires and follows the hard or
   soft path accordingly.

### Idle-timeout flow

When `hardDisconnectAfter` is set on the session:

1. At session init, after the websocket opens, schedule a recurring
   timer via `later::later` with polling interval
   `max(1, min(hardDisconnectAfter / 10, 5))` seconds.
2. The session updates `private$lastClientActivity <- Sys.time()`
   on every incoming websocket message (in the existing
   message-receive path).
3. Each timer tick checks
   `as.numeric(Sys.time() - lastClientActivity) > hardDisconnectAfter`.
   If true, invoke `self$close(hard = TRUE, message = <idle message>)`
   and stop rescheduling. Otherwise, reschedule the next tick.
4. The timer is cancelled (no-op) once the session is closed.

This is independent of `wsClosed()` — the timer drives `close()`,
which follows the normal flow above. No new code path in teardown.

## Wire protocol

Two pieces, both on the termination path.

### 1. Runtime config (server → client)

Immediately before the server closes the socket on a hard close, it
sends:

```json
{ "custom": { "hardDisconnectConfig": { "message": "<author text>" } } }
```

WebSocket message ordering is FIFO, so the client receives the
config first and stashes the message on the `ShinyApp` instance, then
receives the close frame and renders the closed overlay using the
freshly-stashed text. If the close ever beats the message (it
shouldn't, but as a robustness check), the client falls back to a
default `"This app has closed."`.

There is no init-time pre-staging. Every hard close sends its own
config inline.

### 2. Termination (server → client, server → hosting layer)

The server closes the websocket with:

```
code: 4001
reason: "shiny-hard-disconnect"
```

Code `4001` is in the private application range (4000–4999) and is
reserved here for "session terminated, do not reconnect or reattach."

- **Client side:** `socket.onclose` inspects `e.code`. If `4001`,
  enter the closed state (see "Client-side behavior").
- **Hosting layer:** Shiny Server / Connect should treat close code
  `4001` as authoritative "release the worker, do not hold for
  reconnect grace period." See "Hosting platform handoff."

Natural client disconnects (tab closed, network drop) never produce
a `4001` close code — the server didn't send one. The session
follows today's soft path, and the hosting platform follows its
normal worker-holding behavior.

### Why this shape

- Termination is the only point where a signal is needed. There is
  no app-wide policy to communicate at handshake.
- The custom message ahead of the close carries the author's text
  with no size constraint (websocket close `reason` is capped at
  123 bytes).
- WebSocket FIFO ordering keeps the message-then-close pair
  race-free in practice; the default-message fallback covers any
  pathological reorder.
- Each consumer (hosting platform, client JS) gets exactly what it
  needs in the shape it natively inspects.

## Client-side behavior

The `socket.onclose` handler in `srcts/src/shiny/shinyapp.ts` gains
a check before the existing reconnect logic:

```ts
if (e.code === 4001) {
  this.$enterClosedState();
  return;
}
```

A new message handler for `hardDisconnectConfig` stashes the message
on the `ShinyApp` instance (`this.$hardDisconnectMessage`) when
received.

`$enterClosedState()` does:

1. **Render `#shiny-closed-overlay`** (new DOM id, separate SCSS).
   Uses `this.$hardDisconnectMessage` if set, otherwise the default
   `"This app has closed."`. The existing grey
   `#shiny-disconnected-overlay` is *not* shown — closed state
   replaces it.
2. **Skip the reconnect path entirely**, regardless of any
   `$allowReconnect` value. The early return takes care of it; no
   need to mutate `$allowReconnect`.
3. **Tear down JS Shiny state.** Stop the action queue loop, null
   the socket, clear any scheduled reconnect timer, dispose
   input/output bindings. The page becomes inert.
4. **Fire `shiny:closed`** on `document` with
   `detail: { message }`. Authors who want custom UX listen and
   replace or extend the overlay.
5. **Post `closed` to `window.parent`** in addition to the existing
   `disconnected` postMessage, so iframe embedders can distinguish
   hard from soft.

## Interactions

### `allowReconnect()`

`allowReconnect()` continues to work as today for soft closes. A
hard close (via `close(hard = TRUE)` or the idle timer) bypasses
reconnect via the `4001` close code regardless of any prior
`allowReconnect()` call. No warning is emitted; the call simply has
no effect on the hard-close path because the teardown has already
run.

### `onSessionEnded` / `closedCallbacks`

These run *before* the hard cleanup of `inputs` / `clientData` /
`downloads` / `files`. Author teardown code can still read session
state.

### `session$destroy()` and modules

Unchanged. Modules are destroyed by the existing root-namespace
destroy walk that already runs in `wsClosed()`. A hard close adds
the cleanup of the four Maps and the `SessionProxy` reference;
module behavior is identical.

### `startApp()` / local runs

Hard close works the same locally. Tier 1 (in-process teardown) and
tier 3 (client UX + close code) function fully. Tier 2 (hosting
signal) is a no-op locally — there's no hosting layer to read the
close code, and that's fine. The feature degrades gracefully.

### Default-off invariant

Every code path that runs hard cleanup is gated by `hard = TRUE` on
the call site (either author-supplied or idle-timer-supplied). An
author who never passes `hard = TRUE` and never sets
`hardDisconnectAfter` sees today's behavior on every disconnect.

The new `hard` and `message` arguments to `session$close()` are
backward-compatible additions; existing callers passing no
arguments get today's behavior.

## Testing

- **R unit tests** (`tests/testthat/`): construct a session, call
  `session$close(hard = TRUE, message = "bye")`, drive `wsClosed`,
  assert (a) a `hardDisconnectConfig` message was sent with
  `"bye"`, (b) the websocket close used code `4001`, (c) root
  `inputs` / `clientData` / `downloads` / `files` are empty, and
  (d) the `SessionProxy` reference is dropped from the relevant
  registry.
- **Soft close unchanged:** call `session$close()` (no args) and
  assert no `4001` close code, no `hardDisconnectConfig` message,
  and the four Maps survive.
- **Message fallback chain:** with `hardDisconnectMessage = "app
  default"` and `session$close(hard = TRUE)` (no message), assert
  the runtime config carries `"app default"`. With both set, the
  per-call message wins.
- **Idle timeout fires hard close:** with `hardDisconnectAfter = 1`
  (or whatever small value works with the test harness's fake
  clock), drive a session, advance time past the threshold without
  sending any incoming message, and assert a hard close fires with
  code `4001` and the four Maps are cleared.
- **Idle timer resets on activity:** with `hardDisconnectAfter = 5`,
  advance time by 3 seconds, send an incoming message, advance
  another 3 seconds (total 6 > 5), and assert the session is still
  alive (the activity reset the effective clock).
- **TypeScript / DOM tests:** in the existing `srcts` test harness,
  simulate `socket.onclose` with code `4001` and assert
  `#shiny-closed-overlay` appears, `#shiny-disconnected-overlay`
  does not, `shiny:closed` fires, and no reconnect is scheduled.
  Separately, simulate a normal close (no `4001`) and assert the
  existing soft-disconnect path runs unchanged.
- **Integration test (shinytest2):** a small app with a button that
  calls `session$close(hard = TRUE, message = "done")`. Click the
  button and assert the browser ends up in the closed state with
  `"done"` and no reconnect attempt within a generous wait window.

## Hosting platform handoff

The brief below is what to bring to the Shiny Server / Posit Connect
teams. Shiny implements the contract; the platforms need to honor
it for the feature to deliver its full value on hosted deployments.
Without platform-side changes, tiers 1 (in-process) and 3 (client
UX) still work; tier 2 (worker-pool release) only realizes on
platforms that implement the contract.

---

> **Subject:** Honoring hard-disconnect close code in Shiny Server / Connect
>
> **Background.** Open-source Shiny is adding two opt-in
> hard-disconnect affordances:
>
> 1. A per-call trigger: `session$close(hard = TRUE, message = ...)`,
>    callable from inside an `observeEvent` or any server-side code.
>    A single session ends terminally on author command. The typical
>    use case is a user action like Submit or Log Out, often paired
>    with a modal acknowledging the action, after which the session
>    is destroyed.
>
> 2. An idle timeout: `shinyApp(hardDisconnectAfter = N)`. The
>    session auto-fires a hard close after N seconds of client
>    inactivity. Internally this just calls
>    `session$close(hard = TRUE)`.
>
> There is no app-wide "every disconnect is hard" mode. Natural
> client disconnects (tab closed, network drop) continue to follow
> today's soft behavior.
>
> **What Shiny does.** Two things, both on the termination path:
>
> 1. Before closing the websocket on a hard close, Shiny sends one
>    `hardDisconnectConfig` custom message carrying the author's
>    closed-overlay text. This is purely a client-side concern.
> 2. Shiny closes the websocket with code `4001` and reason
>    `shiny-hard-disconnect`. Inside the R process, the full
>    teardown runs — module scopes, `inputs`, `clientData`,
>    `downloads`, `files`, and the `SessionProxy` reference are all
>    released immediately.
>
> **What we'd like Shiny Server / Connect to do.** One change.
>
> **Treat websocket close code `4001` as equivalent to
> `app_idle_timeout = 0` for this worker.** Today, when a session's
> websocket closes, the scheduler decrements `sockConn`. If counts
> reach zero, `WorkerEntry.startIdleTimer()` schedules a worker reap
> after `idleTimeout` milliseconds. The ask is: when the closing
> websocket carries close code `4001`, skip the wait — treat it as
> if `idleTimeout` were `0` for this worker, so the reap fires
> immediately and any reconnect token tied to the session is
> released.
>
> Pointer to the relevant code: `lib/scheduler/worker-entry.js`,
> specifically the `release()` method (which calls
> `startIdleTimer()`) and the `startIdleTimer()` method itself.
> The change is small in scope — propagate the close code from the
> socket close event through to the scheduler so `startIdleTimer()`
> can short-circuit to immediate emission of `idletimeout` when the
> code is `4001`.
>
> No other close codes need to change behavior; the existing
> `app_idle_timeout` grace-period flow for normal closes is exactly
> the right thing when an author hasn't opted in to hard close.
>
> **What we are *not* asking the platform to do.**
>
> - Not asking the platform to kill the R worker process itself.
>   The platform's existing worker lifecycle (one process per
>   session vs. shared workers, idle timeouts, etc.) is the right
>   place to make that call. Hard close just removes the "hold for
>   reconnect" tier of the lifecycle for sessions that ended
>   terminally.
> - Not asking the platform to read or forward the
>   `hardDisconnectConfig` message — that's purely a client-side
>   concern.
> - Not asking for changes to logging, billing, or auth.
> - Not asking for any handshake-level signal. Hard-disconnect is a
>   per-close decision, not an app-wide policy.
>
> **Compatibility.** Platforms that don't implement this contract
> are not broken — hard-closed sessions just continue to be held
> for the reconnect grace period as they are today. Tiers 1 and 3
> of the feature still function. The hard-disconnect value
> proposition is partially realized.
>
> **Open questions for the platform team:**
>
> - For Shiny Server: confirming the proposed touch points in
>   `lib/scheduler/worker-entry.js` are the right ones, and whether
>   the close-code propagation should happen in the scheduler or
>   one layer up in the proxy (`lib/proxy/`).
> - For Connect: does the session-pool behavior already have a "no
>   grace" path we'd be turning on, or does this require a new code
>   path? The shiny-server-shaped fix won't translate directly.
> - Any preference on the close-code value? `4001` is a Shiny
>   choice; we can move it.
> - Versioning: do we need a Shiny minimum version on the platform
>   side to begin honoring the code?
> - Activity-driven (rather than connection-driven) idle timeouts
>   are *not* part of this ask. Shiny implements them in R because
>   the proxy doesn't have a natural seam for per-session activity
>   tracking. If platform teams want to optimize this later, that's
>   a separate conversation; the close-code change above is the
>   only thing this design depends on.

## Open questions

- **Close code value.** `4001` is unclaimed by any other Shiny use
  today, but no Shiny convention exists for application-range close
  codes yet. If more land later they should share a small registry.
- **Default idle message.** "Your session has ended due to
  inactivity." reads well but is the framework's choice. Worth
  flagging for review.
- **Activity definition.** "Any incoming websocket message" is the
  simplest rule. If we later find authors want to count only certain
  message types (e.g., excluding internal heartbeats), the
  definition can be tightened.
- **`runApp()` argument forwarding.** Today `runApp()` accepts an
  `appDir` or a `shinyApp` object. When `runApp()` is called
  directly with `appDir`, the `hardDisconnectMessage` /
  `hardDisconnectAfter` settings must come from the loaded app's
  `shinyApp()` call, not from `runApp()` itself. The pass-through
  detail is an implementation question, not a design question.
