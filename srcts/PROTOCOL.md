# Shiny wire protocol

The client (`srcts/`) and the server (`R/server.R`, `R/shiny.R`) talk over
one WebSocket per session. Messages are JSON text (binary frames carry file
uploads). This document is language-neutral so that other implementations
(py-shiny) can follow it. Message families in outline; the **resume**
family in full.

## Connection

The client opens `ws[s]://<host><app path>websocket/`. As soon as the
socket opens the server sends `config`, and the client sends its first
message: `init`, or `resume` when it is reconnecting.

## Client → server

| method | data | when |
|---|---|---|
| `init` | object of all input values; keys may carry a `:type` suffix (`btn:shiny.action`), plus `.clientdata_*` keys | first message of a new session |
| `resume` | `{ "token"?: "<resumeToken>", "inputs": <the input set as of the moment the previous connection was lost> }` | first message when reconnecting; the `update`s queued while disconnected follow at once, in order |
| `update` | object of changed input values, same key rules | whenever inputs change |
| `unload` | `{ "method": "unload", "args": [], "tag": <n> }` | on `pagehide`, unless the page enters the back/forward cache |
| `<other>` | `{ method, args, tag, blobs? }` | RPC (`@uploadInit`, `@uploadEnd`); the server answers with `response` carrying the same `tag` |

## Server → client

`config`, `values`, `errors`, `inputMessages`, `progress`, `notification`,
`modal`, `response`, `javascript`, `console`, `allowReconnect`, `custom`,
`busy`, `recalculating`, `reload`, `shiny-insert-ui`, `shiny-remove-ui`,
`shiny-insert-tab`, `shiny-remove-tab`, `shiny-change-tab-visibility`,
`updateQueryString`, `resetBrush`, `frozen`. Each message is an object whose
top-level keys name the message types it carries; the client dispatches on
those keys.

## The resume family

### `resume` (client → server, first message when reconnecting)

```json
{ "method": "resume",
  "data": { "inputs": { "...": "the input set as of the moment the previous connection was lost" },
            "current": { "...": "the input set now" } } }
```

The server treats it as `init` with one difference: the session's restore
context (what `restoreInput()` reads) is seeded from `inputs` and marked
inactive, instead of being built from the page URL's bookmark state. So
`renderUI()` content keeps the values the user had, `onRestore()` /
`onRestored()` callbacks do not run, and a bookmark URL is not restored over
inputs changed since the page loaded. `inputs` missing or not an object is
treated as empty. Entries whose key ends in `:shiny.file` (bookmark-restore
values the page rendered into file inputs) are dropped: their files live in
a bookmark directory the new session does not have.

`inputs` is the client's input set **as of the disconnect**, not its current
one. Every change the user made while disconnected was queued as an
ordinary `update` message, and the client sends those right after `resume`,
in order. So the server sees each outage-era change exactly once, as live
input, after the restored state; nothing in `inputs` is an event.

`current` is the client's input set now. It feeds only the restore context
(what `restoreInput()` reads), so a widget rebuilt by `renderUI()` on the
first flush shows what the user has, including edits made while
disconnected, and the browser does not echo an old value back over them.
`current` missing is treated as `inputs`.

### `allowReconnect` (server → client)

```json
{ "allowReconnect": true | false | "force" }
```

Carries the app's `session$allowReconnect()` calls. `"force"` behaves as
`true`, except that it ignores a hosting shim's veto (see "Hosting shims").

### Client retry rule

After the socket closes, the client retries when the last `allowReconnect`
value it saw is `true` or `"force"`, unless the connection layer vetoed it
(see "Hosting shims" below). Delays between attempts are 1.5 s, 1.5 s,
2.5 s, 2.5 s, 5.5 s, 5.5 s, 10.5 s, 10.5 s, 10.5 s, then 30.5 s repeated
for as long as the page is open. Each retry opens a new socket and sends
`resume`. The schedule restarts when the server answers (`config`, `values`
or `resumed`), not when the socket opens. The "Attempting to reconnect"
notification shows a countdown and a **Try now** link that attempts at once.

### Hosting shims

Shiny Server and Posit Connect wrap the socket with shiny-server-client. It
retries the same session itself for a short window, then closes. It also
sets `allowReconnect` on the socket object: `true` by default and `false`
after a clean close the server meant to be final (a session idle timeout),
unless the close code is in the 4700 range. The client treats that `false`
as a veto and a missing flag as permission, so `allowReconnect(TRUE)` works
on plain sockets while a platform can still end a session for good.
`"force"` ignores the flag.

## Saved state

These apply when the app turned resume on (`enableResume()`); a server with
it off never sends a token and answers every `resume` as above.

### `config` (server → client, at socket open)

`"resumeToken": "<32 lowercase hex chars>" | null` joins `config`. It names
this session's saved state. Its presence also means the app allows
reconnecting, so the client sets its `allowReconnect` to `true` as the
frame arrives. The client keeps the token in memory only; a page reload
starts without one.

A fresh page adopts the token as `config` arrives. A reconnecting page keeps
the token it sent in `resume` until `resumed` arrives, then adopts the new
one: a drop between `config` and the server processing `resume` leaves the
old record in place, and the next attempt names it. The other side of that
window: if the socket drops after the server read (and so deleted) the record
but before the client processed `resumed`, the client still holds the used
token. The next attempt names it and resumes from inputs only, and the record
the first attempt wrote under its new token stays until its 24-hour limit.

### `resume` with a token (client → server)

```json
{ "method": "resume",
  "data": { "token": "<resumeToken>", "inputs": { "...": "as init carries them" },
            "current": { "...": "the input set now" } } }
```

`token` is the one from the previous session's `config`. The server reads
that session's record, restores it, and answers with `resumed`. A record is
used once: the server deletes it as it reads it, and writes the new
session's own record after the first flush. If the saved state does not fit
the app once the server function has run, the server closes the socket
without `resumed` and without `allowReconnect: false`. The record is already
consumed, so the client's retry with the same token is answered
`resumed: "inputs"`. Two `resume`
messages with one token (a duplicated tab): the first gets the record, the
second resumes from inputs only. A `dom` key is ignored.

On every `resume`, with or without a token, resume on or off, the server
treats `inputs` as state the previous session already handled: it applies
them before the server function runs, with or without a record, and holds
back the first run of each `observeEvent()` handler. The queued `update`s
that follow `resume` are processed after the first `values` message, as
ordinary input, so each outage-era change runs
its handlers exactly once. An `update` the previous session never processed
(sent after its last processed message but before the browser noticed the
close, or on a reconnect attempt that opened but was not answered) is in
`inputs` and is not replayed. A server implementation
must treat every `update` as an event even when the value is unchanged, as
R's `input` does.

### `resumed` (server → client, once, before the first `values`)

```json
{ "resumed": "snapshot" | "inputs" }
```

`"snapshot"`: the saved state was restored. `"inputs"`: only the inputs
were applied (no token, no record, a mismatch, an incomplete record, the
app's code changed, or a value that could not be decoded; the server log
names the cause). The client fires the DOM event `shiny:resumed` with
`resumed` set to the value, and resets its retry count. When the `resume` it
answers carried a `token` and the value is `"inputs"`, the client then shows a
dismissable warning, "Reconnected, but your previous work could not be
restored." A `shiny:resumed` handler that calls `event.preventDefault()`
suppresses it. With resume off the server sends no `resumed`.

### `unload` (client → server)

Sent on `pagehide` when `event.persisted` is false and the page holds a
token. The page can only come back fresh, so the server deletes the record
and writes no more. Best effort: if the socket is already closed, the
record lives to its 24-hour limit.

### `allowReconnect` after a server-ended session

When the server ends a session itself (`session$close()`, an error in the
server function, an unhandled error in an observer), it sends `allowReconnect: false`, deletes the record and
closes the socket. The client keeps its disconnected overlay and does not
retry. If the page was reconnecting and the socket closes before it received
`resumed` or `values`, it shows "This session could not be resumed." with a Reload link;
otherwise it shows nothing.
