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
| `resume` | `{ "inputs": <the object init would carry> }` | first message when reconnecting |
| `update` | object of changed input values, same key rules | whenever inputs change |
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
  "data": { "inputs": { "...": "the complete current input set, as init carries it" } } }
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

### `allowReconnect` (server → client)

```json
{ "allowReconnect": true | false | "force" }
```

Carries the app's `session$allowReconnect()` calls. `"force"` behaves as
`true`; it is kept for compatibility.

### Client retry rule

After the socket closes, the client retries when the last `allowReconnect`
value it saw is `true` or `"force"`, on any socket. Delays between attempts
are 1.5 s, 1.5 s, 2.5 s, 2.5 s, 5.5 s, 5.5 s, then 10.5 s repeated; at most
**ten** attempts, about a minute, after which the client stops, removes the
"Attempting to reconnect" notification and leaves the disconnected overlay.
Each retry opens a new socket and sends `resume` with the current inputs.
The attempt count resets when the server answers (`config` or `values`),
not when the socket opens, so a server that accepts the socket and drops it
before answering still exhausts the attempts.

### `config` additions (server → client, at socket open)

```json
{ "config": { "workerId": "...", "sessionId": "...", "user": "...",
              "resumeToken": "<32 lowercase hex chars> | null",
              "resumeReload": "ask" | "resume" | "fresh" } }
```

`resumeToken` identifies this session's saved state; `null` with resume off.
Its presence tells the client the app allows reconnecting. `resumeReload`
is the app's `enableResume(reload =)`; the client stashes both in
`sessionStorage` so the next page can decide before it connects.

### `resume` with a token

```json
{ "method": "resume",
  "data": { "token": "<resumeToken>", "dom": "intact" | "fresh",
            "inputs": { "...": "as init carries them" } } }
```

`dom` is `"intact"` on a reconnect and `"fresh"` on the first socket of a
reloaded page. A `resume` with a token on a server with resume off is
answered as `init` with its inputs (the rule above). With resume on the
server reads the record, applies it under the all-or-nothing rule, and
answers with `resumed`.

### `resumed` (server → client, once, before the first `values`)

```json
{ "resumed": "snapshot" | "inputs", "inputs": { "<id>": value } }
```

`"snapshot"`: the saved state was restored. `"inputs"`: only the inputs were
applied (no record, a mismatch, an incomplete record, or a value that could
not restore; the server log names the cause). `inputs` is present on a
fresh-page resume whenever a record supplied inputs, under either outcome,
and maps each bound input whose reported value differs from the record's to
the record's value, file inputs and `clientData` excluded. The client
applies each through its binding (`setValue` if defined, else
`receiveMessage({ value })`), teaches its no-resend filter the pushed
values, re-reads every bound input and sends the ones that still differ as
ordinary `update`s. The client fires the DOM event `shiny:resumed` with
`resumed`.

### The stash and the token

The client keeps one `sessionStorage` entry per app path with the token it
would resume. `config` normally rewrites it with the new session's token.
After a socket that opened with `resume`, though, the client waits for
`resumed` before it does, and a `reload` that arrives first pins the stash
to the token being resumed. The UI-fingerprint check relies on this: the
server answers that `resume` with a session of its own (and its own token)
and a `reload`, and the reloaded page has to resume the record the server
left, not the session that asked for the reload.

### `unload` (client → server)

`{ "method": "unload", "args": [], "tag": <n> }`, sent on `pagehide`. The
record then gets a short lifetime (minutes) after the closing write instead
of the full TTL.

### `discardSnapshot` (client → server request)

Deletes this session's record and stops further writes ("Start fresh").

### `fatalError` (server → client)

```json
{ "fatalError": { "message": "<text, absent when sanitized>", "saved": true | false } }
```

Sent when an unhandled error in an observer ends the session, before
`allowReconnect: false` and the close. `saved` says whether a record from
before the error exists. The client shows a dialog with **Resume** (when
`saved`) and **Start over**, does not retry, and keeps its stash through the
close so Resume can reload into the saved state.

### `reload` (server → client)

`true`: reload, and the next page resumes without asking (the client marks
its stash). `"fresh"`: drop the stash and reload (`session$reload()`).
