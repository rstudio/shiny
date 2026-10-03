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
