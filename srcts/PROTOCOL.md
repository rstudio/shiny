# Shiny wire protocol

The client (`srcts/`) and the server (`R/server.R`, `R/shiny.R`) talk over
one WebSocket per session. Messages are JSON text (binary frames carry file
uploads). This document is language-neutral so that other implementations
(py-shiny) can follow it. Message families in outline; the **resume**
family in full.

## Connection

The client opens `ws[s]://<host><app path>websocket/`. As soon as the
socket opens the server sends `config` (below), and the client sends its
first message: `init`, or `resume` when it is reconnecting.

## Client → server

| method | data | when |
|---|---|---|
| `init` | object of all input values, keys may carry a `:type` suffix (`btn:shiny.action`), plus `.clientdata_*` keys | first message of a new session |
| `resume` | see below | first message when reconnecting |
| `update` | object of changed input values, same key rules | whenever inputs change |
| `<other>` | `{ method, args, tag, blobs? }` | RPC (`@uploadInit`, `@uploadEnd`); the server answers with `response` carrying the same `tag` |

## Server → client

`config`, `resumed`, `values`, `errors`, `inputMessages`, `progress`,
`notification`, `modal`, `response`, `javascript`, `console`,
`allowReconnect`, `custom`, `busy`, `recalculating`, `reload`,
`shiny-insert-ui`, `shiny-remove-ui`, `shiny-insert-tab`,
`shiny-remove-tab`, `shiny-change-tab-visibility`, `updateQueryString`,
`resetBrush`, `frozen`. Each message is an object whose top-level keys name
the message types it carries; the client dispatches on those keys.

## The resume family

### `config` (server → client, at socket open)

```json
{ "config": {
    "workerId": "<string>",
    "sessionId": "<string>",
    "user": "<string, optional>",
    "resumeToken": "<32 lowercase hex chars> | null"
} }
```

- `resumeToken` identifies this session's saved state. It is `null` when
  resume is off for the app, and its presence is how the client knows
  resume is on, which makes it retry (see "Client retry rule"). It is
  separate from `sessionId`, which appears in download URLs and logs. The
  client keeps it in memory and presents it once when reconnecting.

### `resume` (client → server, first message when reconnecting)

```json
{ "method": "resume",
  "data": {
    "token": "<resumeToken of the previous session>",
    "dom": "intact",
    "inputs": { "...": "the complete current input set, as init carries it" }
} }
```

- `dom` is `"intact"` when the page stayed open. `"fresh"` is reserved for
  page reload; a server that does not implement it treats the message as
  inputs-only.
- The schema also allows `share` in place of `token` (session sharing, a
  later release). A message with both, or neither, is malformed and is
  answered inputs-only: the inputs are applied and the server replies with
  a `resumed` message whose outcome is `"inputs"`.
- The server validates the token's form before hashing it; the client
  never sends serialized state.
- A server with resume off answers `resume` as `init` with its `inputs`: it
  reads no saved state and sends no `resumed`.

### `resumed` (server → client, once, after the session's state is applied and before the first `values`)

```json
{ "resumed": "snapshot" | "inputs",
  "from": "reconnect",
  "dom": "intact" }
```

The three fields are siblings at the top level of the message.

- `resumed` is `"snapshot"` when the reactive graph was restored from a
  snapshot (even partially), `"inputs"` when only the inputs were replayed.
  `"warm"` is reserved.
- `from` is `"reconnect"`. `"reload"` and `"share"` are reserved for later
  releases.
- Never sent after `init`. The client fires the DOM event `shiny:resumed`
  with the same three fields.

### `allowReconnect` (server → client)

```json
{ "allowReconnect": true | false | "force" }
```

Sets whether the client retries after the socket closes. With resume off,
it carries the app's `session$allowReconnect()` calls, as it always has.
With resume on, the server sends it only as `false`, when it ends the
session for good (`session$close()`, a fatal error, or an error in the
server function); the session's saved state is deleted at the same time.

### Client retry rule

After the socket closes for good, the client retries with increasing
delays (1.5 s, 1.5 s, 2.5 s, 2.5 s, 5.5 s, 5.5 s, then 10.5 s) when it
holds a `resumeToken` and has not since received `allowReconnect: false`,
or, as on main, when the last `allowReconnect` value it saw is `"force"`,
or `true` and the socket is shiny-server-client's (which marks it
`allowReconnect: true`). A client holding a token makes at most ten
attempts (about a minute); then it stops and leaves the disconnected
overlay. Without a token, attempts are unbounded and each retry sends
`init`. With a token, each retry opens a new socket and sends `resume` with
the last `resumeToken` and the current inputs. The new session's `config` supplies a
new token. The attempt count resets when the server answers (on
`resumed`, or on the first `config` or `values` of a fresh `init`), not
when the socket opens, so a server that refuses or drops the socket before
answering still exhausts the attempts. The server sends `resumed` before
the server function runs, and `config` as the session is created, so the
count does not guard against a process that dies after that (a crash or
out-of-memory exit under a supervisor that restarts it): such a client
retries indefinitely. R errors are covered on the server side instead: it
sends `allowReconnect: false` before it ends a session itself
(`session$close()`, a fatal error, or an error in the server function).

### `reload` on resume (server → client)

When a `resume` arrives for a page whose UI no longer matches the app's
current UI (the snapshot records a fingerprint of the page), the server
sends the existing `reload` message instead of adopting, then answers the
`resume` as inputs-only so the session stays valid until the page reloads.
