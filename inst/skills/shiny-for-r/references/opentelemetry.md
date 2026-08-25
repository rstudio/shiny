# OpenTelemetry for Shiny for R apps

## Overview

Shiny has built-in [OpenTelemetry](https://opentelemetry.io/) instrumentation:
sessions, reactive flush cycles, and individual reactive/observer/output
executions are automatically wrapped in spans, and value updates and errors
are logged. Do NOT reach for log-scraping or ad hoc timing `print()`
statements to answer "which reactive is slow" or "how long did this session
take" in production — those are exactly the latency questions spans already
answer. This is a production-observability tool, not an interactive
debugging one: for chasing a specific bug during development, an
interactive debugger and reactlog are the better fit.

## Turning it on

By default, if `otel::is_tracing_enabled()` returns `TRUE` — some
OpenTelemetry SDK has configured a tracer provider for the process — Shiny
records spans automatically, with no app code changes. Configuring that
provider is {otelsdk}'s job: see [{otelsdk}'s Collecting Telemetry
Data](https://otelsdk.r-lib.org/reference/collecting.html) guide for
setting up an exporter (console, OTLP, etc.) before launching the app.

## What gets recorded

Spans:

- `session_start` / `session_end` — wrap the server function call and the
  `onSessionEnded()` handlers, respectively.
- `reactive_update` — one span per reactive flush cycle: starts when Shiny
  detects something needs recalculating, ends when there is nothing left to
  calculate (sync or async).
- `reactive <label>`, `observe <label>`, `output <label>` — one span per
  execution of a `reactive()`, `observe()`, or `render*()` output, including
  any async promise chain. The name is the kind plus the object's label, so
  a named reactive reads `reactive myVal` and an anonymous observer reads
  `observe <anonymous>`. `bindCache()`/`bindEvent()` variants add a suffix:
  `reactive cache <label>`, `observe event <label>`.
- Helper-built reactives keep the helper in the *label*, not the kind, so
  they read `reactive debounce <label>`, `reactive throttle <label>`,
  `reactive reactivePoll <label>`, `reactive reactiveFileReader <label>`.
- `ExtendedTask` — wraps an `ExtendedTask`'s calculation, including its async
  chain.

Logs: `reactiveVal`/`reactiveValues` element sets, fatal/unhandled errors
(see the sanitization note below), and `ExtendedTask` status/value/error
updates and queue-add events.

Every span and log carries a `session.id` attribute tying it back to the
session that produced it.

## Collection levels: `shiny.otel.collect` / `SHINY_OTEL_COLLECT`

Control how much of the above Shiny records with the `shiny.otel.collect`
option (falls back to the `SHINY_OTEL_COLLECT` environment variable, default
`"all"`):

| Level | Adds | Use |
|---|---|---|
| `"none"` | nothing | opt out of Shiny's own tracing entirely |
| `"session"` | session start/end spans | minimal overhead in production |
| `"reactive_update"` | + one span per flush cycle | balanced production default |
| `"reactivity"` | + per reactive/observer/output spans | development, diagnosing latency |
| `"all"` | everything (currently = `"reactivity"`) | maximum detail |

```r
# Partial snippet: set before launching the app, or via Sys.setenv(SHINY_OTEL_COLLECT = "session")
options(shiny.otel.collect = "session")
```

## Per-block control: `withOtelCollect()` / `localOtelCollect()`

To override the global level for a specific piece of code that *creates*
reactive expressions — not code that runs them — use `withOtelCollect(collect,
expr)` or, inside a function, `localOtelCollect(collect)` (scoped to the
rest of that function call). Both accept `"none"`, `"reactivity"`, or `"all"`
(not `"session"`/`"reactive_update"`, which are runtime-specific and only
settable via the option or env var).

```r
# Partial snippet: inside a server function
withOtelCollect("none", {
  sensitive <- reactive(authenticate(input$user, input$pass))
})

my_module_result <- withOtelCollect("none", {
  my_module_server("id")
})
# Use my_module_result as normal here — the override only affected creation
```

The setting is captured at *creation*, not each time the reactive runs —
wrapping the body of an already-created `reactive()` in `withOtelCollect()`
has no effect on that reactive's own span.

## Error content is sanitized by default

Fatal/unhandled errors are logged with their message as an attribute. By
default (`shiny.otel.sanitize.errors` defaults to `TRUE`) that message is
sanitized before being sent, to avoid leaking sensitive details to the OTel
backend. Set `options(shiny.otel.sanitize.errors = FALSE)` only if you want
the full error and stack trace recorded instead.

## Quick reference

| Function/option | Purpose |
|---|---|
| `withOtelCollect(collect, expr)` | Temporarily set the collection level while `expr`'s reactives are created |
| `localOtelCollect(collect)` | Same, scoped to the rest of the current function |
| `options(shiny.otel.collect = ...)` | Set the global collection level (`"none"`, `"session"`, `"reactive_update"`, `"reactivity"`, `"all"`) |
| `Sys.setenv(SHINY_OTEL_COLLECT = ...)` | Same, via environment variable |
| `otel::is_tracing_enabled()` | Check whether a tracer provider is configured |

## Common mistakes

- Grepping application logs to answer "why was this session slow" → the
  `reactive_update`/`reactive`/`output` spans already carry timing and
  parent/child relationships; query those instead.
- Expecting `withOtelCollect("none", { ... })` around an *existing* reactive
  object's body to suppress its telemetry → it only affects reactives
  *created* inside the block, not ones merely referenced or invoked there.
- Passing `"session"` or `"reactive_update"` to `withOtelCollect()` → these
  levels are runtime-specific and only valid via `options(shiny.otel.collect
  = ...)` or `SHINY_OTEL_COLLECT`.
- Seeing no spans at all in production → confirm an OpenTelemetry SDK (e.g.
  via {otelsdk}) has actually configured a tracer provider; Shiny only
  records when `otel::is_tracing_enabled()` is `TRUE`.
- Leaving collection at `"all"`/`"reactivity"` in a high-traffic production
  app and being surprised by overhead → drop to `"session"` or
  `"reactive_update"` for routine production use, and dial `"reactivity"`
  back on only while investigating a specific issue.
