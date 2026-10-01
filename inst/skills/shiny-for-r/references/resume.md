# Resuming sessions in Shiny for R

## Overview

Shiny saves each session's state as it goes. When the connection drops and
the page stays open, the reconnected session **resumes** from that saved
state: `reactiveVal()`/`reactiveValues()` keep their values, `input` keeps
its values, observers do **not** run again, and `reactive()` expressions
recompute lazily on their next read. Outputs are not stored or re-run, so
the page keeps them, and a non-reactive value in one (a PID) keeps its
pre-restart value. Inputs changed while disconnected apply on top.

Resume is on by default locally and off under Shiny Server or Connect. Turn
it on or off for an app with `enableResume()` / `disableResume()` at the top
of `app.R` (before `shinyApp()`) or in `global.R`; a platform can set
`SHINY_RESUME=TRUE`. With resume on, the browser retries up to 10 times on
any server; `session$allowReconnect()` is only for apps with resume off.

Nothing is required from app code; how it holds state decides what
restores (the five rules below). `session$resumeReport()` says, node by
node, what will restore and why not. After a resume the R console prints
`Resumed (reconnect): restored N of M reactive nodes, K re-ran`.

## The rules of reactives

**1. Keep state in reactive primitives, not `<<-` variables.** A closure
variable restarts at its initial value while the reactive values it was
paired with come back, a mix worse than either alone. The report's `hazard`
column names observers that assign with `<<-` or `assign()`.

```r
# Partial snippet: inside a server function
n <- 0
observeEvent(input$go, n <<- n + 1)        # WRONG: lost on resume

n <- reactiveVal(0)
observeEvent(input$go, n(n() + 1))          # RIGHT: restored, observer not re-run
```

**2. Create reactives at the top of a scope** (server function, module
body). An observer that creates other reactives has to run again on
resume so they exist, and its other side effects run with it. The usual
offender is `observeEvent(input$add, { insertUI(...); moduleServer(...) })`,
which runs once more after a resume and adds an extra panel. Drive such
UI from a `reactiveVal()` holding the key set. Reactives created inside a
`reactive()` body are never restored.

**3. Do not store reference objects in reactive values.** Connections,
pools, R6 objects and chat clients cannot be serialized; a `reactiveVal()`
holding one is left out (the report names the offending path) and
everything downstream runs again. Hold them in plain variables and rebuild
them in `onResumed()`, or mark the observer that creates them with
`disableResume()` so it runs on resume as on a fresh start:

```r
# Partial snippet: inside a server function
conn <- NULL
connector <- observe({
  conn <<- DBI::dbConnect(RSQLite::SQLite(), "app.db")
})
disableResume(connector)

session$onResumed(function(info) {
  if (info$resumed == "snapshot") message("state restored")
})
```

**4. Give a `label` to reactives created in loops or passed unassigned**,
and to every source in packaged module code. Identity comes from the
assignment name in the source; installed packages keep no source
references, so unlabelled sources in one scope are told apart by position
only. `reactiveValues()` takes `.label` (dotted, because every other named
argument is a value).

```r
# Partial snippet: inside a module server
state <- reactiveValues(page = 1, .label = "state")
totals <- lapply(cols, function(col) reactiveVal(0, label = paste0("total_", col)))
```

**5. Derive with reactives; hold only plain data in sources.** A model fit
assigned into a `reactiveVal()` by an observer is a reference-laden value in
a source: excluded, and it restarts `NULL`. The same fit as an
`eventReactive()` is a derived value: nothing to save, recomputed on first
read.

```r
# Partial snippet: inside a server function
fit <- reactiveVal(NULL)
observeEvent(input$fit, fit(lm(y ~ x, data())))   # WRONG: excluded, restarts NULL

fit <- eventReactive(input$fit, lm(y ~ x, data()))  # RIGHT: recomputes on demand
```

## Reading the report

```r
testServer(server, {
  session$setInputs(go = 1)
  rep <- session$resumeReport()
  stopifnot(all(rep$adoptable[rep$kind != "input"]))
})
```

One row per node: `id`, `kind`, `label`, `namespace`, `adoptable`, `reason`
(`NA` when adoptable), `hazard`. Common reasons: `value is not serializable:
<path>`, `creates other reactives`, `depends on a timer`, `disableResume()`,
`created inside a reactive()`,
`password input; restored from the client`.

## What you can expect after a resume

- `observeEvent()` handlers do not re-fire for earlier clicks; clicks made
  while disconnected run the handler once.
- An expensive `reactive()` recomputes on its first downstream read;
  `bindCache()` with a persistent cache avoids that.
- Timers (`invalidateLater()`, `reactiveTimer()`) and their consumers run
  and re-arm as on a fresh start. A completed `ExtendedTask` result is
  restored; one still running becomes an "interrupted" error, even
  without a server restart.
- `session$userData` entries that serialize are restored.
- Password inputs are never written to disk; the browser re-sends them.

## Known limitation

After a server **process restart**, the page is reloaded rather than
adopted if its UI changed: the fingerprint compares the rendered UI, and
ids that come from Shiny's per-process RNG (`tabsetPanel()`,
`navbarPage()`, bslib navsets) differ on every start. HTML dependencies are
ignored by the fingerprint.

## Quick reference

| Function | Purpose |
|---|---|
| `enableResume()` / `disableResume()` | Turn resume on (with settings) or off for the app |
| `disableResume(x)` | Leave a source, observer, or render function out (pipe render functions only) |
| `session$onResumed(fn)` | Runs `fn(info)` once state is restored, before the first flush |
| `session$resumeReport()` | Data frame: what restores and why not |
| `SHINY_RESUME=TRUE` | Platform switch for hosted apps |
| `reactiveValues(.label = )`, `reactiveVal(label = )` | Explicit identity for sources |

The browser fires `shiny:resumed` (`from`, `resumed`, `dom`).

## Common mistakes

- Building a connection in an observer without `disableResume()`: it stays `NULL`.
- Piping a constructor into `disableResume()` (`n <- reactiveVal(0) |> disableResume()`): the inferred label is lost; call `disableResume(n)` on its own line.
