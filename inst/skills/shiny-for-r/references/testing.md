# Testing Shiny for R apps

## Overview

`testServer()` runs a server function (or module) inside a mocked reactive
session, without starting a real Shiny process, opening a browser, or
touching HTTP at all. Do NOT spin up a real browser (or `shinytest2`) to
verify server-side logic — setting an input, checking a reactive, and
asserting an output take milliseconds with `testServer()` versus seconds per
round trip with a browser driver. Reserve browser automation for what only
the browser can prove: rendered HTML, JS widgets, CSS, and click-through
flows.

## Test a server function: `testServer()`

`testServer(app, expr, args = list())` takes a server function (with
`input`, `output`, `session` parameters), runs `expr` inside the server's
environment, and gives `expr` direct access to everything it created —
reactives, `input`, `output`, `session` — via a data mask. Drive inputs
with `session$setInputs()`; read outputs as `output$name`; read a
`reactive()` by calling it like a function.

```r
library(shiny)
library(testthat)

server <- function(input, output, session) {
  doubled <- reactive(input$n * 2)
  output$txt <- renderText(paste("doubled:", doubled()))
}

test_that("doubled reacts to n", {
  testServer(server, {
    session$setInputs(n = 5)
    expect_equal(doubled(), 10)
    expect_equal(output$txt, "doubled: 10")

    session$setInputs(n = 10)
    expect_equal(doubled(), 20)
  })
})
```

Each `session$setInputs()` call flushes the reactive graph before returning,
so `doubled()` and `output$txt` are already up to date — no manual flush is
needed after setting inputs. If you invalidate something another way (e.g.
by mutating a `reactiveValues()` object directly, or via a timer), call
`session$flushReact()` to force a synchronous flush before asserting.

## Test a module

Pass the module's server function (the one with a leading `id` argument that
calls `moduleServer()`) directly to `testServer()`. Any extra arguments the
module needs go in `args`; if the module doesn't require a specific `id`,
`testServer()` generates one automatically.

```r
library(shiny)
library(testthat)

counterServer <- function(id, step = 1) {
  moduleServer(id, function(input, output, session) {
    count <- reactiveVal(0)
    observeEvent(input$bump, count(count() + step))
    count
  })
}

test_that("counterServer increments by step", {
  testServer(counterServer, args = list(step = 5), {
    session$setInputs(bump = 1)
    expect_equal(count(), 5)

    session$setInputs(bump = 2)
    expect_equal(count(), 10)
  })
})
```

`input$bump` here is an `actionButton`-style counter: `session$setInputs()`
just needs to change its value to trigger the `observeEvent()`. The
module's return value (`count`) is available in `expr` like any other
object the server function created.

## Expose internals for whole-app testing: `exportTestValues()`

`testServer()` covers server logic in isolation. When you need a running app
(for a full `shinytest2` snapshot test, or manual inspection) to reveal an
internal reactive that has no output of its own, register it with
`exportTestValues()` instead of rendering it into a hidden output. This only
has an effect when the app runs in test mode (`options(shiny.testmode =
TRUE)`, or `runApp(..., test.mode = TRUE)`).

```r
# Partial snippet: inside a server function, app launched with
# options(shiny.testmode = TRUE)
vals <- reactiveValues(x = 1)
y <- reactive(vals$x + 1)

exportTestValues(
  x = vals$x,
  y = y()
)
```

With test mode on, `session$getTestSnapshotUrl()` returns a URL that serves
the current `input`, `output`, and exported values as JSON — useful for
confirming, from outside the R process, exactly what the server holds.

## Full-browser snapshot tests

For end-to-end coverage — verifying rendered HTML, JS widget behavior, or a
multi-page click-through flow — reach for {shinytest2}. Its
`shinytest2::record_test()` opens your app in a browser and records your
interactions as a runnable test script; it complements `testServer()` rather
than replacing it.

## Quick reference

| Function | Purpose |
|---|---|
| `testServer(app, expr, args)` | Run a server/module function in a mocked session |
| `session$setInputs(...)` | Set one or more inputs and flush the reactive graph |
| `session$flushReact()` | Force a reactive flush without changing an input |
| `exportTestValues(...)` | Register internal values for the test-mode snapshot |
| `session$getTestSnapshotUrl()` | URL serving the current input/output/export snapshot as JSON |
| `shinytest2::record_test()` | Record a full-browser snapshot test |

## Common mistakes

- Launching a real app and clicking through it by hand to check server logic
  → write a `testServer()` test instead; it runs in milliseconds and needs no
  browser.
- Asserting on a reactive right after mutating a `reactiveValues()` field
  directly (not through `session$setInputs()`) and getting a stale result →
  call `session$flushReact()` first.
- Rendering a reactive into a hidden `textOutput()` just so a test can see it
  → register it with `exportTestValues()` instead; it only activates in test
  mode.
- Calling `session$getTestSnapshotUrl()` when the app was not launched with
  `shiny.testmode = TRUE` → the snapshot endpoint isn't available; set the
  option (or `test.mode = TRUE` in `runApp()`) first.
- Passing a module's inner function (the one taking `input`, `output`,
  `session`) to `testServer()` instead of the outer function that takes `id`
  → `testServer()` expects the function you'd call from a UI, not the one
  passed to `moduleServer()`.
