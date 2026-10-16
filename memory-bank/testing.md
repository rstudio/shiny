---
title: "Testing"
description: "How shiny's own R, browser, and TypeScript test suites are laid out, run locally, and run in CI."
---

# Testing

## Layout

```
tests/
  testthat.R            # library(testthat); library(shiny); test_check("shiny")
  testthat/             # the real suite (testthat 3e: Config/testthat/edition: 3)
    helper*.R           # sourced automatically before tests
    test-*.R / test-*.r # a few files use lowercase .r (reactivity, bootstrap, gc)
    _snaps/             # expect_snapshot() output, one .md per test file
  test-helpers/         # fixture apps (app1-standard ... app7-port) used by test-app.R / test-test-runTests.R
  test-modules/         # fixture apps with their own tests/, used by test-test-server-app.R
  test-encoding/        # manual locale/encoding script, NOT run by testthat
```

Only `tests/testthat/` is collected by `test_check()`. The sibling directories
are fixtures reached via `test_path("..", "test-modules", ...)` or
`test_path("../test-helpers/app1-standard")`.

## Running tests

```r
devtools::test()                                   # whole suite (load_all + NOT_CRAN=true)
devtools::test(filter = "timer")                   # files matching test-timer*
testthat::test_file(testthat::test_path("test-otel-error.R"))  # one file, after load_all()
```

`devtools::test(filter = "timer")` was verified to run (`PASS 23`, about 2s).
Tests call shiny internals directly (`flushReact()`, `timerCallbacks`,
`execCount()`, `MockShinySession`), so run them under `devtools::load_all()`
semantics. Plain `library(shiny)` won't expose those internals.

## Helpers (`tests/testthat/helper*.R`)

| File | Provides |
|---|---|
| `helper.R` | `contents_identical()` (order-insensitive compare), `suppress_stacktrace()` (captures stderr), `rewire(f, ...)` (swap bindings a function sees), `rewire_namespace_handler()` (for `pkg::fn` calls) |
| `helper-shinytest2.R` | `skip_if_not_shinytest2()`, `launch_test_app(app_fn, ...)` (an `AppDriver` with `withr::defer(app$stop())`, waits for idle) |
| `helper-stacks.R` | test-only copy of the removed `extractStackTrace()`, plus `cleanLocs()`, `dumpTests()`, `captureFilteredRenderTrace()` |
| `helper-otel.R` | `skip_if_shiny_otel_tracer_is_enabled()`, `create_mock_otel_span()`, `create_mock_tracer()`, `create_mock_logger()` |
| `helper-otel-interactive-app.R` | `dev_otel_kitchen()`, a manual demo app (`devtools::load_all(); dev_otel_kitchen()`). Not a test |

`rewire()` is the old-style stubbing approach, used in `test-test-runTests.R`
(`rewire(runTests, sourceUTF8 = ..., loadSupport = ...)`). Newer tests use
`testthat::local_mocked_bindings()` (about 29 calls, e.g. `test-non-blocking.R`,
`test-otel-reactive-update.R`). Options and env vars are set with
`withr::local_options()` / `withr::local_envvar()`.

## Snapshots

`_snaps/` holds `actionButton.md`, `busy-indication.md`, `downloadButton.md`,
`reactivity.md`, `stacks.md`, `stacks-deep.md`, `tabPanel.md`. There are no
platform variants. Update them with `testthat::snapshot_accept("stacks")` after
reviewing `snapshot_review()`. CI uploads failed snapshots as an artifact
(`upload-snapshots: true` in the shared check workflow).

## Skip conditions in use

- `skip_on_cran()`: file-level in `test-timer.R`, `test-otel-mock.R`, `test-otel-error.R`. Per test in `test-stacks.R`, `test-pkgdown.R`, `test-utils.R`, `test-reactivity.r`, `test-test-server.R`.
- `skip_if_not_installed("pkg")`: for Suggests like `otelsdk`, `ggplot2`, `shinyjs`, `rprojroot`, `shinytest2`.
- `skip_on_os("windows")`: in `test-shinywrappers.R`, `test-busy-indication.R`, `test-utils.R` (UTF-8).
- `skip_if(!isTRUE(as.logical(Sys.getenv("CI"))), ...)`: in `test-devmode.R` (only runs in CI).
- `skip_if_shiny_otel_tracer_is_enabled()`: stack-trace tests, which differ when otel is active.
- `skip_if_not_shinytest2()`: skip on CRAN, skip on Windows/Linux/Solaris/emscripten, skip if shinytest2 is missing. **Browser tests run only on macOS.**

## In-process server tests vs browser tests

**In-process (the default choice).** `testServer(app, expr, args, session = MockShinySession$new())`
(`R/test-server.R`) runs a server function, module, or app directory without
a browser. Inside `expr` you have `input`, `output`, `session` and the
server's local variables. `MockShinySession` methods used in tests (all
defined in `R/mock-session.R`):

- `session$setInputs(x = 1)`: set inputs and flush
- `session$elapse(millis)`: advance the fake clock (for `reactiveTimer`, `invalidateLater`, `debounce`, `throttle`)
- `session$flushReact()`: flush only
- `session$getReturned()`: a module's return value
- `session$setEnv()`, `session$getOutput()`, `session$destroy()`

Some tests build `MockShinySession$new()` directly and pass it to
`withReactiveDomain()` or `testServer(session = )`. Examples:
`test-test-server.R`, `test-mock-session.R`, `test-destroy.R`, `test-bind-cache.R`.
Use this approach for reactive logic, module wiring, outputs, and session lifecycle.

**Browser (shinytest2).** Files named `test-zzz-st2-*.R` (the `zzz` makes them sort last). Pattern:

```r
skip_if_not_shinytest2()
library(shinytest2)
app <- launch_test_app(function() {
  library(shiny)   # required: shinytest2's shim then load_all()s the dev package
  shinyApp(ui, server)
})
test_that("...", { expect_equal(app$get_value(input = "x"), ...) })
```

Use shinytest2 only when the behavior lives in JavaScript, such as
bootstrap-datepicker, the enabled/disabled state of download buttons, or the
client DOM. Examples: `test-zzz-st2-input-date.R` and
`test-zzz-st2-download.R` (the latter also needs `shinyjs`, which
`Config/Needs/check: shinyjs` installs in CI).

## Reactivity idioms (verified in tests)

- `flushReact()` (internal, `R/react.R`) runs pending invalidations and returns TRUE if any work ran. It's used about 245 times across 13 files. It's the core of `test-reactivity.r`.
- `isolate({ ... })` reads reactives outside an observer.
- `execCount(observer)` (internal, `R/reactives.R`) asserts how many times an observer ran.
- `withReactiveDomain(domain, expr)` scopes a fake domain (an `env` or a `MockShinySession`).
- `reactiveConsole(TRUE)` with `on.exit(reactiveConsole(FALSE))` allows top-level reactive reads (`test-reactives.R`).
- `later::run_now()` and loops on `later::loop_empty()` drain promises and later callbacks (about 59 calls).
- `timerCallbacks$schedule()` / `$executeElapsed()` test the timer queue directly (`test-timer.R`).

## Fixture app suites

- `tests/test-helpers/`: apps with `global.R`, `R/` and `tests/` dirs. They exercise `loadSupport()` (autoloading `R/`, the `_disable_autoload.R` opt-out, nested dirs, a bad `global.R`) and `runTests()` (runner files, `filter =`, missing or empty `tests/`).
- `tests/test-modules/`: `06_tabsets`, `server_r`, `12_counter`, `107_scatterplot`. `test-test-server-app.R` calls `testServer()` on the directories and `runTests()` on `12_counter` and `107_scatterplot`. Each of those two has its own `tests/testthat.R`, which calls `testthat::test_dir("./testthat", env = shiny::loadSupport())`.
- `tests/test-encoding/test-all.R`: an interactive script. Run it from the package root. It launches each `NN-*` app with `runApp()` under several locales (Chinese and English on Windows) and also expects `../shiny-examples/022-unicode-chinese/`. Never automated.

## `inst/app_template` and `runTests()`

`shinyAppTemplate(path, examples)` copies `inst/app_template` (`app.R`, `R/`,
`tests/testthat/` with `setup-shinytest2.R`, `test-shinytest2.R`,
`test-server.R`, `test-examplemodule.R`, `test-sort.R`). `runTests(appDir)`
(`R/test.R`) sources each top-level file in `appDir/tests/` and returns a
result whose `$pass` column you can assert on. The template round-trip test
is `test-test-shinyAppTemplate.R`.

## TypeScript tests

```sh
npm run test_types   # tsx --test $(find srcts/src -path '*/__tests__/*.test.ts')
```

This uses Node's built-in `node:test` and `node:assert/strict`. Current files:
`srcts/src/shiny/__tests__/{sendOutputInfo,outputProgress}.test.ts` and
`srcts/src/time/__tests__/debounce.test.ts`. `__tests__` is excluded from
`tsconfig.json`, so `build_types` doesn't type-check the tests. `test_types`
is part of `npm run checks`, which `npm run build` runs. There is no `npm test`
script.

## OpenTelemetry tests

- `with_shiny_otel_record(expr)` (`R/otel-shiny.R`, internal) wraps `otelsdk::with_otel_record()`. It re-runs `init_otel()` so shiny picks up the recording tracer, and resets on exit. It refuses to run unless `TESTTHAT == "true"`. It returns `$traces`.
- The usual pattern (`test-otel-mock.R`, `test-otel-error.R`): set `withr::with_options(list(shiny.otel.collect = "all"|"reactivity"))` around `with_shiny_otel_record(testServer(server, {{ expr }}, session = session))`, then inspect `traces` by span name.
- Pure-mock tests (`test-otel-shiny.R`, `test-otel-reactive-update.R`) avoid otelsdk by using `create_mock_tracer()` / `create_mock_otel_span()` together with `local_mocked_bindings()`.

## CI (`.github/workflows/R-CMD-check.yaml`)

Triggers: pushes to `main` and `rc-**`, every PR, and a cron job on Mondays.
It calls three reusable workflows from `rstudio/shiny-workflows@v1`:

- **R-CMD-check**: `rcmdcheck` with `--no-manual --as-cran`, `error-on: "warning"`. Matrix: macOS release; Windows release; Ubuntu 24.04 devel, release, and oldrel-1 to oldrel-4; plus a `check-depends-only` job (`_R_CHECK_DEPENDS_ONLY_=true`, so Suggests are absent and `skip_if_not_installed()` must guard every Suggests dependency). Windows oldrels and devel run only for packages with a `src/` dir, which shiny doesn't have. A failing check prints `testthat.Rout`.
- **routine**: runs on R release and **can auto-commit to non-fork PR branches**. In order, it runs `usethis::use_tidy_description()`, then `devtools::document()` (DESCRIPTION, NAMESPACE, `man/`), then the README build (skipped because shiny has no README.Rmd), then oxipng on `man/figures/*.png`, then `npm ci` and `npm run build`. The build runs `checks` (lint with `--fix`, `build_types`, `test_types`, coverage, circular) and commits `inst/` and `srcts/` changes. Last, it runs the local `.github/shiny-workflows/routine.sh`, which runs `tools/documentation/checkDocsCurrent.sh`. That script **fails** if the roxygen docs or the re-exported htmltools docs are stale. It then syncs the `package.json` version and rebuilds. R code formatting (air) is off by default. On `push` events, and on PRs from forks, any new commit fails the job, so you must commit those changes yourself. There's no `.lintr` or `codecov.yml`, so the lint and coverage steps are skipped. On `rc-v*` branches it also runs `urlchecker::url_update()` and syncs the version.
- **website**: builds the pkgdown site. It only verifies on PRs and the cron job, and deploys to `gh-pages` on push.

## Pitfalls

- `test-test-shinyAppTemplate.R` starts with an unconditional `testthat::skip("Refactor test for next release")`, so the app template and `runTests()` round-trip are currently untested.
- shinytest2 tests never run on Linux or Windows CI, only on the macOS job. A JS regression that only shows up on other platforms won't be caught.
- Inside `launch_test_app()`, omitting `library(shiny)` makes the app run the *installed* shiny, not your working copy.
- `test-otel-error.R` calls `skip_if(length(traces) > 3, ...)`. Spans leak between files when the whole suite runs, so these assertions may silently skip. To see them for real, run `testthat::test_file(test_path("test-otel-error.R"))`.
- `test-otel-reactive-update.R` defines its own `create_mock_otel_span(name, attributes, ended)` that shadows the one in `helper-otel.R`, which has a different signature.
- `with_shiny_otel_record()` errors with a `stopifnot` failure outside testthat, because it checks `Sys.getenv("TESTTHAT")`.
- `test-built-files.R` checks that the installed `www/shared/shiny.js` has no `{{ VERSION }}` placeholder. After editing `srcts/`, run `npm run build`. Routine will otherwise commit the built JS for you on a PR, or fail on a push.
- `test-pkgdown.R` only checks something when `tools/documentation/checkPkgdown.R` is reachable (a source checkout). Under `R CMD check` it skips. A new export missing from `tools/documentation/pkgdown.yml` is caught by routine's `checkDocsCurrent.sh`, not by `R CMD check`.
- `test-devmode.R`'s message test runs only when `CI=true`, so it's skipped locally.
- `devtools::test()` sets `NOT_CRAN=true`, so `skip_on_cran()` tests do run locally. Under `R CMD check --as-cran` without `NOT_CRAN`, those files are skipped entirely (for example all of `test-timer.R`).
