---
title: "Architecture"
description: "How shiny (R) works internally: reactive graph and flush cycle, ShinySession and the websocket protocol, module scopes and destroy/close teardown, UI and dependency rendering, input/output bindings, async, testServer, OpenTelemetry, and the JS bundle. Read the relevant section before changing shiny's internals."
---

# Architecture

Internals map for contributors. Each section names the files and classes to
open first. R code is in `R/`, the TypeScript client in `srcts/src/`, and the
built client assets are committed in `inst/www/shared/`.

## Reactive System

Implementation: `R/react.R` (`Context`, `ReactiveEnvironment`, `flushReact()`),
`R/reactives.R` (primitives), `R/reactive-domains.R` (domains),
`R/graph.R` (reactlog), `R/priorityqueue.R`, `R/timer.R`.

- **`Context`** (`R/react.R`) is one execution of a consumer. A consumer
  re-run creates a fresh Context. `ctx$run(func)` installs the reactive
  domain, the otel span context, and the reactlog enter/exit hooks, then runs
  via `ReactiveEnvironment$runWith()`. `invalidate()` fires `onInvalidate`
  callbacks once. `addPendingFlush(priority)` enqueues the context, and
  `onFlush` callbacks run when it is dequeued.
- **`ReactiveEnvironment`** is a process-wide singleton
  (`.getReactiveEnvironment()`). It holds the current context and a
  `PriorityQueue` of pending flushes. `flush()` drains the queue and guards
  against re-entrancy with `.inFlush`. `flushReact()` is the entry point.
- **`Dependents`** (`R/reactives.R`) is the source side of an edge. `register()`
  records `getCurrentContext()` and removes it again on invalidation.
  `invalidate()` invalidates every dependent context. Weak contexts
  (`Context$new(weak = TRUE)`, used by `Observable`) are stored as
  `rlang::new_weakref()`.
- **Sources:** `ReactiveVal` (`reactiveVal()`), and `ReactiveValues` (wrapped
  by `reactiveValues()` / `.createReactiveValues()` into an S3
  `reactivevalues` object with optional `ns` and `readonly`). `ReactiveValues`
  keeps per-key deps plus separate deps for `names()`, `reactiveValuesToList()`
  and `all.names = TRUE`. It also supports `freeze()`/`thaw()`, `getMeta()` /
  `setMeta()` and `destroyByPrefix()`.
- **`Observable`** backs `reactive()`. It is a lazy, cached value.
  `getValue()` registers the caller, then recomputes in `.updateValue()` if it
  is invalidated. Errors are cached too: `.error`/`.value` are re-thrown until
  the next invalidation.
- **`Observer`** backs `observe()`. On invalidation it enqueues itself with
  `.priority` and calls `.domain$incrementBusyCount()`. Its flush callback runs
  `run()` inside `hybrid_chain(..., finally = .domain$decrementBusyCount)`.
  Other members: `suspend()`/`resume()` (a suspended observer stores the
  pending flush in `.onResume`), `setAutoDestroy()` (hooks `onEnded` and
  `onDestroy`), and `destroy()`.
- `isolate()`, `observeEvent()`, `eventReactive()`, `debounce()`,
  `throttle()`, `reactivePoll()`, `invalidateLater()` and `reactiveTimer()` all
  live in `R/reactives.R`.
- **Domains:** `getDefaultReactiveDomain()` reads `.globals$domain`.
  `withReactiveDomain()` is a *promise domain* (`createVarPromiseDomain()`,
  `R/utils.R`), so the domain follows promise callbacks. Consumers capture
  their domain when they are created.
- **Reactlog:** the global `rLog` (an `RLog` R6 object in `R/graph.R`) is
  called from every primitive (`define`, `dependsOn`, `invalidateStart`,
  `valueChange`, `asyncStart`, ...). Entries are stored only when
  `options(shiny.reactlog = TRUE)`, but they are always forwarded to
  `domain$reactlog()`. The HTTP side is `reactLogHandler` in
  `R/middleware-shiny.R`.

## Server, Sessions and the Wire Protocol

Implementation: `R/server.R`, `R/shiny.R` (`ShinySession`), `R/runapp.R`,
`R/app-handle.R`, `R/shinyapp.R`, `R/middleware*.R`,
`R/server-input-handlers.R`.

- **App objects:** `shinyApp()`/`shinyAppDir()` (`R/shinyapp.R`) build a
  `shiny.appobj` with `httpHandler` (`uiHttpHandler()` in `R/shinyui.R`) and
  `serverFuncSource`.
- **Startup and loop:** `runApp()` blocks in `while (!.globals$stopped)
  serviceApp()`. `startApp()` is non-blocking: it returns a `ShinyAppHandle`
  and drives `serviceNonBlocking()` with `later`. `serviceApp()` runs
  `timerCallbacks$executeElapsed()`, `flushReact()`, `flushPendingSessions()`,
  then httpuv `service()`, then flushes again.
- **Handlers:** `createAppHandlers()` returns `http` (`joinHandlers()` over
  `sessionHandler`, the app handler, `www/`, resource paths and reactlog) and
  `ws`. Each websocket gets a `ShinySession$new(ws)`, which is registered in
  `appsByToken`. `ws$onClose` calls `shinysession$wsClosed()`.
- **Client to server:** `decodeMessage()` parses JSON. A binary frame starts
  with the `0x01020202` signature, followed by length-prefixed parts: JSON
  first, then blobs. `applyInputHandlers()` turns `"id:type"` names into
  values using `registerInputHandler()` types (`shiny.date`, `shiny.file`,
  ...). The methods are:
  - `init`: run the server function inside `otel_span_session_start()`.
  - `update`: `manageInputs()`.
  - anything else: `ShinySession$dispatch()`, which calls `self[["@<method>"]]`
    and sends a `response`.
- **Inputs are cycle-gated:** `manageInputs()` goes through
  `cycleStartAction()`. It applies immediately only when `busyCount == 0`.
  Otherwise `decrementBusyCount()` reaching 0 schedules `startCycle()` through
  `later`. `busy`/`idle` messages are sent on 0 transitions of `busyCount`.
- **Output flush:** `output$x <- render...` is `$<-.shinyoutput`, which calls
  `defineOutput()`. That wraps the render function in an `observe()` that
  sends `recalculating`, runs the function through `hybrid_chain`, and stores
  the result in `invalidatedOutputValues` or `invalidatedOutputErrors`.
  `requestFlush()` adds the session to `appsNeedingFlush`. `flushOutput()`
  (skipped while `busyCount > 0` or when closed) invokes `onFlush` callbacks
  and sends one message with `errors`, `values` and `inputMessages`, then
  runs `onFlushed`. Server-to-client messages are JSON objects keyed by
  handler name (`private$sendMessage(...)`). `sendBinaryMessage()` uses a
  1-byte type-length prefix.
- **Hidden outputs:** `shouldSuspend()` reads clientData
  `output_<id>_hidden`. `manageHiddenOutputs()` suspends or resumes output
  observers after every input batch.

## Module Scopes, destroy() and close()

Implementation: `R/modules.R`, `ShinySession$makeScope()` /
`private$invokeDestroyCallbacks()` in `R/shiny.R`, `R/reactives.R`
(`make_weak_destroy_wrapper()`, `isClosedDomain()`, `destroyedReactiveError()`).
Tests: `tests/testthat/test-destroy.R`.

- `NS()` joins ids with `ns.sep` (`"-"`). `moduleServer()` calls
  `callModule()`, which runs the module inside
  `withReactiveDomain(session$makeScope(id), ...)`.
- `makeScope()` returns a `session_proxy` built by `createSessionProxy()`.
  `$.session_proxy` returns overrides (namespaced `input`/`output`, `ns`,
  `makeScope`, bookmark hooks, `onDestroy`, `destroy`, ...) and forwards
  everything else to the parent. `$<-` is rejected, except for `options` and
  same-value writes, so `session$userData$x <- 1` still works.
  `find_ancestor_session()` walks back to the real `ShinySession`.
- **onDestroy/destroy:** callbacks are stored per namespace in
  `private$destroyCallbacksByNs`. The root uses the sentinel key
  `destroyNsRoot` (`"..root"`). `session$destroy(ns)` (validated by
  `validateNamespace()`) runs callbacks for `ns` and `ns-*`, deepest first.
  It then calls `destroyByPrefix()` on input and clientData (including
  `output_<ns>-`), destroys matching outputs, and removes download and file
  routes. The `ReactiveVal`, `ReactiveValues`, `Observable` and auto-destroy
  `Observer` objects register `onDestroy(make_weak_destroy_wrapper(...))`, so
  a destroyed reactive raises a `shiny.destroyed.error` when read.
- **Soft teardown on close (#4421):** `wsClosed()` sets `closed <- TRUE`,
  suspends outputs, runs `onSessionEnded` callbacks (observers auto-destroy
  here), then calls `invokeDestroyCallbacks(allowRoot = TRUE)`.
  `make_weak_destroy_wrapper()` returns early when `isClosedDomain(domain)`,
  so on close, reactive values and expressions stay **readable** at their last
  value. This keeps async work that outlives the socket (such as
  `ExtendedTask`) from failing. Only an explicit `destroy(ns)` poisons
  reactives.

## UI / HTML Generation

Implementation: `R/shinyui.R` (`renderPage()`, `shinyDependencies()`,
`uiHttpHandler()`), `R/bootstrap.R`, `R/bootstrap-layout.R`, `R/input-*.R`,
`R/html-deps.R`, `R/shinywrappers.R` (`renderUI()`), `R/insert-ui.R`,
`R/insert-tab.R`, `inst/template/default.html`.

- Tags come from htmltools (`R/reexports.R`). `uiHttpHandler()` evaluates `ui`
  (or `ui(req)`) inside a `RestoreContext`. `renderPage()` wraps the result in
  `default.html` and calls `htmltools::renderDocument()` with
  `processDep = createWebDependency`, which maps dependencies to URLs served by
  `resourcePathHandler`.
- `shinyDependencies()` is `bslib::bs_dependency_defer(shinyDependencyCSS)`
  plus the busy indicators plus `shiny.min.js`. Set the devmode option
  `shiny.minified = FALSE` to get `shiny.js`.
- `bootstrapLib(theme)` is a `tagFunction`. While the app is running it calls
  `setCurrentTheme()` (`shinyOptions(bootstrapTheme=)`) and
  `registerThemeDependency()`, then returns either `bslib::bs_theme_dependencies()`
  or the static BS3 `bootstrapDependency()`. Themeable inputs resolve their
  CSS at render time through `getCurrentTheme()`. `session$setCurrentTheme()`
  re-sends those dependencies.
- `renderUI()` calls `processDeps()`, which tagifies, strips singletons already
  on the page (`session$singletons`), and returns `list(html, deps)`. The
  client renders the dependencies before the HTML (`renderContentAsync()` in
  `srcts/src/shiny/render.ts`).

## Input / Output Bindings

R side, in `R/shinywrappers.R`:

- `createRenderFunction(func, transform, outputFunc)` wraps `func` in
  `hybrid_chain(func(), transform(value, session, name))` and calls
  `markRenderFunction()`.
- `markRenderFunction()` returns a `shiny.render.function` carrying the
  `outputFunc`, `outputArgs`, `cacheHint` (derived from the `wrappedFunc`
  attribute set by `installExprFunction()` in `R/utils-lang.R`), cache hooks
  and `otelAttrs`.
- Custom renderers must keep that `wrappedFunc` chain, or `bindCache()`
  cannot build a cache hint.
- Server-to-client updates (`updateXxxInput()`, `R/update-input.R`) use
  `session$sendInputMessage()`. That queues into `inputMessages` for the next
  flush.

JS side, in `srcts/src/bindings/`:

- `InputBinding` and `OutputBinding` base classes, plus one file per binding.
  They are registered in `initInputBindings()` and `initOutputBindings()` into
  a `BindingRegistry` (`registry.ts`).
- `srcts/src/shiny/bind.ts` (`bindAll()`/`unbindAll()`) finds DOM elements,
  applies input rate policies (`srcts/src/inputPolicies/`), and appends the
  `:type` suffix from `getType()`.
- `srcts/src/shiny/shinyapp.ts` (`ShinyApp`) owns the socket. It provides
  `sendInput()` (`method: "update"`), `makeRequest()` (binary blobs) and
  `dispatchMessage()`. `dispatchMessage()` triggers `shiny:message`, updates
  `OutputProgressReporter`, and runs handlers in `messageHandlerOrder`
  (registration order in `_init()`).
- `ShinyClass` (`srcts/src/shiny/index.ts`) becomes `window.Shiny` in
  `srcts/src/initialize/index.ts`.

## Async: Promises, ExtendedTask, bindCache, bindEvent

- `hybrid_chain()` (`R/utils.R`) stays synchronous for plain values and
  switches to promise chaining when needed. Outputs, observers and caches all
  use it. Async observers hold `busyCount > 0` until they resolve, which
  defers input updates and output flushes.
- `ExtendedTask` (`R/extended-task.R`) keeps private `reactiveVal`s for
  status, value and error. `invoke()` runs `func` under
  `maskReactiveContext()` and queues the call if a run is already active.
  While running, `result()` is `req(FALSE, cancelOutput = "progress")`.
- `bindCache()` (`R/bind-cache.R`) has S3 methods for `reactiveExpr`,
  `shiny.render.function` and `shiny.renderPlot`. The key is
  `rlang::hash(list(keyResult, cacheHint))`. The cache is resolved by
  `resolve_cache_object()` (`R/cache-utils.R`): `"app"` uses
  `getShinyOption("cache")` and `"session"` uses `session$cache`.
- `bindEvent()` (`R/bind-event.R`) adds the `reactive.event` /
  `*.event` classes. `bindCache()` after `bindEvent()` errors, and so does
  calling either one twice.

## testServer / MockShinySession

Implementation: `R/test-server.R`, `R/mock-session.R`, `R/timer.R`
(`MockableTimerCallbacks`).

- `testServer()` runs the server or module under `withMockContext()` (which
  combines `isolate()`, `withReactiveDomain()` and
  `shiny.allowoutputreads = TRUE`), then evaluates `expr` in a data mask built
  from the captured function env (`session$setEnv()`).
- `MockShinySession` re-implements the session surface. Its `setInputs()`
  calls `private$flush()` (`flushCBs`, then `flushReact()`, then `flushedCBs`,
  then `later::run_now()`). `elapse()` advances mock time. `makeScope()`,
  `destroy()`, `close()` and `invokeDestroyCallbacks()` mirror
  `ShinySession`. Unsupported methods come from `makeExtraMethods()` (noops
  that warn or error).

## OpenTelemetry (brief)

Files: `R/otel-*.R`. `init_otel()` (`R/otel-shiny.R`, run on load) caches the
tracer and logger in `.globals`. `has_otel_collect(level)` checks
`shiny.otel.collect` / `SHINY_OTEL_COLLECT` against `otel_collect_choices`
(`none < session < reactive_update < reactivity < all`). The spans are:

- session start and end (`R/otel-session.R`);
- the reactive update between busy and idle (`R/otel-reactive-update.R`);
- one span per reactive or observer through the `Context`'s `otel_info`
  (`ctx_otel_info_obj()`).

Primitives opt in at creation time (`enable_otel_*()` in `R/otel-enable.R`),
so the collect level is captured when the object is built.
`with_no_otel_collect()` hides internal reactives.

## JS Bundle

- Source is in `srcts/src/` (entry `index.ts`). The build scripts are in
  `srcts/build/`: `shiny.ts` uses esbuild with jQuery mapped to
  `window.jQuery`, and `_build.ts` sets `outDir = "./inst/www/shared/"`.
- `npm run build` runs `checks` (eslint `--fix`, `tsc`, unit tests,
  type-coverage of at least 90%, `dpdm` circular-dependency check), then
  bundles `shiny.js` and `shiny.min.js`, the extras (autoreload, showcase,
  testmode) and the external libs.
- `npm run watch` rebuilds only `bundle_shiny`.

## Pitfalls

- **Close is not destroy.** After `close()` / `wsClosed()`, reactives are
  still readable. Don't add teardown that `stop()`s on close. Any new
  `onDestroy` user must guard with `isClosedDomain()`, as
  `make_weak_destroy_wrapper()` does. `MockShinySession$close()` must keep the
  same ordering: mark closed first, then ended callbacks, then
  `invokeDestroyCallbacks(allowRoot = TRUE)`.
- **Duplicated teardown logic.** `MockShinySession`'s private
  `invokeDestroyCallbacks()` is a copy of the `ShinySession` version, but it
  only cleans inputs. It skips clientData, outputs and download routes. Change
  both together.
- **Bookmark callbacks accumulate.** Every `makeScope()` call registers
  bookmark and restore callbacks plus a bookmark-exclude function on the root
  session. They are removed only by that scope's `onDestroy`. Avoid calling
  `makeScope()` speculatively; `destroy(ns)` itself calls `makeScope()` first.
- **Namespaces must be length 1.** `validateNamespace()` (also called by
  `makeScope()`) requires a length-1, non-empty string that is not `"..root"`,
  even though `NS()` accepts vectors. Destroy matches by prefix: `destroy("a")`
  also tears down `a-b`.
- **Inputs are deferred while busy.** `manageInputs()` waits whenever any
  observer in the session is busy, including pending async work. Code that
  needs inputs immediately uses `now = TRUE`; the only caller is `init`.
- **Outputs missing from the page never run.** With no
  `output_<id>_hidden` clientData, `shouldSuspend()` treats the output as
  hidden, so it stays suspended unless `suspendWhenHidden = FALSE`.
- **Input ids are split on `:`.** In `applyInputHandler()`, an id containing
  `:` is parsed as a type suffix and errors with "No handler registered".
- **Input dedupe differs.** `.input` is `ReactiveValues(dedupe = FALSE)`, so
  every client send invalidates. `.clientData` uses `dedupe = TRUE`.
- **Output attributes live on an environment.** `defineOutput()` sets output
  attrs with `attr(obs, name) <-` inside `lapply()`. This works only because
  R6 objects are environments, whose attributes are shared. `getOutput()`
  depends on it.
- **Weak context references.** `Observable` contexts are weak in
  `Dependents`. `Observable$.ctx` holds the strong reference that keeps them
  alive, so don't drop it.
- **Handler order matters.** JS message handler order is registration order
  in `ShinyApp._init()`, and `values` must stay ahead of handlers that rely on
  updated outputs.
- **Commit rebuilt bundles.** The built `inst/www/shared/shiny*.js` files are
  committed. Edit `srcts/` and rebuild, and commit the rebuilt bundle with
  your change. `npm run lint` runs `eslint --fix` and rewrites files.
