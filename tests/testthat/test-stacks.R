causeError <- function(full) {
  A <- function() {
    stop("foo")
  }

  B <- function() {
    A()
  }

  C <- reactive({
    B()
  })

  res <- try({
      captureStackTraces({
        isolate({
          renderTable({
            C()
          }, server = FALSE)()
        })
      })
    },
    silent = TRUE)
  cond <- attr(res, "condition", exact = TRUE)

  suppressMessages(df <- extractStackTrace(conditionStackTrace(cond), full = full))
  df$loc <- cleanLocs(df$loc)
  # Compensate for this test being called from different call sites;
  # whack the top n frames off using the `num` frame column
  df <- df[df$num >= sys.nframe(), ]
  df$num <- df$num - sys.nframe()
  df
}

test_that("integration tests", {
  if (shiny_otel_tracer()$is_enabled()) {
    announce_snapshot_file(name = "stacks.md")

    skip("Skipping stack trace tests when OpenTelemetry is already enabled")
  }

  # The expected call stack can be changed by other packages (namely, promises).
  # If promises changes its internals, it can break this test on CRAN. Because
  # CRAN package releases are generally not synchronized (that is, promises and
  # shiny can't be updated at the same time, unless there is manual intervention
  # from CRAN maintaineres), these specific test expectations make it impossible
  # to release a version of promises that will not break this test and cause
  # problems on CRAN.
  skip_on_cran()

  df_integration_slim <- causeError(full = FALSE)
  # dumpTests(df_integration_slim)

  expect_snapshot(df_integration_slim)

  df_integration_full <- causeError(full = TRUE)

  expect_snapshot(df_integration_full)
  # dumpTests(df_integration_full)
})

test_that("shiny.error", {
  caught <- NULL
  op <- options(shiny.error = function() { caught <<- TRUE })
  on.exit(options(op))

  # Regular errors should be intercepted by shiny.error
  try(shiny:::shinyCallingHandlers(stop("boom")), silent = TRUE)
  expect_true(caught)

  caught <- NULL

  # Validation errors shouldn't be intercepted by shiny.error

  try(shiny:::shinyCallingHandlers(validate(need(NULL, FALSE))), silent = TRUE)
  expect_null(caught)

  er <- eventReactive(NULL, { "Hello" })
  try(shiny:::shinyCallingHandlers(isolate(er())), silent = TRUE)
  expect_null(caught)
  try(shiny:::shinyCallingHandlers(isolate(er())), silent = TRUE)
  expect_null(caught)
})

test_that("chained silent errors aren't intercepted (tidyverse/dplyr#5552)", {
  withr::local_options(
    shiny.error = function() caught <<- TRUE
  )

  f <- function() {
    withCallingHandlers(
      validate(need(NULL, FALSE)),
      error = function(cnd) {
        rlang::abort("Child error.", parent = cnd)
      }
    )
  }
  caught <- NULL
  try(shiny:::shinyCallingHandlers(f()), silent = TRUE)
  expect_null(caught)

  caught <- NULL
  try(hybrid_chain(f()), silent = TRUE)
  expect_null(caught)
})

test_that("validation error logging", {
  caught <- NULL

  # Given an error-throwing exception expr, execute it
  # using withLogErrors, and superassign the warning that
  # results (the error log is emitted using warning())
  # into the parent variable `caught`
  captureErrorLog <- function(expr) {
    tryCatch(
      tryCatch(
        shiny::withLogErrors(expr),
        warning = function(cond) {
          caught <<- cond
        }
      ),
      error = function(e) {
      }
    )
  }

  captureErrorLog(validate("boom"))
  expect_null(caught)

  caught <- NULL
  captureErrorLog(stop("boom"))
  expect_true(!is.null(caught))
})

test_that("observeEvent is not overly stripped (#4162)", {
  caught <- NULL
  ..stacktraceoff..(
    ..stacktracefloor..({
      observeEvent(1, {
        tryCatch(
          captureStackTraces(stop("boom")),
          error = function(cond) {
            caught <<- cond
          }
        )
      })
      flushReact()
    })
  )
  st_str <- capture.output(printStackTrace(caught), type = "message")
  expect_match(st_str, "observeEvent\\(1\\)", all = FALSE)

  # Now same thing, but deep stack trace version

  A__ <- function() {
    promises::then(promises::promise_resolve(TRUE), ~{
      stop("boom")
    })
  }

  B__ <- function() {
    promises::then(promises::promise_resolve(TRUE), ~{
      A__()
    })
  }

  caught <- NULL
  ..stacktraceoff..(
    ..stacktracefloor..({
      observeEvent(1, {
        captureStackTraces(promises::catch(B__(), ~{
          caught <<- .
        }))
      })
      flushReact()
      wait_for_it()
    })
  )
  st_str <- capture.output(printStackTrace(caught), type = "message")
  # cat(st_str, sep = "\n")
  expect_match(st_str, "A__", all = FALSE)
  expect_match(st_str, "B__", all = FALSE)
})

test_that("renderPlot stack trace fences hide internal rendering pipeline (#4357)", {
  skip_on_cran()

  skip_if_shiny_otel_tracer_is_enabled()

  userFunc <- function() {
    stop("test error in renderPlot")
  }

  df <- captureFilteredRenderTrace(renderPlot({ userFunc() }))

  expect_true("userFunc" %in% df$call)

  # Internal rendering pipeline frames should NOT appear in the filtered
  # stack trace. These are Shiny internals between the stack trace fences
  # that currently leak through due to missing fences.
  internal_render_frames <- c(
    "drawPlot",
    "drawReactive",
    "renderFunc",
    "startPNG"
  )

  leaked <- df$call[df$call %in% internal_render_frames]
  expect_length(leaked, 0)
})

test_that("renderPrint stack trace fences hide internal rendering pipeline (#4357)", {
  skip_on_cran()

  skip_if_shiny_otel_tracer_is_enabled()

  userFunc <- function() {
    stop("test error in renderPrint")
  }

  df <- captureFilteredRenderTrace(renderPrint({ userFunc() }))

  expect_true("userFunc" %in% df$call)

  internal_render_frames <- c("renderFunc")
  leaked <- df$call[df$call %in% internal_render_frames]
  expect_length(leaked, 0)
})

test_that("renderText stack trace fences hide internal rendering pipeline (#4357)", {
  skip_on_cran()

  skip_if_shiny_otel_tracer_is_enabled()

  userFunc <- function() {
    stop("test error in renderText")
  }

  df <- captureFilteredRenderTrace(renderText({ userFunc() }), needs_session = FALSE)

  expect_true("userFunc" %in% df$call)

  internal_render_frames <- c("renderFunc")
  leaked <- df$call[df$call %in% internal_render_frames]
  expect_length(leaked, 0)
})

test_that("renderUI stack trace fences hide internal rendering pipeline (#4357)", {
  skip_on_cran()

  skip_if_shiny_otel_tracer_is_enabled()

  userFunc <- function() {
    stop("test error in renderUI")
  }

  df <- captureFilteredRenderTrace(renderUI({ userFunc() }), needs_session = FALSE)

  expect_true("userFunc" %in% df$call)

  internal_render_frames <- c("renderFunc")
  leaked <- df$call[df$call %in% internal_render_frames]
  expect_length(leaked, 0)
})

test_that("renderTable stack trace fences hide internal rendering pipeline (#4357)", {
  skip_on_cran()

  skip_if_shiny_otel_tracer_is_enabled()

  userFunc <- function() {
    stop("test error in renderTable")
  }

  df <- captureFilteredRenderTrace(
    renderTable({ userFunc() }, server = FALSE),
    needs_session = FALSE
  )

  expect_true("userFunc" %in% df$call)

  internal_render_frames <- c("renderFunc")
  leaked <- df$call[df$call %in% internal_render_frames]
  expect_length(leaked, 0)
})

test_that("renderImage stack trace fences hide internal rendering pipeline (#4357)", {
  skip_on_cran()

  skip_if_shiny_otel_tracer_is_enabled()

  userFunc <- function() {
    stop("test error in renderImage")
  }

  df <- captureFilteredRenderTrace(
    renderImage({ userFunc() }, deleteFile = FALSE),
    needs_session = FALSE
  )

  expect_true("userFunc" %in% df$call)

  internal_render_frames <- c("renderFunc")
  leaked <- df$call[df$call %in% internal_render_frames]
  expect_length(leaked, 0)
})

test_that("legacyRenderDataTable stack trace fences hide internal rendering pipeline (#4357)", {
  skip_on_cran()

  skip_if_shiny_otel_tracer_is_enabled()

  userFunc <- function() {
    stop("test error in renderDataTable")
  }

  df <- captureFilteredRenderTrace(
    legacyRenderDataTable({ userFunc() })
  )

  expect_true("userFunc" %in% df$call)

  internal_render_frames <- c("renderFunc")
  leaked <- df$call[df$call %in% internal_render_frames]
  expect_length(leaked, 0)
})

test_that("markRenderFunction preserves user frames outside reactive domain", {
  skip_on_cran()

  skip_if_shiny_otel_tracer_is_enabled()

  # htmlwidgets-style: exprToFunction + markRenderFunction, no ..stacktraceon..
  renderWidgetLike <- function(expr, env = parent.frame(), quoted = FALSE) {
    if (!quoted) expr <- substitute(expr)
    func <- exprToFunction(expr, env, TRUE)
    renderFunc <- function() { func() }
    markRenderFunction(textOutput, renderFunc)
  }

  userFunc <- function() stop("boom")
  render_fn <- renderWidgetLike({ userFunc() })

  res <- try(captureStackTraces({ render_fn() }), silent = TRUE)
  cond <- attr(res, "condition", exact = TRUE)
  df <- extractStackTrace(conditionStackTrace(cond), full = FALSE)

  expect_true("userFunc" %in% df$call)
})


# ---- await() ------------------------------------------------------------------

# "test-stacks.R#<line>]", where <line> is the line of this file that is just
# `text`
srcLoc <- function(text) {
  line <- which(trimws(readLines(test_path("test-stacks.R"))) == text)
  stopifnot(length(line) == 1)
  paste0("test-stacks.R#", line, "]")
}

# Run `expr` the way `runApp()` does, wait for its promises, and return the
# error passed to `caught()` along with its printed stack trace
catchAsyncError <- function(expr) {
  caught <- NULL
  caught_fn <- function(e) caught <<- e
  ..stacktraceoff..(
    ..stacktracefloor..({
      expr(caught_fn)
      flushReact()
      wait_for_it()
    })
  )
  list(
    cond = caught,
    trace = capture.output(printStackTrace(caught), type = "message")
  )
}

test_that("an error after await() in a reactive shows the reactive and its line", {
  skip_if_not_installed("coro")
  out <- catchAsyncError(function(caught) {
    r <- reactive(label = "r", {
      x <- await(promises::promise_resolve(1))
      stop("boom after await in reactive")
    })
    observeEvent(1, {
      captureStackTraces(promises::catch(r(), caught))
    })
  })
  expect_identical(getCallNames(list(conditionCall(out$cond))), "<reactive:r>")
  expect_match(
    out$trace,
    paste0(
      "<reactive:r> (coro::async) [",
      srcLoc('stop("boom after await in reactive")')
    ),
    fixed = TRUE,
    all = FALSE
  )
  # coro's and the promise package's frames tell the user nothing
  expect_no_match(out$trace, "eval_bare|evalq|onFulfilled|then")
  # The deep stack trace shows where the code started
  expect_match(out$trace, "From earlier call:", fixed = TRUE, all = FALSE)
  expect_match(out$trace, "observeEvent(1)", fixed = TRUE, all = FALSE)
})

test_that("an error after await() in renderText() shows renderText and its line", {
  skip_if_not_installed("coro")
  out <- catchAsyncError(function(caught) {
    p <- isolate(captureStackTraces(renderText({
      x <- await(promises::promise_resolve(1))
      stop("boom after await in renderText")
    })()))
    promises::catch(p, caught)
  })
  expect_identical(getCallNames(list(conditionCall(out$cond))), "renderText")
  expect_match(
    out$trace,
    paste0(
      "renderText (coro::async) [",
      srcLoc('stop("boom after await in renderText")')
    ),
    fixed = TRUE,
    all = FALSE
  )
  expect_no_match(out$trace, "eval_bare|evalq|onFulfilled|then")
})

test_that("an error before await() shows the reactive once, with its line", {
  skip_if_not_installed("coro")
  out <- catchAsyncError(function(caught) {
    r <- reactive(label = "r", {
      stop("boom before await")
      x <- await(promises::promise_resolve(1))
    })
    observeEvent(1, {
      captureStackTraces(tryCatch(promises::catch(r(), caught), error = caught))
    })
  })
  expect_identical(getCallNames(list(conditionCall(out$cond))), "<reactive:r>")
  frames <- grep("<reactive:r>", out$trace, fixed = TRUE, value = TRUE)
  expect_length(frames, 1)
  expect_match(
    frames,
    paste0("<reactive:r> (coro::async) [", srcLoc('stop("boom before await")')),
    fixed = TRUE
  )
  expect_no_match(out$trace, "eval_bare|evalq|instance")
})

# Run `setup()` the way `runApp()` does and wait for its promises. Return the
# "Error in" heading and the stack trace that an observer printed for an error
printedObserverError <- function(setup) {
  heading <- NULL
  trace <- capture.output(type = "message", {
    withCallingHandlers(
      ..stacktraceoff..(
        ..stacktracefloor..({
          setup()
          flushReact()
          wait_for_it()
        })
      ),
      warning = function(w) {
        heading <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    )
  })
  list(heading = heading, trace = trace)
}

# Each kind of code that can use `await()`: the label its errors and frame get
# (the same as without `await()`, except for event and cache key code, which
# is `eval_tidy` there), and a function that makes it from `body`. Reactives
# and render functions are read by an observer.
asyncLabelCases <- list(
  list("reactive()", "<reactive:r>", function(body) {
    r <- inject(reactive(label = "r", !!body))
    observe(r())
  }),
  list("observe()", "observe", function(body) {
    inject(observe(!!body))
  }),
  list("eventReactive() event", "eventExpr", function(body) {
    r <- inject(eventReactive(!!body, 1))
    observe(r())
  }),
  list("eventReactive() value", "eventReactiveValueFunc", function(body) {
    r <- inject(eventReactive(1, !!body))
    observe(r())
  }),
  list("observeEvent() event", "eventExpr", function(body) {
    inject(observeEvent(!!body, 1))
  }),
  list("observeEvent() handler", "observe", function(body) {
    inject(observeEvent(1, !!body))
  }),
  list("bindEvent() event of reactive()", "eventExpr", function(body) {
    r <- inject(bindEvent(reactive(1), !!body))
    observe(r())
  }),
  list("reactive() with bindEvent()", "eventReactiveValueFunc", function(body) {
    r <- inject(bindEvent(reactive(!!body), 1))
    observe(r())
  }),
  list("bindEvent() event of observe()", "eventExpr", function(body) {
    inject(bindEvent(observe(1), !!body))
  }),
  list("observe() with bindEvent()", "observe", function(body) {
    inject(bindEvent(observe(!!body), 1))
  }),
  list("bindCache() key of reactive()", "cacheKeyExpr", function(body) {
    r <- inject(bindCache(reactive(1), !!body, cache = cachem::cache_mem()))
    observe(r())
  }),
  list("reactive() with bindCache()", "cachedReactiveValueFunc", function(body) {
    r <- inject(bindCache(reactive(!!body), 1, cache = cachem::cache_mem()))
    observe(r())
  }),
  list("renderText()", "renderText", function(body) {
    f <- inject(renderText(!!body))
    observe(f())
  }),
  list("bindEvent() event of renderText()", "eventExpr", function(body) {
    f <- inject(bindEvent(renderText(1), !!body))
    observe(f())
  }),
  list("bindCache() key of renderText()", "cacheKeyExpr", function(body) {
    f <- inject(bindCache(renderText(1), !!body, cache = cachem::cache_mem()))
    observe(f())
  })
)

asyncErrorBodies <- list(
  before = quote({
    stop("labelled boom before await")
    x <- await(promises::promise_resolve(1))
  }),
  after = quote({
    x <- await(promises::promise_resolve(1))
    stop("labelled boom after await")
  })
)

# Frames of coro, the promise package, the event loop, and Shiny's reactive
# plumbing
asyncNoise <- paste(
  "eval_bare", "eval_tidy", "evalq", "instance", "onFulfilled", "then",
  "execCallbacks", "run_now", "<Anonymous>", "contextFunc", "runWith",
  "ctx\\$run", "\\.func",
  sep = "|"
)

for (case in asyncLabelCases) {
  local({
    desc <- case[[1]]
    label <- case[[2]]
    make <- case[[3]]
    for (when in names(asyncErrorBodies)) {
      test_that(paste0("an error ", when, " await() in ", desc, " is labelled ", label), {
        skip_if_not_installed("coro")
        out <- printedObserverError(function() make(asyncErrorBodies[[when]]))
        expect_identical(
          out$heading,
          paste0("Error in ", label, ": labelled boom ", when, " await")
        )
        frames <- grep("^ +[0-9]+: ", out$trace, value = TRUE)
        expect_match(
          frames,
          paste0(": ", label, " (coro::async)"),
          fixed = TRUE,
          all = FALSE
        )
        expect_no_match(frames, asyncNoise)
        if (when == "after") {
          # The deep stack trace shows where the code started, and the
          # frame shows the line it was at
          expect_match(out$trace, "From earlier call:", fixed = TRUE, all = FALSE)
          expect_match(
            frames,
            paste0(": ", label, " (coro::async) [test-stacks.R#"),
            fixed = TRUE,
            all = FALSE
          )
        }
      })
    }
  })
}

# Not at the top level of the file, so that their srcrefs are kept
local({
  lazyHelper <<- function(x) {
    y <- 1
    x + y
  }
  lazyBoom <<- function() {
    stop("boom in a lazily forced argument")
  }
  lazyCaller <<- function() {
    lazyHelper(lazyBoom())
  }
})

test_that("a lazily forced argument does not change its caller's line", {
  # `lazyBoom()` runs when `lazyHelper()` forces `x`, so its srcref is a line
  # of `lazyHelper()`, though its parent frame is `lazyCaller()`
  err <- tryCatch(captureStackTraces(lazyCaller()), error = identity)
  trace <- capture.output(printStackTrace(err), type = "message")
  expect_match(
    trace,
    paste0("lazyCaller [", srcLoc("lazyHelper(lazyBoom())")),
    fixed = TRUE,
    all = FALSE
  )
})

test_that("bindEvent() does not rename a reactive that uses await()", {
  skip_if_not_installed("coro")
  out <- catchAsyncError(function(caught) {
    r <- reactive(label = "r", {
      stop("boom before await, also bound")
      x <- await(promises::promise_resolve(1))
    })
    # Wraps the same async function under another label
    bound <- bindEvent(r, 1)
    observeEvent(1, {
      captureStackTraces(tryCatch(promises::catch(r(), caught), error = caught))
    })
  })
  expect_identical(getCallNames(list(conditionCall(out$cond))), "<reactive:r>")
  expect_match(out$trace, "<reactive:r> (coro::async)", fixed = TRUE, all = FALSE)
  expect_no_match(out$trace, "eventReactiveValueFunc", fixed = TRUE)
  # The frames that called the reactive are still shown
  expect_match(out$trace, "observeEvent(1)", fixed = TRUE, all = FALSE)
})

test_that("a lazily forced argument does not change the await() frame's line", {
  skip_if_not_installed("coro")
  out <- catchAsyncError(function(caught) {
    r <- reactive(label = "r", {
      x <- await(promises::promise_resolve(1))
      lazyHelper(lazyBoom()) # after await
    })
    observeEvent(1, {
      captureStackTraces(promises::catch(r(), caught))
    })
  })
  expect_match(
    out$trace,
    paste0(
      "<reactive:r> (coro::async) [",
      srcLoc("lazyHelper(lazyBoom()) # after await")
    ),
    fixed = TRUE,
    all = FALSE
  )
})

test_that("the deep stack trace shows the file and line of each await()", {
  skip_if_not_installed("coro")
  out <- catchAsyncError(function(caught) {
    r <- reactive(label = "r", {
      x <- await(promises::promise_resolve("first await"))
      y <- await(promises::promise_resolve("second await"))
      z <- await(
        promises::promise_resolve("third await")
      )
      stop("boom after three awaits")
    })
    observeEvent(1, {
      captureStackTraces(promises::catch(r(), caught))
    })
  })
  asyncLoc <- function(text) {
    paste0("<reactive:r> (coro::async) [", srcLoc(text))
  }
  earlier <- which(out$trace == "From earlier call:")
  expect_length(earlier, 2)
  errorTrace <- out$trace[seq_len(earlier[[1]] - 1)]
  latestAwait <- out$trace[(earlier[[1]] + 1):(earlier[[2]] - 1)]
  firstAwait <- out$trace[-seq_len(earlier[[2]])]

  expect_match(errorTrace, asyncLoc('stop("boom after three awaits")'), fixed = TRUE, all = FALSE)
  # The stacks saved at the second and third `await()` are the same but for
  # the line; the latest one is kept
  expect_match(latestAwait, asyncLoc("z <- await("), fixed = TRUE)
  expect_match(
    firstAwait,
    asyncLoc('x <- await(promises::promise_resolve("first await"))'),
    fixed = TRUE,
    all = FALSE
  )
  expect_match(firstAwait, "observeEvent(1)", fixed = TRUE, all = FALSE)
  expect_no_match(out$trace, "second await", fixed = TRUE)
})

test_that("an error with no call of its own after await() shows the reactive and its line", {
  skip_if_not_installed("coro")
  out <- catchAsyncError(function(caught) {
    r <- reactive(label = "r", {
      x <- await(promises::promise_resolve(1))
      x + "not a number"
    })
    observeEvent(1, {
      captureStackTraces(promises::catch(r(), caught))
    })
  })
  expect_match(
    out$trace,
    paste0("<reactive:r> (coro::async) [", srcLoc('x + "not a number"')),
    fixed = TRUE,
    all = FALSE
  )
  expect_no_match(out$trace, "eval_bare|evalq|user|onFulfilled")
})

test_that("an error after await() keeps the deep stack when run from a nested event loop", {
  skip_if_not_installed("coro")
  out <- catchAsyncError(function(caught) {
    r <- reactive(label = "r", {
      x <- await(promises::promise_resolve(1))
      stop("boom after await in nested loop")
    })
    observeEvent(1, {
      captureStackTraces(promises::catch(r(), caught))
      # The code after `await()` runs under this observer's frames, which
      # include `..stacktraceon..` fences
      wait_for_it()
    })
  })
  expect_match(
    out$trace,
    paste0(
      "<reactive:r> (coro::async) [",
      srcLoc('stop("boom after await in nested loop")')
    ),
    fixed = TRUE,
    all = FALSE
  )
  expect_match(out$trace, "From earlier call:", fixed = TRUE, all = FALSE)
  expect_match(out$trace, "observeEvent(1)", fixed = TRUE, all = FALSE)
  expect_no_match(out$trace, "No stack trace available", fixed = TRUE)
  expect_no_match(out$trace, "eval_bare|evalq|onFulfilled|run_now")
})

test_that("an error after await() is not matched to another frame with its label", {
  skip_if_not_installed("coro")
  out <- catchAsyncError(function(caught) {
    p <- isolate(captureStackTraces(renderText({
      x <- await(promises::promise_resolve(1))
      stop("boom after await, label shared")
    })()))
    promises::catch(p, caught)
    # Code with the same label runs the event loop, so the code after
    # `await()` runs under its frames
    isolate(renderText({
      wait_for_it() # outer renderText
      "done"
    })())
  })
  earlier <- which(out$trace == "From earlier call:")
  expect_length(earlier, 1)
  errorTrace <- out$trace[seq_len(earlier - 1)]
  expect_match(
    errorTrace,
    paste0(
      "renderText (coro::async) [",
      srcLoc('stop("boom after await, label shared")')
    ),
    fixed = TRUE,
    all = FALSE
  )
  # Frames outside the code after `await()`, including the other
  # renderText's, are hidden, as when the event loop is not run from inside
  # other code
  expect_match(errorTrace, ": (stop|renderText \\(coro::async\\) \\[.*)$")
})

test_that("a full stack trace keeps every frame of code using await()", {
  skip_if_not_installed("coro")
  out <- catchAsyncError(function(caught) {
    r <- reactive(label = "r", {
      x <- await(promises::promise_resolve(1))
      stop("boom after await, full trace")
    })
    observeEvent(1, {
      captureStackTraces(promises::catch(r(), caught))
    })
  })
  full <- capture.output(printStackTrace(out$cond, full = TRUE), type = "message")
  # Frames that are hidden by default
  expect_match(full, "eval_bare", fixed = TRUE, all = FALSE)
  expect_match(full, "domain$wrapOnFulfilled", fixed = TRUE, all = FALSE)
  expect_match(full, ": <reactive:r>$", all = FALSE)
  # Every `..stacktraceon..` in the trace is a real call, which has arguments
  traces <- c(
    attr(out$cond, "deep.stack.trace", exact = TRUE),
    list(conditionStackTrace(out$cond))
  )
  for (trace in traces) {
    fences <- Filter(function(call) isCallNamed(call, "..stacktraceon.."), trace)
    expect_true(all(lengths(fences) > 1))
  }
})

test_that("relabelAsyncFrames() handles a frame that is its own parent", {
  labelEnv <- new.env()
  labelEnv$.shinyAsyncLabel <- "observe"
  userEnv <- new.env(parent = labelEnv)
  calls <- list(quote(a()), quote(b()), quote(c()), quote(evalq(x)), quote(stop("x")))
  # Frame 2 lists itself as its parent
  parents <- c(0L, 2L, 2L, 3L, 4L)
  frames <- list(globalenv(), globalenv(), globalenv(), userEnv, globalenv())
  out <- relabelAsyncFrames(calls, parents, frames)
  expect_identical(attr(out, "asyncLabel"), "observe")
  expect_identical(attr(out, "asyncFrames"), 4L)
  expect_identical(out[[4]], call("observe (coro::async)"))
  expect_identical(attr(out, "asyncHidden"), c(TRUE, TRUE, TRUE, FALSE, FALSE))
  # The other calls are left as they are
  expect_identical(out[-4], calls[-4])
})

test_that("relabelAsyncFrames() names only the innermost frame of each async function", {
  # coro's own setup frames can also run in children of the async function's
  # env; only the innermost one runs the user's code
  skip_if_not_installed("coro")
  asyncFun <- labelAsyncFunction(
    awaitToAsyncFunction(function() await(1)),
    "observe"
  )
  labelEnv <- environment(coroSourceFn(asyncFun))
  calls <- list(quote(outer()), quote(observe()), quote(local()), quote(eval()), quote(evalq()), quote(stop("x")))
  parents <- c(0L, 1L, 2L, 3L, 4L, 5L)
  frames <- list(
    globalenv(),
    globalenv(),
    globalenv(),
    new.env(parent = labelEnv),
    new.env(parent = labelEnv),
    globalenv()
  )
  # Frame 2 calls the async function
  functionsWith <- function(caller) {
    function(j) if (j == 2L) caller else identity
  }
  out <- relabelAsyncFrames(calls, parents, frames, getFunction = functionsWith(asyncFun))
  expect_identical(attr(out, "asyncFrames"), 5L)
  expect_identical(out[[5]], call("observe (coro::async)"))
  # Hidden from the call to the async function to the frame's parent
  expect_identical(
    attr(out, "asyncHidden"),
    c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE)
  )
  expect_identical(out[-5], calls[-5])

  # A frame with the same name that calls another function is not the
  # caller, so everything outside is hidden, as after an `await()`
  other <- labelAsyncFunction(
    awaitToAsyncFunction(function() await(2)),
    "observe"
  )
  out <- relabelAsyncFrames(calls, parents, frames, getFunction = functionsWith(other))
  expect_identical(
    attr(out, "asyncHidden"),
    c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE)
  )
})

test_that("coroAwaitSrcref() reads the await() line from coro's then() call", {
  srcref <- structure(c(3L, 1L, 3L, 9L, 1L, 9L, 3L, 3L), class = "srcref")
  block <- structure(
    quote({
      a <- 1
      p
    }),
    srcref = list("brace", "first statement", srcref)
  )
  thenCall <- bquote(then(as_promise(user(.(block))), callback = .self))
  expect_identical(coroAwaitSrcref(thenCall), srcref)
})

test_that("coroAwaitSrcref() returns NULL for a then() call of another shape", {
  block <- structure(quote({ p }), srcref = list("brace", "statement"))
  # Not a call, too short, not nested deep enough, not a block, no srcrefs
  expect_null(coroAwaitSrcref(quote(then)))
  expect_null(coroAwaitSrcref(quote(then())))
  expect_null(coroAwaitSrcref(quote(then(p, callback = .self))))
  expect_null(coroAwaitSrcref(quote(then(as_promise(p), callback = .self))))
  expect_null(coroAwaitSrcref(quote(then(as_promise(user(p)), callback = .self))))
  bare <- structure(block, srcref = NULL)
  expect_null(coroAwaitSrcref(bquote(then(as_promise(user(.(bare))), callback = .self))))
  expect_null(coroAwaitSrcref(bquote(then(.(block), callback = .self))))
  expect_null(coroAwaitSrcref(NULL))
})

test_that("relabelAsyncFrames() drops only the await() line when coro's then() call changes", {
  labelEnv <- new.env()
  labelEnv$.shinyAsyncLabel <- "observe"
  frames <- list(globalenv(), new.env(parent = labelEnv), globalenv(), globalenv(), globalenv())
  parents <- c(0L, 1L, 2L, 3L, 4L)
  srcref <- structure(c(3L, 1L, 3L, 9L, 1L, 9L, 3L, 3L), class = "srcref")
  block <- structure(quote({ p }), srcref = list("brace", srcref))
  stackWith <- function(thenCall) {
    calls <- list(
      quote(run_now()),
      quote(base::evalq(x, user_env)),
      quote(base::evalq(y)),
      thenCall,
      quote(domain$wrapOnFulfilled(onFulfilled))
    )
    relabelAsyncFrames(calls, parents, frames, atThen = TRUE)
  }

  known <- stackWith(bquote(then(as_promise(user(.(block))), callback = .self)))
  # The line is put on the call after the user's frame
  expect_identical(attr(known[[3]], "srcref"), srcref)

  # The frame is still named and coro's frames are still hidden
  expectNoLine <- function(out) {
    expect_null(attr(out[[3]], "srcref"))
    expect_identical(out[[2]], call("observe (coro::async)"))
    expect_identical(attr(out, "asyncLabel"), "observe")
    expect_identical(attr(out, "asyncHidden"), attr(known, "asyncHidden"))
  }
  # A `then()` call of another shape
  expectNoLine(stackWith(quote(then(new_wrapper(p), callback = .self))))
  expectNoLine(stackWith(quote(then())))
  # No `then()` call at all
  expectNoLine(stackWith(quote(chain(p))))
})

test_that("relabelAsyncFrames() leaves a stack with no await() code alone", {
  calls <- list(quote(a()), quote(b()))
  out <- relabelAsyncFrames(calls, c(0L, 1L), list(globalenv(), globalenv()))
  expect_identical(out, calls)
})

test_that("stacks are relabelled once code using await() is made", {
  skip_if_not_installed("coro")
  old <- .globals$hasAsyncLabels
  on.exit(.globals$hasAsyncLabels <- old, add = TRUE)
  .globals$hasAsyncLabels <- FALSE
  r <- reactive(label = "r", 1)
  expect_false(.globals$hasAsyncLabels)
  r <- reactive(label = "r", await(promises::promise_resolve(1)))
  expect_true(.globals$hasAsyncLabels)
})

test_that("only stacks saved at a then() inside await() code are relabelled", {
  skip_if_not_installed("coro")
  relabelled <- 0L
  origRelabel <- relabelAsyncFrames
  # Count only stacks saved at a `then()`; errors are always relabelled
  local_mocked_bindings(relabelAsyncFrames = function(..., atThen = FALSE) {
    if (atThen) relabelled <<- relabelled + 1L
    origRelabel(..., atThen = atThen)
  })
  # Code using `await()` exists, but only promises outside it run
  r <- reactive(label = "r", await(promises::promise_resolve(1)))
  printedObserverError(function() {
    observe({
      promises::then(promises::promise_resolve(1), function(x) {
        promises::then(promises::promise_resolve(x), function(y) stop("plain boom"))
      })
    })
  })
  expect_identical(relabelled, 0L)
  expect_identical(.globals$asyncDepth, 0L)

  # Code using `await()` runs, before and after the `await()`, then errors
  out <- printedObserverError(function() {
    observe({
      x <- await(promises::promise_resolve(1))
      x <- await(promises::promise_resolve(x))
      stop("async boom")
    })
  })
  expect_gt(relabelled, 0L)
  expect_identical(out$heading, "Error in observe: async boom")
  expect_identical(.globals$asyncDepth, 0L)
})

test_that("stripStackTraces() leaves hidden frames out of fence matching", {
  deep <- c("user1", "..stacktraceoff..", "internal")
  current <- c("..stacktraceon..", "plumbing", "asyncFrame", "stop")
  # The unmatched fence in `current` hides `internal`, unless the fence is
  # marked as hidden
  expect_identical(
    stripStackTraces(list(deep, current), values = TRUE),
    list("user1", c("plumbing", "asyncFrame", "stop"))
  )
  expect_identical(
    stripStackTraces(
      list(deep, current),
      values = TRUE,
      hidden = list(NULL, c(TRUE, TRUE, FALSE, FALSE))
    ),
    list(c("user1", "internal"), c("asyncFrame", "stop"))
  )
})

test_that("stripStackTraces() keeps earlier segments when the event loop runs inside reactive code", {
  deep <- c("user1", "..stacktraceoff..", "internal")
  # A callback run by the main event loop
  topLevel <- c(
    "runApp", "..stacktraceoff..", "..stacktracefloor..",
    "run_now", "execCallbacks", "callback"
  )
  # The same callback, run by an event loop that an observer started. The
  # observer's `..stacktraceon..` is matched by the `..stacktraceoff..` in the
  # prefix
  nested <- c(
    "runApp", "..stacktraceoff..", "..stacktracefloor..",
    "flush", "..stacktraceon..", "observer", "run_now", "execCallbacks", "callback"
  )
  expect_identical(
    stripStackTraces(list(deep, topLevel), values = TRUE),
    list(c("user1", "internal"), c("run_now", "execCallbacks", "callback"))
  )
  expect_identical(
    stripStackTraces(list(deep, nested), values = TRUE),
    list(c("user1", "internal"), c("observer", "run_now", "execCallbacks", "callback"))
  )
  # A callback that starts with `..stacktraceon..` still hides the end of the
  # segment before it
  expect_identical(
    stripStackTraces(list(deep, c(nested, "..stacktraceon..", "user2")), values = TRUE),
    list("user1", "user2")
  )
  # If later's `execCallbacks()` frame is not found, nothing is cancelled
  renamed <- sub("execCallbacks", "runCallbacks", nested, fixed = TRUE)
  expect_identical(
    stripStackTraces(list(deep, renamed), values = TRUE),
    list("user1", c("observer", "run_now", "runCallbacks", "callback"))
  )
})

test_that("an error in a promise callback keeps the deep stack when run from a nested event loop", {
  out <- catchAsyncError(function(caught) {
    r <- reactive(label = "r", {
      promises::then(promises::promise_resolve(1), function(value) {
        stop("boom in then() in nested loop")
      })
    })
    observeEvent(1, {
      captureStackTraces(promises::catch(r(), caught))
      wait_for_it()
    })
  })
  expect_no_match(out$trace, "No stack trace available", fixed = TRUE)
  earlier <- which(out$trace == "From earlier call:")
  expect_length(earlier, 1)
  deep <- out$trace[-seq_len(earlier)]
  expect_match(deep, "<reactive:r>", fixed = TRUE, all = FALSE)
  expect_match(deep, "observeEvent(1)", fixed = TRUE, all = FALSE)
})
