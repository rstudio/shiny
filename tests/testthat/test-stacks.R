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
  labelEnv <- new.env()
  labelEnv$.shinyAsyncLabel <- "observe"
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
  out <- relabelAsyncFrames(calls, parents, frames)
  expect_identical(attr(out, "asyncFrames"), 5L)
  expect_identical(out[[5]], call("observe (coro::async)"))
  # Hidden from the call to the async function to the frame's parent
  expect_identical(
    attr(out, "asyncHidden"),
    c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE)
  )
  expect_identical(out[-5], calls[-5])
})

test_that("relabelAsyncFrames() leaves a stack with no await() code alone", {
  calls <- list(quote(a()), quote(b()))
  out <- relabelAsyncFrames(calls, c(0L, 1L), list(globalenv(), globalenv()))
  expect_identical(out, calls)
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
