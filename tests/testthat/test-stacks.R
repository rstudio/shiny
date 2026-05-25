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
  # from CRAN maintainers), these specific test expectations make it impossible
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

