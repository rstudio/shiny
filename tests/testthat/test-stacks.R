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
  # whack the
  df <- head(df, -sys.nframe())
  df$num <- df$num - sys.nframe()
  df
}

#' @details `extractStackTrace` takes a list of calls (e.g. as returned
#'   from `conditionStackTrace(cond)`) and returns a data frame with one
#'   row for each stack frame and the columns `num` (stack frame number),
#'   `call` (a function name or similar), and `loc` (source file path
#'   and line number, if available). It was deprecated after shiny 1.0.5 because
#'   it doesn't support deep stack traces.
#' @rdname stacktrace
#' @export
extractStackTrace <- function(calls,
                              full = get_devmode_option("shiny.fullstacktrace", FALSE),
                              offset = getOption("shiny.stacktraceoffset", TRUE)) {

  srcrefs <- getSrcRefs(calls)
  if (offset) {
    # Offset calls vs. srcrefs by 1 to make them more intuitive.
    # E.g. for "foo [bar.R:10]", line 10 of bar.R will be part of
    # the definition of foo().
    srcrefs <- c(utils::tail(srcrefs, -1), list(NULL))
  }
  calls <- setSrcRefs(calls, srcrefs)

  callnames <- getCallNames(calls)

  # Hide and show parts of the callstack based on ..stacktrace(on|off)..
  if (full) {
    toShow <- rep.int(TRUE, length(calls))
  } else {
    # Remove stop(), .handleSimpleError(), and h() calls from the end of
    # the calls--they don't add any helpful information. But only remove
    # the last *contiguous* block of them, and then, only if they are the
    # last thing in the calls list.
    hideable <- callnames %in% c("stop", ".handleSimpleError", "h")
    # What's the last that *didn't* match stop/.handleSimpleError/h?
    lastGoodCall <- max(which(!hideable))
    toRemove <- length(calls) - lastGoodCall
    # But don't remove more than 5 levels--that's an indication we might
    # have gotten it wrong, I guess
    if (toRemove > 0 && toRemove < 5) {
      calls <- utils::head(calls, -toRemove)
      callnames <- utils::head(callnames, -toRemove)
    }

    # This uses a ref-counting scheme. It might make sense to switch this
    # to a toggling scheme, so the most recent ..stacktrace(on|off)..
    # directive wins, regardless of what came before it.
    # Also explicitly remove ..stacktraceon.. because it can appear with
    # score > 0 but still should never be shown.
    score <- rep.int(0, length(callnames))
    score[callnames == "..stacktraceoff.."] <- -1
    score[callnames == "..stacktraceon.."] <- 1
    toShow <- (1 + cumsum(score)) > 0 & !(callnames %in% c("..stacktraceon..", "..stacktraceoff..", "..stacktracefloor.."))

    # doTryCatch, tryCatchOne, and tryCatchList are not informative--they're
    # just internals for tryCatch
    toShow <- toShow & !(callnames %in% c("doTryCatch", "tryCatchOne", "tryCatchList"))
  }
  calls <- calls[toShow]

  calls <- rev(calls) # Show in traceback() order
  index <- rev(which(toShow))
  width <- floor(log10(max(index))) + 1

  data.frame(
    num = index,
    call = getCallNames(calls),
    loc = getLocs(calls),
    # category = getCallCategories(calls),
    stringsAsFactors = FALSE
  )
}

cleanLocs <- function(locs) {
  locs[!grepl("test-stacks\\.R", locs, perl = TRUE)] <- ""
  # sub("^.*#", "", locs)
  locs
}

dumpTests <- function(df) {
  print(bquote({
    expect_equal(df$num, .(df$num))
    expect_equal(df$call, .(df$call))
    expect_equal(nzchar(df$loc), .(nzchar(df$loc)))
  }))
}

test_that("integration tests", {
  # The expected call stack can be changed by other packages (namely, promises).
  # If promises changes its internals, it can break this test on CRAN. Because
  # CRAN package releases are generally not synchronized (that is, promises and
  # shiny can't be updated at the same time, unless there is manual intervention
  # from CRAN maintaineres), these specific test expectations make it impossible
  # to release a version of promises that will not break this test and cause
  # problems on CRAN.
  skip_on_cran()

  df <- causeError(full = FALSE)
  # dumpTests(df)

  expect_snapshot(df)

  df <- causeError(full = TRUE)

  expect_snapshot(df)
  # dumpTests(df)
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
  expect_true(any(grepl("observeEvent\\(1\\)", st_str)))

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
  expect_true(any(grepl("A__", st_str)))
  expect_true(any(grepl("B__", st_str)))
})
