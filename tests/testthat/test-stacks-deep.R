formatError <- function(err, full = FALSE, offset = TRUE, cleanPaths = TRUE) {
  # This complicated capturing code is necessary because printStackTrace uses a
  # combination of `message()` and `cat(file=stderr())` to print the error,
  # stack traces, and stack trace boundaries ("From earlier call:"). We want to
  # treat all of it as part of the same string.

  str <- noquote(capture.output(
    suppressWarnings(
      suppressMessages(
        withCallingHandlers(
          printError(err, full = full, offset = offset),
          warning = function(cnd) {
            cat(conditionMessage(cnd), "\n", sep = "", file = stderr())
          },
          message = function(cnd) {
            cat(conditionMessage(cnd), file = stderr())
          }
        )
      )
    ),
    type = "message"
  ))

  # Remove directories and line numbers from file/line references, e.g.
  #   53: callback [/Users/jcheng/Development/rstudio/shiny/R/conditions.R#155]
  # becomes
  #   53: callback [conditions.R#XXX]
  #
  # This is to make the snapshot tests more stable across different machines and
  # ignores benign code movement within a file.
  str <- sub("\\[(.*/)?(.+?\\.R)#\\d+\\]", "[\\2#XXX]", str, perl = TRUE)
  # The frame numbers vary too much between different ways of invoking testthat
  # ("Run Tests" editor toolbar button and "Test" Build tab button in RStudio,
  # devtools::test(), etc.) so we blank them out.
  str <- sub("^[ \\d]+:", "    :", str, perl = TRUE)
  str
}


describe("deep stack trace filtering", {
  it("passes smoke test", {
    st <- list(
      c(
        common <- c("1", "2", "..stacktraceoff..", "3", "..stacktracefloor.."),
        "4", "..stacktraceon..", "5"
      ),
      c(common, "6", "..stacktraceoff..", "7"),
      c(common, "8", "..stacktraceon.."),
      c(common, "9")
    )

    expect_equal(
      stripStackTraces(values = TRUE, st),
      jsonlite::fromJSON('[["1", "2", "5"],["6"],[],["9"]]')
    )
  })

  it("handles null cases", {
    expect_equal(
      stripStackTraces(values = TRUE, list(c())),
      list(character(0))
    )
  })

  it("handles various edge cases", {
    expect_equal(
      stripStackTraces(values = TRUE, list(
        c("..stacktraceoff..", "..stacktraceoff..")
      )),
      list(character(0))
    )

    expect_equal(
      stripStackTraces(values = TRUE, list(
        c("..stacktraceoff..", "..stacktraceoff.."),
        c(),
        c("..stacktraceon.."),
        c("..stacktraceon.."),
        c("1")
      )),
      list(character(0), character(0), character(0), character(0), "1")
    )
  })
})

test_that("deep stack capturing", {
  `%...>%` <- promises::`%...>%`
  `%...!%` <- promises::`%...!%`
  finally <- promises::finally

  err <- NULL
  captureStackTraces({
    promise_resolve("one") %...>% {
      promise_reject("error") %...!% {
        finally(promise_resolve("two"), ~{
          stop("boom")
        })
      }
    }
  }) %...!% (function(err) {
    err <<- err
  })

  wait_for_it()

  expect_s3_class(err, "error", exact = FALSE)
  expect_snapshot(formatError(err))
  expect_snapshot(formatError(err, full = TRUE))
})

test_that("deep stack capturing within reactives", {
  rerr <- NULL
  observe({
    promise_resolve("one") %...>% {
      promise_resolve("two") %...>% {
        stop("boom")
      }
    } %...!% (function(err) {
      rerr <<- err
    })
  })

  flushReact()
  wait_for_it()

  expect_s3_class(rerr, "error", exact = FALSE)
  expect_length(attr(rerr, "deep.stack.trace"), 2)
})

test_that("deep stack culling", {
  op <- options(shiny.deepstacktrace = 3L)
  on.exit(options(op), add = TRUE, after = FALSE)

  # Without deep stack traces, the stack trace would give no clue that the error
  # originally started from a call to `A__()`. With deep stack traces, we can
  # see that the error originated from `A__` and passed through `I__` and `J__`.
  # But due to culling, we don't see `B__` through `H__`--these are omitted for
  # brevity and to prevent unbounded growth of the accounting we do.

  A__ <- function() promise_resolve(TRUE) %...>% B__()
  B__ <- function(x) promise_resolve(TRUE) %...>% C__()
  C__ <- function(x) promise_resolve(TRUE) %...>% D__()
  D__ <- function(x) promise_resolve(TRUE) %...>% E__()
  E__ <- function(x) promise_resolve(TRUE) %...>% F__()
  F__ <- function(x) promise_resolve(TRUE) %...>% G__()
  G__ <- function(x) promise_resolve(TRUE) %...>% H__()
  H__ <- function(x) promise_resolve(TRUE) %...>% I__()
  I__ <- function(x) promise_resolve(TRUE) %...>% J__()
  J__ <- function(x) promise_resolve(TRUE) %...>% { stop("boom") }

  dserr <- NULL
  captureStackTraces(
    A__()
  ) %...!% (function(err) {
    dserr <<- err
  })

  wait_for_it()

  expect_s3_class(dserr, "error", exact = FALSE)
  stacktrace <- formatError(dserr)
  expect_snapshot(stacktrace)
  # Ensure that A__, I__, and J__ are present in the traces
  expect_length(which(grepl("A__", stacktrace)), 1L)
  expect_length(which(grepl("I__", stacktrace)), 1L)
  expect_length(which(grepl("J__", stacktrace)), 1L)

  # Ensure the message appears
  expect_length(which(grepl("omitted 7 more stack traces", stacktrace)), 1L)

  # Ensure that B__ through H__ are not present in the traces; these are the
  # calls that were omitted
  expect_false(any(grepl("B__", stacktrace)))
  expect_false(any(grepl("C__", stacktrace)))
  expect_false(any(grepl("D__", stacktrace)))
  expect_false(any(grepl("E__", stacktrace)))
  expect_false(any(grepl("F__", stacktrace)))
  expect_false(any(grepl("G__", stacktrace)))
  expect_false(any(grepl("H__", stacktrace)))
})

test_that("appendWithLimit edge cases", {
  # Can't have a limit of 0
  expect_error(appendWithLimit(list("a", "b", "c", "d", "e"), "f", 0), "limit")
  # Can't have a keep_head of 0
  expect_error(appendWithLimit(list("a", "b", "c", "d", "e"), "f", 5, 0), "keep_head")
  # Can't have keep_head >= limit
  expect_error(appendWithLimit(list("a", "b", "c", "d", "e"), "f", 5, 5), "keep_head")
  # Can't change keep_head
  expect_error(appendWithLimit(list("a", 1L, "c", "d", "e"), "f", 5, 2), "keep_head")

  # Fits
  expect_identical(
    appendWithLimit(list("a", "b", "c", "d", "e"), "f", 6),
    list("a", "b", "c", "d", "e", "f")
  )
  # Elide 1
  expect_identical(
    appendWithLimit(list("a", "b", "c", "d", "e"), "f", 5),
    list("a", 1L, "c", "d", "e", "f")
  )
  # Elide from already elided list
  expect_identical(
    appendWithLimit(list("a", 1L, "c", "d", "e", "f"), "g", 5),
    list("a", 2L, "d", "e", "f", "g")
  )
  expect_identical(
    appendWithLimit(list("a", "b", "c", "d", "e"), "f", 5, 2),
    list("a", "b", 1L, "d", "e", "f")
  )
})

test_that("unlimited deep stacks can be opted into", {
  recursive_promise <- function(n) {
    if (n <= 0) {
      stop("boom")
    }

    p <- promises::promise_resolve(TRUE)
    promises::then(p, ~{
      recursive_promise(n - 1)
    })
  }

  op <- options(shiny.deepstacktrace = TRUE)
  on.exit(options(op), add = TRUE, after = FALSE)

  uerr <- NULL
  captureStackTraces(recursive_promise(100)) %...!% (function(err) {
    uerr <<- err
  })

  wait_for_it()

  expect_s3_class(uerr, "error", exact = FALSE)
  expect_identical(length(attr(uerr, "deep.stack.trace", exact = TRUE)), 100L)
})
