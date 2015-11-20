context("stacks")

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

  res <- try(captureStackTraces(isolate(renderTable({C()}, server = FALSE)())),
    silent = TRUE)
  cond <- attr(res, "condition", exact = TRUE)

  df <- extractStackTrace(conditionStackTrace(cond), full = full)
  df$loc <- cleanLocs(df$loc)
  # Compensate for this test being called from different call sites;
  # whack the
  df <- head(df, -sys.nframe())
  df$num <- df$num - sys.nframe()
  df
}

cleanLocs <- function(locs) {
  locs[!grepl("test-stacks\\.R", locs, perl = TRUE)] <- ""
  sub("^.*#", "", locs)
}

dumpTests <- function(df) {
  print(bquote({
    expect_equal(df$num, .(df$num))
    expect_equal(df$call, .(df$call))
    expect_equal(nzchar(df$loc), .(nzchar(df$loc)))
  }))
}

test_that("integration tests", {
  df <- causeError(full = FALSE)
  # dumpTests(df)

  expect_equal(df$num, c(31L, 30L, 29L, 18L, 17L, 16L, 15L,
    8L, 7L, 6L, 5L, 4L, 3L, 2L, 1L))
  expect_equal(df$call, c("A", "B", "reactive C", "C", "renderTable",
    "func", "renderTable({     C() }, server = FALSE)", "isolate",
    "withCallingHandlers", "captureStackTraces", "doTryCatch",
    "tryCatchOne", "tryCatchList", "tryCatch", "try"))
  expect_equal(nzchar(df$loc), c(TRUE, TRUE, TRUE, FALSE, TRUE,
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    FALSE))

  df <- causeError(full = TRUE)
  # dumpTests(df)

  expect_equal(df$num, c(34L, 33L, 32L, 31L, 30L, 29L, 28L,
    27L, 26L, 25L, 24L, 23L, 22L, 21L, 20L, 19L, 18L, 17L, 16L,
    15L, 14L, 13L, 12L, 11L, 10L, 9L, 8L, 7L, 6L, 5L, 4L, 3L,
    2L, 1L))
  expect_equal(df$call, c("h", ".handleSimpleError", "stop",
    "A", "B", "reactive C", "..stacktraceon..", ".func", "withVisible",
    "withCallingHandlers", "contextFunc", "env$runWith", "withReactiveDomain",
    "ctx$run", "self$.updateValue", "..stacktraceoff..", "C",
    "renderTable", "func", "renderTable({     C() }, server = FALSE)",
    "..stacktraceon..", "contextFunc", "env$runWith", "withReactiveDomain",
    "ctx$run", "..stacktraceoff..", "isolate", "withCallingHandlers",
    "captureStackTraces", "doTryCatch", "tryCatchOne", "tryCatchList",
    "tryCatch", "try"))
  expect_equal(nzchar(df$loc), c(FALSE, FALSE, FALSE, TRUE,
    TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE,
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    FALSE, FALSE, FALSE))
})
