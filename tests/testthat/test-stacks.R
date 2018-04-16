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

  expect_equal(df$num, c(56L, 55L, 54L, 38L, 37L, 36L, 35L, 
    34L, 33L, 32L, 31L, 30L))
  expect_equal(df$call, c("A", "B", "<reactive:C>", "C", "renderTable", 
    "func", "force", "withVisible", "withCallingHandlers", "globals$domain$wrapSync", 
    "promises::with_promise_domain", "captureStackTraces"))
  expect_equal(nzchar(df$loc), c(TRUE, TRUE, TRUE, FALSE, TRUE, 
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
  
  df <- causeError(full = TRUE)
  # dumpTests(df)

  expect_equal(df$num, c(59L, 58L, 57L, 56L, 55L, 54L, 53L, 
    52L, 51L, 50L, 49L, 48L, 47L, 46L, 45L, 44L, 43L, 42L, 41L, 
    40L, 39L, 38L, 37L, 36L, 35L, 34L, 33L, 32L, 31L, 30L, 29L, 
    28L, 27L, 26L, 25L, 24L, 23L, 22L, 21L, 20L, 19L, 18L, 17L, 
    16L, 15L, 14L, 13L, 12L, 11L, 10L, 9L, 8L, 7L, 6L, 5L, 4L, 
    3L, 2L, 1L))
  expect_equal(df$call, c("h", ".handleSimpleError", "stop", 
    "A", "B", "<reactive:C>", "..stacktraceon..", ".func", "withVisible", 
    "withCallingHandlers", "contextFunc", "env$runWith", "force", 
    "globals$domain$wrapSync", "promises::with_promise_domain", 
    "withReactiveDomain", "globals$domain$wrapSync", "promises::with_promise_domain", 
    "ctx$run", "self$.updateValue", "..stacktraceoff..", "C", 
    "renderTable", "func", "force", "withVisible", "withCallingHandlers", 
    "globals$domain$wrapSync", "promises::with_promise_domain", 
    "captureStackTraces", "doTryCatch", "tryCatchOne", "tryCatchList", 
    "tryCatch", "do", "hybrid_chain", "origRenderFunc", "renderTable({     C() }, server = FALSE)", 
    "..stacktraceon..", "contextFunc", "env$runWith", "force", 
    "globals$domain$wrapSync", "promises::with_promise_domain", 
    "withReactiveDomain", "globals$domain$wrapSync", "promises::with_promise_domain", 
    "ctx$run", "..stacktraceoff..", "isolate", "withCallingHandlers", 
    "globals$domain$wrapSync", "promises::with_promise_domain", 
    "captureStackTraces", "doTryCatch", "tryCatchOne", "tryCatchList", 
    "tryCatch", "try"))
  expect_equal(nzchar(df$loc), c(FALSE, FALSE, FALSE, TRUE, 
    TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
    TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, 
    FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
    FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, 
    FALSE))
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

  captureErrorLog(stop("boom"))
  expect_true(!is.null(caught))
})
