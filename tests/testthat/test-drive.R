test_that("waitForFlush RPC replies only after the session flushes", {
  sent <- list()
  session <- ShinySession$new(list(
    request = list(),
    send = function(json) sent[[length(sent) + 1]] <<- jsonlite::fromJSON(json),
    close = function() invisible(NULL),
    onMessage = function(f) invisible(NULL),
    onClose = function(f) invisible(NULL)
  ))
  responses <- function() Filter(function(m) !is.null(m$response), sent)

  session$dispatch(list(method = "waitForFlush", args = list(), tag = 7))
  for (i in 1:5) later::run_now()
  expect_length(responses(), 0)

  session$flushOutput()
  for (i in 1:5) later::run_now()
  expect_length(responses(), 1)
  expect_equal(responses()[[1]]$response$tag, 7)
  expect_true(responses()[[1]]$response$value)
})

test_that("dispatch() sends a rejected promise as an error response", {
  FailingSession <- R6::R6Class("FailingSession",
    inherit = ShinySession,
    public = list(
      `@failLater` = function() promises::promise_reject(simpleError("boom"))
    )
  )
  sent <- list()
  session <- FailingSession$new(list(
    request = list(),
    send = function(json) sent[[length(sent) + 1]] <<- jsonlite::fromJSON(json),
    close = function() invisible(NULL),
    onMessage = function(f) invisible(NULL),
    onClose = function(f) invisible(NULL)
  ))

  session$dispatch(list(method = "failLater", args = list(), tag = 8))
  for (i in 1:5) later::run_now()

  response <- Filter(function(m) !is.null(m$response), sent)[[1]]$response
  expect_equal(response$tag, 8)
  expect_equal(response$error, "boom")
})
