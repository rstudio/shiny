test_that("startHttpuvApp() announces the URL only after the socket is bound (#4400)", {
  # The "Listening on ..." message is used as a readiness signal (e.g. by
  # shinytest2 and log watchers), so it must not be emitted until httpuv has
  # actually bound the listening socket. Here we mock the (synchronous) bind and
  # confirm the message is printed *after* it returns, never before.
  withr::defer(handlerManager$clear())

  app <- shinyApp(fluidPage(), function(input, output, session) {})

  bound <- FALSE
  bound_when_announced <- NULL

  # Replace httpuv's startServer so no real socket is opened; record the bind.
  local_mocked_bindings(
    startServer = function(host, port, app, ...) {
      bound <<- TRUE
      structure(list(), class = "mock_server")
    }
  )

  server <- withCallingHandlers(
    startHttpuvApp(app, port = 3839L, host = "127.0.0.1", quiet = FALSE),
    message = function(m) {
      if (grepl("Listening on", conditionMessage(m), fixed = TRUE)) {
        bound_when_announced <<- bound
      }
      invokeRestart("muffleMessage")
    }
  )

  # The bind was attempted and its return value is passed through.
  expect_true(bound)
  expect_s3_class(server, "mock_server")

  # The "Listening on" line was seen, and only after the bind had returned.
  expect_true(isTRUE(bound_when_announced))
})

test_that("startHttpuvApp() with quiet = TRUE binds without announcing (#4400)", {
  withr::defer(handlerManager$clear())

  app <- shinyApp(fluidPage(), function(input, output, session) {})

  bound <- FALSE
  local_mocked_bindings(
    startServer = function(host, port, app, ...) {
      bound <<- TRUE
      structure(list(), class = "mock_server")
    }
  )

  expect_no_message(
    server <- startHttpuvApp(app, port = 3839L, host = "127.0.0.1", quiet = TRUE)
  )
  expect_true(bound)
  expect_s3_class(server, "mock_server")
})

test_that("flushPendingSessions() closes a session whose flush errors (#4439)", {
  closed <- FALSE
  session <- list(
    flushOutput = function() stop("boom"),
    close = function() closed <<- TRUE
  )
  appsNeedingFlush$set("test-flush-error", session)
  withr::defer(appsNeedingFlush$remove("test-flush-error"))

  expect_no_error(flushPendingSessions())
  expect_true(closed)
})

test_that("ShinySession$dispatch() reports an unknown method once (#4439)", {
  responses <- list()
  env <- new.env()
  env$self <- list(`@known` = function(x) x * 2)
  env$private <- list(
    sendErrorResponse = function(msg, error) {
      responses[[length(responses) + 1]] <<- list(error = error)
    },
    sendResponse = function(msg, value) {
      responses[[length(responses) + 1]] <<- list(value = value)
    }
  )
  dispatch <- ShinySession$public_methods$dispatch
  environment(dispatch) <- env

  dispatch(list(method = "nope", args = list(), tag = 1))
  expect_equal(responses, list(list(error = "Unknown method nope")))

  responses <- list()
  dispatch(list(method = "known", args = list(3), tag = 2))
  expect_equal(responses, list(list(value = 6)))
})
