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
