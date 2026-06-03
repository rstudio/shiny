test_that("shinyApp() captures hardDisconnectMessage into appOptions", {
  app <- shinyApp(
    ui = shiny::tagList(),
    server = function(input, output, session) {},
    hardDisconnectMessage = "Thanks for using the app."
  )
  expect_identical(
    app$appOptions$hardDisconnectMessage,
    "Thanks for using the app."
  )
})

test_that("shinyApp() defaults hardDisconnectMessage to NULL", {
  app <- shinyApp(
    ui = shiny::tagList(),
    server = function(input, output, session) {}
  )
  expect_null(app$appOptions$hardDisconnectMessage)
})

test_that("ShinySession reads hardDisconnectMessage from shinyOptions at init", {
  shiny::shinyOptions(hardDisconnectMessage = "Bye!")
  withr::defer(shiny::shinyOptions(hardDisconnectMessage = NULL))
  ws <- FakeWebSocket$new()
  session <- ShinySession$new(ws)
  expect_identical(
    session$.__enclos_env__$private$hardDisconnectMessage,
    "Bye!"
  )
  expect_false(session$.__enclos_env__$private$wasHardClose)
})

test_that("ShinySession defaults hardDisconnectMessage to NULL", {
  # Clear any leftover option from a previous test
  shiny::shinyOptions(hardDisconnectMessage = NULL)
  withr::defer(shiny::shinyOptions(hardDisconnectMessage = NULL))
  ws <- FakeWebSocket$new()
  session <- ShinySession$new(ws)
  expect_null(session$.__enclos_env__$private$hardDisconnectMessage)
})

test_that("session$close() with no args performs a soft close (backward compatible)", {
  ws <- FakeWebSocket$new()
  session <- ShinySession$new(ws)

  session$close()

  expect_true(ws$closeCalled)
  expect_null(ws$closeCode)
  expect_null(ws$closeReason)
  expect_false(session$.__enclos_env__$private$wasHardClose)
  # No hardDisconnectConfig should have been sent
  expect_length(fake_ws_messages_with(ws, "hardDisconnectConfig"), 0L)
})

test_that("session$close(hard = TRUE) sends hardDisconnectConfig and uses code 4001", {
  shiny::shinyOptions(hardDisconnectMessage = NULL)
  withr::defer(shiny::shinyOptions(hardDisconnectMessage = NULL))
  ws <- FakeWebSocket$new()
  session <- ShinySession$new(ws)

  session$close(hard = TRUE, message = "All done.")

  # hardDisconnectConfig sent before close
  configs <- fake_ws_messages_with(ws, "hardDisconnectConfig")
  expect_length(configs, 1L)
  expect_identical(
    configs[[1L]]$hardDisconnectConfig$message,
    "All done."
  )

  # Close code 4001 with reason
  expect_true(ws$closeCalled)
  expect_identical(ws$closeCode, 4001L)
  expect_identical(ws$closeReason, "shiny-hard-disconnect")
  expect_true(session$.__enclos_env__$private$wasHardClose)
})

test_that("session$close(hard = TRUE) falls back to hardDisconnectMessage when message is NULL", {
  shiny::shinyOptions(hardDisconnectMessage = "App default text")
  withr::defer(shiny::shinyOptions(hardDisconnectMessage = NULL))
  ws <- FakeWebSocket$new()
  session <- ShinySession$new(ws)

  session$close(hard = TRUE)

  configs <- fake_ws_messages_with(ws, "hardDisconnectConfig")
  expect_identical(
    configs[[1L]]$hardDisconnectConfig$message,
    "App default text"
  )
})

test_that("session$close(hard = TRUE) uses framework default when no message is set anywhere", {
  shiny::shinyOptions(hardDisconnectMessage = NULL)
  withr::defer(shiny::shinyOptions(hardDisconnectMessage = NULL))
  ws <- FakeWebSocket$new()
  session <- ShinySession$new(ws)

  session$close(hard = TRUE)

  configs <- fake_ws_messages_with(ws, "hardDisconnectConfig")
  expect_identical(
    configs[[1L]]$hardDisconnectConfig$message,
    "This app has closed."
  )
})

test_that("wsClosed after soft close leaves root Maps in place", {
  ws <- FakeWebSocket$new()
  session <- ShinySession$new(ws)

  # Populate Maps with sentinel entries so we can detect cleanup
  session$files$set("file1", list())
  session$downloads$set("dl1", list())

  session$close()
  session$wsClosed()

  expect_true(session$files$containsKey("file1"))
  expect_true(session$downloads$containsKey("dl1"))
})

test_that("wsClosed after hard close clears root inputs, clientData, downloads, files", {
  ws <- FakeWebSocket$new()
  session <- ShinySession$new(ws)

  session$.__enclos_env__$private$.input$set("test_input", 42)
  session$.__enclos_env__$private$.clientData$set("test_cd", "hello")
  session$files$set("file1", list())
  session$downloads$set("dl1", list())

  session$close(hard = TRUE, message = "bye")
  session$wsClosed()

  expect_false(session$files$containsKey("file1"))
  expect_false(session$downloads$containsKey("dl1"))
  expect_length(isolate(session$.__enclos_env__$private$.input$names()), 0L)
  expect_length(isolate(session$.__enclos_env__$private$.clientData$names()), 0L)
})

test_that("onSessionEnded runs before hard cleanup (callback sees live state)", {
  ws <- FakeWebSocket$new()
  session <- ShinySession$new(ws)

  session$files$set("sentinel", list(payload = "still here"))

  observedFiles <- NULL
  session$onSessionEnded(function() {
    # At the moment this fires, files should still be populated.
    observedFiles <<- session$files$keys()
  })

  session$close(hard = TRUE)
  session$wsClosed()

  expect_true("sentinel" %in% observedFiles)
  # And after wsClosed completes, the file is cleaned up.
  expect_false(session$files$containsKey("sentinel"))
})
