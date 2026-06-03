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
  withr::with_options(list(), {
    shiny::shinyOptions(hardDisconnectMessage = "Bye!")
    ws <- FakeWebSocket$new()
    session <- ShinySession$new(ws)
    expect_identical(
      session$.__enclos_env__$private$hardDisconnectMessage,
      "Bye!"
    )
    expect_false(session$.__enclos_env__$private$wasHardClose)
  })
})

test_that("ShinySession defaults hardDisconnectMessage to NULL", {
  withr::with_options(list(), {
    # Clear any leftover option from a previous test
    shiny::shinyOptions(hardDisconnectMessage = NULL)
    ws <- FakeWebSocket$new()
    session <- ShinySession$new(ws)
    expect_null(session$.__enclos_env__$private$hardDisconnectMessage)
  })
})
