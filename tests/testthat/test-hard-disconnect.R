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
