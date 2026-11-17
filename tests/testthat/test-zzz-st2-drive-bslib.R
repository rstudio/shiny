skip_if_not_shinytest2()
library(shinytest2)

# library(shiny) must be called explicitly inside the function so that
# shinytest2's library shim loads the dev package via pkgload::load_all().
app <- launch_test_app(function() {
  library(shiny)
  library(bslib)

  ui <- page_sidebar(
    sidebar = sidebar(id = "sb", "Sidebar body"),
    accordion(
      id = "acc",
      accordion_panel("Summary", value = "summary", "Summary body"),
      accordion_panel("Details", value = "details", "Details body")
    ),
    accordion(
      id = "one",
      multiple = FALSE,
      accordion_panel("A", value = "a", "A body"),
      accordion_panel("B", value = "b", "B body")
    ),
    verbatimTextOutput("acc_value"),
    verbatimTextOutput("sb_value")
  )

  server <- function(input, output) {
    output$acc_value <- renderPrint(input$acc)
    output$sb_value <- renderPrint(input$sb)
  }

  shinyApp(ui, server)
})

drive <- function(call) {
  app$get_js(paste0(
    "(async () => { try { return await ", call, "; }",
    " catch (e) { return { error: e.message }; } })()"
  ))
}

test_that("accordion schema lists its panels", {
  info <- drive("Shiny.drive.getInput('acc')")
  expect_equal(info$schema$type, "array")
  expect_equal(unlist(info$schema$items$enum), c("summary", "details"))
  expect_null(info$schema$maxItems)

  one <- drive("Shiny.drive.getInput('one')")
  expect_equal(one$schema$maxItems, 1)
})

test_that("setInput() opens exactly the given accordion panels", {
  info <- drive("Shiny.drive.setInput('acc', ['details'])")
  expect_equal(unlist(info$value), "details")
  expect_equal(app$get_value(input = "acc"), "details")
  expect_match(
    drive("Shiny.drive.getOutput('acc_value')")$text,
    "details"
  )

  info <- drive("Shiny.drive.setInput('acc', ['summary', 'details'])")
  expect_equal(unlist(info$value), c("summary", "details"))

  info <- drive("Shiny.drive.setInput('acc', 'nope')")
  expect_match(info$error, "must be an array")
  info <- drive("Shiny.drive.setInput('one', ['a', 'b'])")
  expect_match(info$error, "items")
})

test_that("setInput() opens and closes a sidebar, whatever its state", {
  # Setting the current value leaves it as is (it used to toggle)
  info <- drive("Shiny.drive.setInput('sb', true)")
  expect_true(info$value)
  expect_true(app$get_value(input = "sb"))

  info <- drive("Shiny.drive.setInput('sb', false)")
  expect_false(info$value)
  expect_null(info$hidden)
  expect_false(app$get_value(input = "sb"))

  # Still listed and settable once closed
  info <- drive("Shiny.drive.setInput('sb', false)")
  expect_false(info$value)
  info <- drive("Shiny.drive.setInput('sb', true)")
  expect_true(info$value)
  expect_true(app$get_value(input = "sb"))
})
