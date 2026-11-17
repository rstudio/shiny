skip_if_not_shinytest2()
library(shinytest2)

# library(shiny) must be called explicitly inside the function so that
# shinytest2's library shim loads the dev package via pkgload::load_all().
app <- launch_test_app(function() {
  library(shiny)
  options(shiny.webmcp = TRUE, shiny.webmcp.polyfill = FALSE)

  ui <- fluidPage(
    tabsetPanel(
      id = "tab",
      tabPanel("A", textInput("shown", "Shown"), textOutput("out_a")),
      tabPanel("B", textInput("hidden", "Hidden"), textOutput("out_b"))
    )
  )

  server <- function(input, output) {
    output$out_a <- renderText("a")
    output$out_b <- renderText("b")
  }

  shinyApp(ui, server)
})

tool <- function(name, args) {
  app$get_js(paste0(
    "Shiny.webmcp.callTool('", name, "', ",
    jsonlite::toJSON(args, auto_unbox = TRUE), ")"
  ))
}
ids <- function(rows) vapply(rows, `[[`, "", "id")

test_that("the polyfill can be off while the tools stay on", {
  expect_false(app$get_js("!!document.querySelector('script[src*=\"mcp-b\"]')"))
  tools <- app$get_js("Shiny.webmcp.listTools().map(t => t.name)")
  expect_true("list_inputs" %in% unlist(tools))
})

test_that("list tools mark hidden items only when asked", {
  res <- tool("list_inputs", list(module = NULL))
  expect_false("hidden" %in% ids(res$structuredContent$inputs))

  res <- tool("list_inputs", list(includeHidden = TRUE))
  rows <- res$structuredContent$inputs
  expect_true("hidden" %in% ids(rows))
  expect_true(rows[[which(ids(rows) == "hidden")]]$hidden)
  expect_null(rows[[which(ids(rows) == "shown")]]$hidden)

  res <- tool("list_outputs", list(includeHidden = TRUE))
  rows <- res$structuredContent$outputs
  expect_true(rows[[which(ids(rows) == "out_b")]]$hidden)
})

test_that("hidden items still can't be set or read through the tools", {
  res <- tool("set_input", list(id = "hidden", value = "x"))
  expect_true(res$isError)
  res <- tool("get_output", list(id = "out_b"))
  expect_true(res$isError)
})

test_that("includeHidden must be a boolean", {
  res <- tool("list_inputs", list(includeHidden = "yes"))
  expect_true(res$isError)
})
