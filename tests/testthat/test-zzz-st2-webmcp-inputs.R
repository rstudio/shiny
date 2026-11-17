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
      tabPanel(
        "A",
        textInput("shown", "Shown"),
        selectInput("pick", "Pick", c("a", "b")),
        actionButton("go", "Go"),
        textOutput("out_a")
      ),
      tabPanel("B", textInput("hidden", "Hidden"), textOutput("out_b"))
    )
  )

  server <- function(input, output) {
    output$out_a <- renderText(paste(input$shown, input$pick, input$go))
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
  res <- tool("set_inputs", list(inputs = list(id = "hidden", value = "x")))
  expect_true(res$isError)
  res <- tool("get_output", list(id = "out_b"))
  expect_true(res$isError)
})

test_that("includeHidden must be a boolean", {
  res <- tool("list_inputs", list(includeHidden = "yes"))
  expect_true(res$isError)
})

test_that("set_inputs takes one input or a list, and changes all or nothing", {
  states <- function(res) res$structuredContent$inputs
  out_a <- function() {
    tool("get_output", list(id = "out_a"))$structuredContent$text
  }

  # One input, not in a list
  res <- tool("set_inputs", list(inputs = list(id = "shown", value = "hi")))
  expect_null(res$isError)
  expect_equal(states(res)[[1]]$value, "hi")

  # A list, with a button (no value), in one update
  res <- tool("set_inputs", list(inputs = list(
    list(id = "shown", value = "yo"),
    list(id = "pick", value = "b"),
    list(id = "go")
  )))
  expect_equal(ids(states(res)), c("shown", "pick", "go"))
  expect_equal(out_a(), "yo b 1")

  # One bad value changes nothing
  res <- tool("set_inputs", list(inputs = list(
    list(id = "shown", value = "nope"),
    list(id = "pick", value = "z")
  )))
  expect_true(res$isError)
  expect_match(res$content[[1]]$text, "must be one of")
  expect_equal(out_a(), "yo b 1")

  expect_match(
    tool("set_inputs", list(inputs = list(
      list(id = "shown", value = "a"),
      list(id = "shown", value = "b")
    )))$content[[1]]$text,
    "more than once"
  )
  expect_match(
    tool("set_inputs", list(inputs = list(id = "shown", val = "a")))$content[[1]]$text,
    "unknown keys: val"
  )
  expect_match(
    tool("set_inputs", list(inputs = list()))$content[[1]]$text,
    "empty"
  )
  expect_match(
    tool("set_inputs", list(id = "shown"))$content[[1]]$text,
    'missing required parameter "inputs"'
  )
})
