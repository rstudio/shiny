skip_if_not_shinytest2()
skip_if_not_installed("ellmer")
skip_if_not_installed("S7")
library(shinytest2)

# library(shiny) must be called explicitly inside the function so that
# shinytest2's library shim loads the dev package via pkgload::load_all().
app <- launch_test_app(function() {
  library(shiny)
  options(shiny.webmcp = TRUE)

  # Outside the server function: for every session
  registerWebMcpTool(webMcpTool(
    "summarize",
    function(column) {
      list(column = column, mean = mean(mtcars[[column]]))
    },
    "Mean of one mtcars column.",
    arguments = list(column = ellmer::type_enum(c("mpg", "hp"))),
    value = ellmer::type_object(
      column = ellmer::type_string(),
      mean = ellmer::type_number()
    ),
    annotations = list(readOnlyHint = TRUE)
  ))

  counterServer <- function(id) {
    moduleServer(id, function(input, output, session) {
      # Inside a module: this session only, namespaced. `input` and `session`
      # are filled in with the module's.
      registerWebMcpTool(webMcpTool(
        "add",
        function(by, input, session) {
          updateNumericInput(session, "n", value = input$n + by)
          paste("Added", by)
        },
        "Add to the counter.",
        arguments = list(by = ellmer::type_integer())
      ))
    })
  }

  ui <- fluidPage(numericInput("counter-n", "n", 1), textOutput("label"))
  server <- function(input, output, session) {
    counterServer("counter")

    # A tool changes what the page shows by setting a reactive value
    label <- reactiveVal("start")
    output$label <- renderText(label())
    registerWebMcpTool(webMcpTool(
      "set_label",
      function(text) {
        label(text)
        "done"
      },
      "Set the label.",
      arguments = list(text = ellmer::type_string())
    ))
  }

  shinyApp(ui, server)
})

app$wait_for_js("Shiny.webmcp.listTools().some((t) => t.name === 'counter-add')")

test_that("R tools are listed with their schemas", {
  tools <- app$get_js(
    "Object.fromEntries(Shiny.webmcp.listTools().map((t) => [t.name, t]))"
  )
  expect_setequal(
    names(tools),
    c("list_inputs", "set_input", "list_outputs", "get_output", "summarize", "counter-add", "set_label")
  )
  expect_equal(tools$summarize$inputSchema$properties$column$enum, list("mpg", "hp"))
  expect_equal(tools$summarize$outputSchema$required, list("column", "mean"))
  expect_true(tools$summarize$annotations$readOnlyHint)
  expect_equal(tools$`counter-add`$inputSchema$required, list("by"))
})

test_that("calling an R tool runs it on the server", {
  res <- app$get_js("Shiny.webmcp.callTool('summarize', { column: 'hp' })")
  expect_null(res$isError)
  expect_equal(res$structuredContent, list(column = "hp", mean = mean(mtcars$hp)))

  res <- app$get_js("Shiny.webmcp.callTool('counter-add', { by: 2 })")
  expect_equal(res$content[[1]]$text, "Added 2")
  app$wait_for_idle()
  expect_equal(app$get_value(input = "counter-n"), 3)
})

test_that("a tool that sets a reactive value updates the page", {
  expect_equal(app$get_value(output = "label"), "start")
  res <- app$get_js("Shiny.webmcp.callTool('set_label', { text: 'from agent' })")
  expect_equal(res$content[[1]]$text, "done")
  app$wait_for_idle()
  expect_equal(app$get_value(output = "label"), "from agent")
})

test_that("bad arguments come back as errors", {
  res <- app$get_js("Shiny.webmcp.callTool('summarize', { column: 'cyl' })")
  expect_true(res$isError)
  expect_match(res$content[[1]]$text, 'Invalid input: `column` must be one of "mpg", "hp"')

  # Unknown keys are caught in the browser, before reaching R
  res <- app$get_js("Shiny.webmcp.callTool('counter-add', { by: 1, extra: 1 })")
  expect_true(res$isError)
  expect_match(res$content[[1]]$text, 'unknown parameter "extra"', fixed = TRUE)
})
