test_that("webMcpAccess() sets, replaces, and validates data-shiny-drive-access", {
  tag <- textInput("x", "X") |> webMcpAccess("read-only")
  expect_equal(tag$attribs[["data-shiny-drive-access"]], "read-only")

  tag <- webMcpAccess(tag, "exclude")
  expect_equal(sum(names(tag$attribs) == "data-shiny-drive-access"), 1)
  expect_equal(tag$attribs[["data-shiny-drive-access"]], "exclude")

  expect_error(webMcpAccess(tag, "nope"))
  expect_error(webMcpAccess(tagList(tag), "exclude"), "single HTML tag")
})

test_that("webMcpAccess() puts the attribute on a tabset's bound tab input", {
  tag <- tabsetPanel(id = "tab", tabPanel("A"), tabPanel("B")) |>
    webMcpAccess("read-only") |>
    webMcpAccess("exclude")

  expect_null(tag$attribs[["data-shiny-drive-access"]])
  ul <- tagQuery(tag)$find(".shiny-tab-input")$selectedTags()[[1]]
  expect_equal(sum(names(ul$attribs) == "data-shiny-drive-access"), 1)
  expect_equal(ul$attribs[["data-shiny-drive-access"]], "exclude")
})

test_that("the WebMCP polyfill is added only when shiny.webmcp is TRUE", {
  ui <- fluidPage("hi")
  has_polyfill <- function() {
    grepl("@mcp-b/global@", as.character(renderPage(ui)), fixed = TRUE)
  }

  withr::with_options(list(shiny.webmcp = TRUE), expect_true(has_polyfill()))
  withr::with_options(list(shiny.webmcp = NULL), expect_false(has_polyfill()))
})
