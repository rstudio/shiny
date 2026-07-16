# tests/testthat/test-mcp-config.R
test_that("mcpConfigure() stores a defaulted, enabled config", {
  withr::defer(.globals$mcp <- NULL)
  mcpConfigure()
  expect_true(mcpEnabled())
  expect_equal(.globals$mcp$displayModes, c("inline", "fullscreen", "pip"))
  expect_true(.globals$mcp$direct)
  expect_false(.globals$mcp$stdio)
})

test_that("no mcpConfigure() call means MCP is disabled", {
  withr::defer(.globals$mcp <- NULL)
  .globals$mcp <- NULL
  expect_false(mcpEnabled())
})

test_that("mcpConfigure(enabled = FALSE) configures but disables", {
  withr::defer(.globals$mcp <- NULL)
  mcpConfigure(appId = "demo", enabled = FALSE)
  expect_false(mcpEnabled())
  expect_equal(.globals$mcp$appId, "demo")
})

test_that("invalid appId errors", {
  withr::defer(.globals$mcp <- NULL)
  expect_error(mcpConfigure(appId = "not ok!"), "appId")
  expect_error(mcpConfigure(appId = c("a", "b")), "appId")
})

test_that("displayModes are intersected and fall back to inline", {
  withr::defer(.globals$mcp <- NULL)
  mcpConfigure(displayModes = c("fullscreen", "bogus"))
  expect_equal(.globals$mcp$displayModes, "fullscreen")
  mcpConfigure(displayModes = "bogus")
  expect_equal(.globals$mcp$displayModes, "inline")
})

test_that("scalar-logical and string args are asserted", {
  withr::defer(.globals$mcp <- NULL)
  expect_error(mcpConfigure(direct = "yes"), "direct")
  expect_error(mcpConfigure(enabled = NA), "enabled")
  expect_error(mcpConfigure(description = c("a", "b")), "description")
})

test_that("arguments must be a named list of ellmer types", {
  skip_if_not_installed("ellmer")
  withr::defer(.globals$mcp <- NULL)
  expect_error(mcpConfigure(arguments = list(ellmer::type_integer("x"))), "named")
  expect_error(mcpConfigure(arguments = list(n = 1L)), "ellmer")
  mcpConfigure(arguments = list(n = ellmer::type_integer("bins")))
  expect_named(.globals$mcp$arguments, "n")
})

test_that("accessors read the stored config", {
  withr::defer(.globals$mcp <- NULL)
  mcpConfigure(
    appId = "cars", description = "Show the cars app.",
    direct = FALSE, displayModes = "inline", origin = "https://x.example/app",
    stdio = TRUE
  )
  expect_true(mcpEnabled())
  expect_true(mcpStdioEnabled())
  expect_equal(mcpAppId(), "cars")
  expect_false(mcpDirectEnabled())
  expect_equal(mcpDisplayModes(), "inline")
  info <- mcpToolInfo()
  expect_equal(info$name, "open_cars_app")
  expect_equal(info$description, "Show the cars app.")
})

test_that("only declared arguments are kept", {
  skip_if_not_installed("ellmer")
  withr::defer(.globals$mcp <- NULL)
  mcpConfigure(arguments = list(n = ellmer::type_integer("bins")))
  expect_equal(mcpFilterArguments(list(n = 5, evil = "x")), list(n = 5))
})

test_that("with no declared arguments nothing passes the filter", {
  withr::defer(.globals$mcp <- NULL)
  mcpConfigure()
  expect_equal(mcpFilterArguments(list(n = 5)), list())
})

test_that("mcpToolInfo() falls back to defaults and builds schema", {
  skip_if_not_installed("ellmer")
  withr::defer(.globals$mcp <- NULL)
  mcpConfigure(arguments = list(n = ellmer::type_integer("bins")))
  info <- mcpToolInfo()
  expect_equal(info$name, "open_shiny_app")
  expect_match(info$description, "interactive Shiny application")
  expect_equal(info$inputSchema$type, "object")
  expect_true("n" %in% names(info$inputSchema$properties))
})
