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
