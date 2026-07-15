test_that("registerMcpTool() stores ellmer tools by name (last-write-wins)", {
  skip_if_not_installed("ellmer")
  local_mcp_tools()

  registerMcpTool(
    ellmer::tool(function(n) n, name = "echo_n",
      description = "Echo n.",
      arguments = list(n = ellmer::type_integer("n", required = TRUE)))
  )
  expect_named(mcpAuthorTools(), "echo_n")
  expect_true(inherits(mcpAuthorTools()[["echo_n"]], "ellmer::ToolDef"))

  # Re-registering the same name replaces, does not error or duplicate.
  registerMcpTool(
    ellmer::tool(function(n) n * 2, name = "echo_n",
      description = "Echo 2n.",
      arguments = list(n = ellmer::type_integer("n", required = TRUE)))
  )
  expect_length(mcpAuthorTools(), 1)
  expect_equal(mcpAuthorTools()[["echo_n"]]@description, "Echo 2n.")
})

test_that("registerMcpTool() accepts multiple tools in one call", {
  skip_if_not_installed("ellmer")
  local_mcp_tools()
  registerMcpTool(
    ellmer::tool(function() 1, name = "a", description = "a"),
    ellmer::tool(function() 2, name = "b", description = "b")
  )
  expect_setequal(names(mcpAuthorTools()), c("a", "b"))
})

test_that("registerMcpTool() rejects non-ToolDef arguments", {
  skip_if_not_installed("ellmer")
  local_mcp_tools()
  expect_error(
    registerMcpTool(list(name = "x", handler = function(args) 1)),
    "ellmer::tool"
  )
})

test_that("registerMcpTool() rejects reserved tool names", {
  skip_if_not_installed("ellmer")
  local_mcp_tools()
  expect_error(
    registerMcpTool(
      ellmer::tool(function() 1, name = "_shiny_send", description = "no")
    ),
    "reserved"
  )
})
