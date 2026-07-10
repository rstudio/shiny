test_that("mcpHttpHandler ignores other paths and handles OPTIONS/GET", {
  h <- mcpHttpHandler(function(req) NULL, function(ws) TRUE)
  expect_null(h(mcp_req(NULL, method = "GET", path = "/other")))

  opts <- h(mcp_req(NULL, method = "OPTIONS"))
  expect_equal(opts$status, 204L)
  expect_equal(opts$headers[["Access-Control-Allow-Origin"]], "*")

  get <- h(mcp_req(NULL, method = "GET"))
  expect_equal(get$status, 405L)
})

test_that("initialize negotiates protocol version", {
  h <- mcpHttpHandler(function(req) NULL, function(ws) TRUE)

  out <- mcp_post(
    h, "initialize",
    params = list(protocolVersion = "2025-06-18", capabilities = empty_named_list())
  )
  expect_equal(out$result$protocolVersion, "2025-06-18")
  expect_equal(out$result$serverInfo$name, "shiny")
  expect_true(is.list(out$result$capabilities$tools))

  out2 <- mcp_post(
    h, "initialize",
    params = list(protocolVersion = "9999-01-01", capabilities = empty_named_list())
  )
  expect_equal(out2$result$protocolVersion, "2025-06-18")
})

test_that("notifications get 202, unknown methods get -32601, ping works", {
  h <- mcpHttpHandler(function(req) NULL, function(ws) TRUE)

  notif <- h(mcp_req(list(jsonrpc = "2.0", method = "notifications/initialized")))
  expect_equal(notif$status, 202L)

  out <- mcp_post(h, "bogus/method")
  expect_equal(out$error$code, -32601L)

  out <- mcp_post(h, "ping")
  expect_true(is.list(out$result))
  expect_length(out$result, 0)
})

test_that("invalid JSON gets a parse error", {
  h <- mcpHttpHandler(function(req) NULL, function(ws) TRUE)
  req <- mcp_req(NULL)
  req$rook.input <- fakeRookInput(charToRaw("not json"))
  resp <- h(req)
  expect_equal(resp$status, 400L)
  out <- jsonlite::parse_json(rawToChar(as_bytes(resp$content)))
  expect_equal(out$error$code, -32700L)
})

test_that("tools/list includes the visible app tool with ui metadata", {
  h <- mcpHttpHandler(function(req) NULL, function(ws) TRUE)
  out <- mcp_post(h, "tools/list")
  tools <- out$result$tools
  tool_names <- vapply(tools, function(t) t$name, character(1))
  expect_true("open_shiny_app" %in% tool_names)
  app_tool <- tools[[which(tool_names == "open_shiny_app")]]
  expect_equal(app_tool$`_meta`$ui$resourceUri, "ui://shiny/app")
})

test_that("tool name and description are configurable", {
  withr::with_options(
    list(shiny.mcp.tool = list(name = "show_dashboard", description = "Show it")),
    {
      h <- mcpHttpHandler(function(req) NULL, function(ws) TRUE)
      out <- mcp_post(h, "tools/list")
      tool_names <- vapply(out$result$tools, function(t) t$name, character(1))
      expect_true("show_dashboard" %in% tool_names)

      called <- mcp_post(h, "tools/call",
        params = list(name = "show_dashboard", arguments = empty_named_list())
      )
      expect_match(called$result$content[[1]]$text, "displayed")
    }
  )
})

test_that("createAppHandlers mounts /mcp only when enabled", {
  ui <- fluidPage("hello mcp")
  app <- shinyApp(ui, function(input, output) {})
  make_handler <- function() {
    h <- createAppHandlers(app$httpHandler, function() function(input, output) {})
    h$http
  }
  withr::with_options(list(shiny.mcp = TRUE), {
    resp <- wait_for_result(make_handler()(mcp_req(
      list(jsonrpc = "2.0", id = 1, method = "ping")
    )))
    expect_equal(resp$status, 200L)
  })
  withr::with_options(list(shiny.mcp = FALSE), {
    expect_null(make_handler()(mcp_req(
      list(jsonrpc = "2.0", id = 1, method = "ping")
    )))
  })
})
