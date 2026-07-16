# Helper: create a tunneled session running `serverFunc` and return the
# connection id + ws handlers for driving it.
mcp_start_session <- function(ui, serverFunc, initData = list()) {
  app <- shinyApp(ui, serverFunc)
  handlers <- createAppHandlers(app$httpHandler, function() serverFunc)
  con_res <- mcpTunnelToolCall("_shiny_connect", empty_named_list(), 1, handlers$ws)
  cid <- con_res$result$structuredContent$connectionId

  init <- as.character(jsonlite::toJSON(
    list(method = "init", data = c(list(.clientdata_url_search = ""), initData)),
    auto_unbox = TRUE
  ))
  mcpTunnelToolCall(
    "_shiny_send",
    list(connectionId = cid, frames = list(list(data = init, binary = FALSE))),
    2, handlers$ws
  )
  list(cid = cid, handlers = handlers)
}

mcp_send_update <- function(sess, data) {
  msg <- as.character(jsonlite::toJSON(
    list(method = "update", data = data),
    auto_unbox = TRUE
  ))
  mcpTunnelToolCall(
    "_shiny_send",
    list(connectionId = sess$cid, frames = list(list(data = msg, binary = FALSE))),
    3, sess$handlers$ws
  )
}

mcp_drain_frames <- function(sess, timeout = 5) {
  frames <- character(0)
  end <- Sys.time() + timeout
  while (Sys.time() < end) {
    pump_shiny(0.05)
    rec <- wait_for_result(mcpTunnelToolCall(
      "_shiny_receive", list(connectionId = sess$cid, timeoutSecs = 0.1),
      4, sess$handlers$ws
    ))
    got <- vapply(
      rec$result$structuredContent$frames,
      function(f) f$data, character(1)
    )
    frames <- c(frames, got)
    if (length(got) == 0 && length(frames) > 0) break
  }
  frames
}

test_that("isMcpSession distinguishes tunnel sessions from others", {
  seen <- NULL
  sess <- mcp_start_session(
    fluidPage(),
    function(input, output, session) {
      seen <<- isMcpSession(session)
    }
  )
  pump_shiny(0.1)
  expect_true(seen)
  expect_false(isMcpSession(MockShinySession$new()))
  mcpTunnelToolCall("_shiny_close", list(connectionId = sess$cid), 9, sess$handlers$ws)
})

test_that("mcpUpdates returns parsed tool arguments reactively", {
  observed <- list()
  sess <- mcp_start_session(
    fluidPage(),
    function(input, output, session) {
      observe({
        val <- mcpUpdates()
        if (!is.null(val)) observed[[length(observed) + 1]] <<- val
      })
    }
  )
  pump_shiny(0.2)
  expect_length(observed, 0)

  mcp_send_update(sess, list(
    .clientdata_mcp_tool_input = '{"note":"hello","n":3}'
  ))
  pump_shiny(0.3)
  expect_gt(length(observed), 0)
  latest <- observed[[length(observed)]]
  expect_equal(latest$note, "hello")
  expect_equal(latest$n, 3)
  mcpTunnelToolCall("_shiny_close", list(connectionId = sess$cid), 9, sess$handlers$ws)
})

test_that("mcpHostContext returns parsed host context", {
  ctx <- NULL
  sess <- mcp_start_session(
    fluidPage(),
    function(input, output, session) {
      observe({
        ctx <<- mcpHostContext()
      })
    }
  )
  mcp_send_update(sess, list(
    .clientdata_mcp_host_context = '{"theme":"dark","locale":"en-US"}'
  ))
  pump_shiny(0.3)
  expect_equal(ctx$theme, "dark")
  mcpTunnelToolCall("_shiny_close", list(connectionId = sess$cid), 9, sess$handlers$ws)
})

test_that("mcpUpdateModelContext and mcpSendMessage send bridge messages", {
  sess <- mcp_start_session(
    fluidPage(),
    function(input, output, session) {
      observeEvent(input$go, {
        mcpUpdateModelContext(
          text = "n is 42",
          data = list(n = 42)
        )
        mcpSendMessage("What does this mean?")
      })
    },
    initData = list(go = 0)
  )
  mcp_drain_frames(sess) # flush startup frames
  mcp_send_update(sess, list(go = 1))
  frames <- paste(mcp_drain_frames(sess), collapse = "\n")

  expect_match(frames, "shiny.mcp.updateModelContext", fixed = TRUE)
  expect_match(frames, "n is 42", fixed = TRUE)
  expect_match(frames, "structuredContent", fixed = TRUE)
  expect_match(frames, "shiny.mcp.sendMessage", fixed = TRUE)
  expect_match(frames, "What does this mean?", fixed = TRUE)
  mcpTunnelToolCall("_shiny_close", list(connectionId = sess$cid), 9, sess$handlers$ws)
})

test_that("mcpUpdateModelContext validates input and no-ops outside MCP", {
  mock <- MockShinySession$new()
  expect_false(mcpUpdateModelContext(text = "x", session = mock))
  expect_false(mcpSendMessage("x", session = mock))
  expect_error(mcpUpdateModelContext(session = mock), "text.*data|data.*text")
})

test_that("tool inputSchema is configurable via arguments", {
  skip_if_not_installed("ellmer")
  local_mcp_config(
    arguments = list(note = ellmer::type_string("A note", required = TRUE))
  )
  h <- mcpHttpHandler(function(req) NULL, function(ws) TRUE)
  out <- mcp_post(h, "tools/list")
  tool_names <- vapply(out$result$tools, function(t) t$name, character(1))
  app_tool <- out$result$tools[[which(tool_names == "open_shiny_app")]]
  expect_equal(app_tool$inputSchema$properties$note$type, "string")
  expect_equal(unlist(app_tool$inputSchema$required), "note")
})

test_that("mcpRequestDisplayMode sends the bridge message", {
  sess <- mcp_start_session(
    fluidPage(),
    function(input, output, session) {
      observeEvent(input$go, {
        mcpRequestDisplayMode("fullscreen")
      })
    },
    initData = list(go = 0)
  )
  mcp_drain_frames(sess)
  mcp_send_update(sess, list(go = 1))
  frames <- paste(mcp_drain_frames(sess), collapse = "\n")
  expect_match(frames, "shiny.mcp.requestDisplayMode", fixed = TRUE)
  expect_match(frames, "fullscreen", fixed = TRUE)
  mcpTunnelToolCall("_shiny_close", list(connectionId = sess$cid), 9, sess$handlers$ws)

  expect_false(mcpRequestDisplayMode("pip", session = MockShinySession$new()))
  expect_error(mcpRequestDisplayMode("cinema", session = MockShinySession$new()))
})
