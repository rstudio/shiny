test_that("McpConnection satisfies the ws duck type and queues frames", {
  conn <- McpConnection$new()
  got <- list()
  conn$onMessage(function(binary, msg) got[[length(got) + 1]] <<- list(binary, msg))
  conn$dispatchFrames(list(list(data = '{"method":"init"}', binary = FALSE)))
  expect_length(got, 1)
  expect_false(got[[1]][[1]])
  expect_equal(got[[1]][[2]], '{"method":"init"}')

  conn$send('{"values":{}}')
  res <- wait_for_result(conn$receiveFrames())
  expect_length(res$frames, 1)
  expect_equal(res$frames[[1]]$data, '{"values":{}}')
  expect_false(res$closed)
})

test_that("binary messages are base64 round-tripped", {
  conn <- McpConnection$new()
  got <- list()
  conn$onMessage(function(binary, msg) got[[length(got) + 1]] <<- list(binary, msg))

  payload <- as.raw(c(1, 2, 2, 2, 255, 0, 128))
  conn$dispatchFrames(list(list(
    data = jsonlite::base64_enc(payload), binary = TRUE
  )))
  expect_true(got[[1]][[1]])
  expect_equal(got[[1]][[2]], payload)

  conn$send(payload)
  res <- wait_for_result(conn$receiveFrames())
  expect_true(res$frames[[1]]$binary)
  expect_equal(jsonlite::base64_dec(res$frames[[1]]$data), payload)
})

test_that("receiveFrames long-polls until send resolves it", {
  conn <- McpConnection$new()
  p <- conn$receiveFrames(timeoutSecs = 5)
  later::later(function() conn$send("late"), 0.2)
  res <- wait_for_result(p)
  expect_equal(res$frames[[1]]$data, "late")
})

test_that("receiveFrames times out with empty frames", {
  conn <- McpConnection$new()
  res <- wait_for_result(conn$receiveFrames(timeoutSecs = 0.2))
  expect_length(res$frames, 0)
  expect_false(res$closed)
})

test_that("server-initiated close resolves pending receive and fires onClose", {
  conn <- McpConnection$new()
  closed <- FALSE
  conn$onClose(function() closed <<- TRUE)
  p <- conn$receiveFrames(timeoutSecs = 5)
  later::later(function() conn$close(), 0.1)
  res <- wait_for_result(p)
  expect_true(res$closed)
  expect_true(closed)
})

test_that("full tunnel: connect/send init/receive config from a real session", {
  ui <- fluidPage(textOutput("out"))
  server <- function(input, output) {
    output$out <- renderText(paste0("n=", input$n))
  }
  app <- shinyApp(ui, server)
  handlers <- createAppHandlers(app$httpHandler, function() server)

  con_res <- mcpTunnelToolCall("_shiny_connect", empty_named_list(), 1, handlers$ws)
  cid <- con_res$result$structuredContent$connectionId
  expect_true(is.character(cid) && nzchar(cid))

  init <- as.character(jsonlite::toJSON(
    list(
      method = "init",
      data = list(
        n = 5,
        .clientdata_url_search = "",
        # Outputs are suspended-when-hidden; the real client reports
        # visibility in the init message.
        .clientdata_output_out_hidden = FALSE
      )
    ),
    auto_unbox = TRUE
  ))
  send_res <- mcpTunnelToolCall(
    "_shiny_send",
    list(connectionId = cid, frames = list(list(data = init, binary = FALSE))),
    2, handlers$ws
  )
  expect_false(isTRUE(send_res$result$isError))

  rec <- wait_for_result(
    mcpTunnelToolCall("_shiny_receive", list(connectionId = cid), 3, handlers$ws),
    timeout = 10
  )
  all_data <- vapply(
    rec$result$structuredContent$frames,
    function(f) f$data, character(1)
  )
  # First outbound message is the config message
  expect_true(any(grepl("workerId", all_data)))

  # Reactive output should flush; pump until we see it
  seen_value <- FALSE
  end <- Sys.time() + 10
  while (!seen_value && Sys.time() < end) {
    pump_shiny(0.1)
    rec <- wait_for_result(
      mcpTunnelToolCall(
        "_shiny_receive", list(connectionId = cid, timeoutSecs = 0.2),
        4, handlers$ws
      ),
      timeout = 10
    )
    frames <- rec$result$structuredContent$frames
    if (length(frames) > 0) {
      datas <- vapply(frames, function(f) f$data, character(1))
      if (any(grepl("n=5", datas, fixed = TRUE))) seen_value <- TRUE
    }
  }
  expect_true(seen_value)

  close_res <- mcpTunnelToolCall(
    "_shiny_close", list(connectionId = cid), 5, handlers$ws
  )
  expect_false(isTRUE(close_res$result$isError))

  err <- mcpTunnelToolCall(
    "_shiny_send", list(connectionId = cid, frames = list()), 6, handlers$ws
  )
  expect_true(isTRUE(err$result$isError))
})

test_that("tools/list over the endpoint includes tunnel tools marked app-only", {
  h <- mcpHttpHandler(function(req) NULL, function(ws) TRUE)
  out <- mcp_post(h, "tools/list")
  tool_names <- vapply(out$result$tools, function(t) t$name, character(1))
  expect_true(all(
    c("_shiny_connect", "_shiny_send", "_shiny_receive", "_shiny_close") %in%
      tool_names
  ))
  connect_tool <- out$result$tools[[which(tool_names == "_shiny_connect")]]
  expect_equal(unlist(connect_tool$`_meta`$ui$visibility), "app")
})
