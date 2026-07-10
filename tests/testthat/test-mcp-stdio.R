make_dispatch <- function() {
  ui <- fluidPage(textOutput("out"))
  server <- function(input, output) {}
  app <- shinyApp(ui, server)
  handlers <- createAppHandlers(app$httpHandler, function() server)
  uiHandler <- joinHandlers(app$httpHandler)
  function(body) mcpDispatch(body, uiHandler, handlers$ws)
}

test_that("mcpStdioHandleLine dispatches requests and writes responses", {
  written <- character(0)
  write <- function(x) written <<- c(written, x)
  dispatch <- make_dispatch()

  mcpStdioHandleLine(
    '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18"}}',
    dispatch, write
  )
  expect_length(written, 1)
  out <- jsonlite::parse_json(written[1])
  expect_equal(out$result$protocolVersion, "2025-06-18")
  expect_equal(out$result$serverInfo$name, "shiny")
  # single line, no embedded newlines
  expect_false(grepl("\n", written[1], fixed = TRUE))

  mcpStdioHandleLine(
    '{"jsonrpc":"2.0","id":2,"method":"tools/list"}',
    dispatch, write
  )
  out <- jsonlite::parse_json(written[2])
  tool_names <- vapply(out$result$tools, function(t) t$name, character(1))
  expect_true("open_shiny_app" %in% tool_names)
})

test_that("mcpStdioHandleLine ignores notifications and blank lines", {
  written <- character(0)
  write <- function(x) written <<- c(written, x)
  dispatch <- make_dispatch()

  mcpStdioHandleLine("", dispatch, write)
  mcpStdioHandleLine(
    '{"jsonrpc":"2.0","method":"notifications/initialized"}',
    dispatch, write
  )
  expect_length(written, 0)
})

test_that("mcpStdioHandleLine reports parse errors and dispatch errors", {
  written <- character(0)
  write <- function(x) written <<- c(written, x)

  mcpStdioHandleLine("not json", make_dispatch(), write)
  out <- jsonlite::parse_json(written[1])
  expect_equal(out$error$code, -32700L)

  boom <- function(body) stop("dispatch exploded")
  mcpStdioHandleLine('{"jsonrpc":"2.0","id":5,"method":"ping"}', boom, write)
  out <- jsonlite::parse_json(written[2])
  expect_equal(out$error$code, -32603L)
  expect_equal(out$id, 5L)
})

test_that("mcpStdioHandleLine resolves promise results asynchronously", {
  written <- character(0)
  write <- function(x) written <<- c(written, x)
  dispatch <- make_dispatch()

  # _shiny_receive long-polls: returns a promise that resolves on timeout
  con_line <- '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"_shiny_connect","arguments":{}}}'
  mcpStdioHandleLine(con_line, dispatch, write)
  cid <- jsonlite::parse_json(written[1])$result$structuredContent$connectionId

  rec_line <- sprintf(
    '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"_shiny_receive","arguments":{"connectionId":"%s","timeoutSecs":0.2}}}',
    cid
  )
  mcpStdioHandleLine(rec_line, dispatch, write)
  expect_length(written, 1) # not resolved yet

  end <- Sys.time() + 5
  while (length(written) < 2 && Sys.time() < end) later::run_now(0.05)
  expect_length(written, 2)
  out <- jsonlite::parse_json(written[2])
  expect_false(is.null(out$result$structuredContent))
})

test_that("stdio transport works end to end in a real subprocess", {
  skip_on_cran()
  skip_if_not_installed("processx")
  skip_if(.Platform$OS.type == "windows")

  app_dir <- withr::local_tempdir()
  writeLines(
    c(
      'library(shiny)',
      'options(shiny.mcp = TRUE, shiny.mcp.stdio = TRUE)',
      'shinyApp(fluidPage("hi"), function(input, output) {})'
    ),
    file.path(app_dir, "app.R")
  )

  # Load the development version of shiny in the subprocess (the installed
  # version won't have the MCP code while this feature is on a branch).
  pkg_dir <- normalizePath(testthat::test_path("../.."), winslash = "/")
  px <- processx::process$new(
    file.path(R.home("bin"), "Rscript"),
    args = c("-e", sprintf(
      "suppressMessages(pkgload::load_all('%s', quiet = TRUE)); shiny::runApp('%s', launch.browser = FALSE)",
      pkg_dir, app_dir
    )),
    stdin = "|", stdout = "|", stderr = "|"
  )
  withr::defer(px$kill())

  read_response <- function(timeout = 20) {
    end <- Sys.time() + timeout
    while (Sys.time() < end) {
      px$poll_io(500)
      line <- px$read_output_lines(1)
      if (length(line) > 0 && nzchar(line)) return(jsonlite::parse_json(line))
      if (!px$is_alive()) {
        stop("subprocess died: ", paste(px$read_all_error_lines(), collapse = "\n"))
      }
    }
    stop("timed out waiting for stdio response; stderr: ",
         paste(px$read_error_lines(), collapse = "\n"))
  }

  px$write_input(
    '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18"}}\n'
  )
  out <- read_response()
  expect_equal(out$id, 1L)
  expect_equal(out$result$serverInfo$name, "shiny")

  px$write_input('{"jsonrpc":"2.0","id":2,"method":"tools/list"}\n')
  out <- read_response()
  tool_names <- vapply(out$result$tools, function(t) t$name, character(1))
  expect_true("open_shiny_app" %in% tool_names)

  px$write_input(
    '{"jsonrpc":"2.0","id":3,"method":"resources/read","params":{"uri":"ui://shiny/app"}}\n'
  )
  out <- read_response()
  expect_match(out$result$contents[[1]]$text, "shiny-mcp-bridge", fixed = TRUE)
})
