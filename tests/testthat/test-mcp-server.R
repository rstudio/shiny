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

test_that("author-declared tools appear in tools/list and execute", {
  tools <- list(
    list(
      name = "add_numbers",
      description = "Add two numbers.",
      inputSchema = list(
        type = "object",
        properties = list(
          x = list(type = "number"),
          y = list(type = "number")
        ),
        required = list("x", "y")
      ),
      handler = function(args) {
        list(sum = args$x + args$y)
      }
    ),
    list(
      name = "get_motd",
      description = "Get the message of the day.",
      handler = function(args) "Hello from R"
    )
  )
  withr::with_options(list(shiny.mcp.tools = tools), {
    h <- mcpHttpHandler(function(req) NULL, function(ws) TRUE)

    out <- mcp_post(h, "tools/list")
    # tools must serialize as a JSON array, not an object
    expect_null(names(out$result$tools))
    tool_names <- vapply(out$result$tools, function(t) t$name, character(1))
    expect_true(all(c("add_numbers", "get_motd") %in% tool_names))
    add_tool <- out$result$tools[[which(tool_names == "add_numbers")]]
    expect_equal(add_tool$inputSchema$properties$x$type, "number")
    # Author tools are model-visible (no app-only visibility marker)
    expect_null(add_tool$`_meta`$ui$visibility)

    res <- mcp_post(h, "tools/call", params = list(
      name = "add_numbers", arguments = list(x = 2, y = 40)
    ))
    expect_equal(res$result$structuredContent$sum, 42)
    expect_match(res$result$content[[1]]$text, "42")

    res <- mcp_post(h, "tools/call", params = list(
      name = "get_motd", arguments = empty_named_list()
    ))
    expect_equal(res$result$content[[1]]$text, "Hello from R")
    expect_null(res$result$structuredContent)
  })
})

test_that("author tool errors become isError results, not crashes", {
  tools <- list(list(
    name = "boom",
    description = "Always fails.",
    handler = function(args) stop("kaboom: ", args$why)
  ))
  withr::with_options(list(shiny.mcp.tools = tools), {
    h <- mcpHttpHandler(function(req) NULL, function(ws) TRUE)
    res <- mcp_post(h, "tools/call", params = list(
      name = "boom", arguments = list(why = "testing")
    ))
    expect_true(isTRUE(res$result$isError))
    expect_match(res$result$content[[1]]$text, "kaboom: testing")
  })
})

test_that("async author tools (promises) are supported", {
  tools <- list(list(
    name = "slow_answer",
    description = "Answers later.",
    handler = function(args) {
      promises::promise(function(resolve, reject) {
        later::later(function() resolve(list(answer = 42)), 0.1)
      })
    }
  ))
  withr::with_options(list(shiny.mcp.tools = tools), {
    h <- mcpHttpHandler(function(req) NULL, function(ws) TRUE)
    res <- mcp_post(h, "tools/call", params = list(
      name = "slow_answer", arguments = empty_named_list()
    ))
    expect_equal(res$result$structuredContent$answer, 42)
  })
})

test_that("invalid or colliding author tools are skipped with a warning", {
  tools <- list(
    list(name = "_shiny_send", description = "collides", handler = function(args) 1),
    list(name = "no_handler", description = "missing handler"),
    list(name = "ok_tool", description = "fine", handler = function(args) "ok")
  )
  withr::with_options(list(shiny.mcp.tools = tools), {
    h <- mcpHttpHandler(function(req) NULL, function(ws) TRUE)
    warnings <- testthat::capture_warnings(out <- mcp_post(h, "tools/list"))
    expect_length(warnings, 2)
    expect_match(warnings, "_shiny_send|no_handler", all = TRUE)
    tool_names <- vapply(out$result$tools, function(t) t$name, character(1))
    expect_true("ok_tool" %in% tool_names)
    expect_false("no_handler" %in% tool_names)
    # the colliding name resolves to the built-in tunnel tool, not the author's
    expect_equal(sum(tool_names == "_shiny_send"), 1)
  })
})

test_that("mcpDirectBase honors option > Connect headers > request origin", {
  req <- new.env(parent = emptyenv())
  req$HTTP_HOST <- "connect.example.com"
  req$HTTP_X_FORWARDED_PROTO <- "https"

  # Request-derived origin (no path information)
  expect_equal(mcpDirectBase(req), "https://connect.example.com")

  # shinyapps.io sends external host + path (schemeless) in
  # X-Redx-Frontend-Name
  req$HTTP_X_REDX_FRONTEND_NAME <- "barret.shinyapps.io/my-app/"
  expect_equal(mcpDirectBase(req), "https://barret.shinyapps.io/my-app")

  # Connect forwards the full external URL of the request (API content)
  req$HTTP_X_RSC_REQUEST <- "https://connect.example.com/content/abc123/mcp"
  expect_equal(mcpDirectBase(req), "https://connect.example.com/content/abc123")

  # Connect sends the app's base URL with every request to Shiny content;
  # it wins over everything but the option (needs no path surgery)
  req$HTTP_RSTUDIO_CONNECT_APP_BASE_URL <-
    "https://connect.example.com/content/def456/"
  expect_equal(mcpDirectBase(req), "https://connect.example.com/content/def456")

  # Explicit option wins over everything, trailing slash normalized
  withr::with_options(
    list(shiny.mcp.origin = "https://other.example.com/apps/dash/"),
    expect_equal(mcpDirectBase(req), "https://other.example.com/apps/dash")
  )
})

test_that("mcpDirectBase ignores empty headers and prefers X-Forwarded-Host", {
  req <- new.env(parent = emptyenv())
  req$HTTP_HOST <- "127.0.0.1:34599"
  req$HTTP_X_FORWARDED_PROTO <- "https"
  # shinyapps.io sends this header with an empty value to Shiny content
  req$HTTP_RSTUDIO_CONNECT_APP_BASE_URL <- ""

  # the internal Host is replaced by the forwarded external host
  req$HTTP_X_FORWARDED_HOST <- "barret.shinyapps.io"
  expect_equal(mcpDirectBase(req), "https://barret.shinyapps.io")
})

test_that("mcpDeployedUrl finds a host-matched rsconnect deployment record", {
  dir <- withr::local_tempdir()
  rec <- file.path(dir, "rsconnect", "connect.example.com", "barret")
  dir.create(rec, recursive = TRUE)
  write.dcf(
    data.frame(
      name = "myapp",
      server = "connect.example.com",
      url = "https://connect.example.com/content/abc123/"
    ),
    file.path(rec, "myapp.dcf")
  )
  rec2 <- file.path(dir, "rsconnect", "shinyapps.io", "barret")
  dir.create(rec2, recursive = TRUE)
  write.dcf(
    data.frame(
      name = "myapp",
      server = "shinyapps.io",
      url = "https://barret.shinyapps.io/myapp/"
    ),
    file.path(rec2, "myapp.dcf")
  )

  # Host match picks the right record and strips the trailing slash
  expect_equal(
    mcpDeployedUrl(dir, "connect.example.com"),
    "https://connect.example.com/content/abc123"
  )
  expect_equal(
    mcpDeployedUrl(dir, "barret.shinyapps.io"),
    "https://barret.shinyapps.io/myapp"
  )
  # No match (e.g. running locally): never use a deployment record
  expect_null(mcpDeployedUrl(dir, "127.0.0.1:7788"))
  expect_null(mcpDeployedUrl(dir, NULL))
  # No records at all
  expect_null(mcpDeployedUrl(withr::local_tempdir(), "connect.example.com"))
})

test_that("resources/read uses the path-aware base: origin-only CSP, full directBase", {
  ui <- fluidPage("hi")
  app <- shinyApp(ui, function(input, output) {})
  withr::with_options(
    list(
      shiny.mcp = TRUE,
      shiny.mcp.origin = "https://connect.example.com/content/abc123/"
    ),
    {
      handlers <- createAppHandlers(
        app$httpHandler,
        function() function(input, output) {}
      )
      req <- mcp_req(list(
        jsonrpc = "2.0", id = 1, method = "resources/read",
        params = list(uri = "ui://shiny/app")
      ))
      out <- jsonlite::parse_json(rawToChar(as_bytes(
        wait_for_result(handlers$http(req))$content
      )))
      csp <- unlist(out$result$contents[[1]]$`_meta`$ui$csp$connectDomains)
      # CSP entries are origins only (host CSP sanitizers reject paths)
      expect_setequal(
        csp,
        c("https://connect.example.com", "wss://connect.example.com")
      )
      # ...but the bridge gets the full base including the path
      expect_match(
        out$result$contents[[1]]$text,
        '"directBase":"https://connect.example.com/content/abc123"',
        fixed = TRUE
      )
    }
  )
})

test_that("mcpDeployedUrl reads Posit Publisher deployment records", {
  dir <- withr::local_tempdir()
  rec_dir <- file.path(dir, ".posit", "publish", "deployments")
  dir.create(rec_dir, recursive = TRUE)
  writeLines(
    c(
      "$schema = 'https://cdn.posit.co/publisher/schemas/posit-publishing-record-schema-v3.json'",
      "server_type = 'connect'",
      "server_url = 'https://connect.example.com'",
      "id = 'abc-123'",
      "dashboard_url = 'https://connect.example.com/connect/#/apps/abc-123'",
      "direct_url = 'https://connect.example.com/content/abc-123/'"
    ),
    file.path(rec_dir, "deployment-A1B2.toml")
  )
  expect_equal(
    mcpDeployedUrl(dir, "connect.example.com"),
    "https://connect.example.com/content/abc-123"
  )
  expect_null(mcpDeployedUrl(dir, "other.example.com"))
})
