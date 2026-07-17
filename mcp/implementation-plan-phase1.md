# Shiny MCP Apps (Phase 1) Implementation Plan

> **Status: executed 2026-07-10.** All tasks complete and E2E-verified (see screenshot-e2e-basic-host.png).

**Goal:** A Shiny app started with `mcpConfigure()` serves an MCP Apps (SEP-1865) endpoint at `/mcp` on its own httpuv port, so MCP hosts render the live app in a sandboxed iframe with reactivity tunneled over postMessage→tools/call.

**Architecture:** A minimal JSON-RPC handler joins Shiny's existing per-app HTTP handler chain (promise-aware via `hybrid_chain`). A duck-typed fake "websocket" (`McpConnection`) feeds `createAppHandlers()$ws()` to create real `ShinySession`s; client frames arrive via a `_shiny_send` tool and server frames leave via a long-polled `_shiny_receive` tool. The `ui://shiny/app` resource is the rendered app page with all assets inlined plus a bundled bridge script that overrides `Shiny.createSocket`.

**Tech Stack:** R (httpuv, promises, later, jsonlite, R6 — all existing deps), TypeScript/esbuild (srcts), `@modelcontextprotocol/ext-apps` (new npm devDependency, bundled).

## Global Constraints

- No new R package dependencies; no exported R functions in Phase 1 (gated by `mcpConfigure()`, tool metadata via `mcpConfigure(description=, arguments=)`).
- Resource URI is exactly `ui://shiny/app`; mimeType exactly `text/html;profile=mcp-app`.
- Tool names: `open_shiny_app` (default, model-visible), `_shiny_connect`, `_shiny_send`, `_shiny_receive`, `_shiny_close` (app-only, `_meta.ui.visibility = ["app"]`).
- Long-poll timeout 15 s; connection GC after 60 s inactivity.
- Frames: `{data: <string>, binary: <bool>}`; binary payloads base64.
- JSON serialization via `jsonlite::toJSON(auto_unbox = TRUE, null = "null")`; empty JSON objects via `named_list()` idiom `setNames(list(), character())`.
- Protocol versions supported: `2024-11-05`, `2025-03-26`, `2025-06-18` (respond with client's if supported, else latest).
- Built JS assets are committed (repo convention, like `inst/www/shared/shiny.js`).
- R tests: testthat 3e style, files `tests/testthat/test-mcp-*.R`. TS tests: node test runner under `srcts/src/mcp/__tests__/`.

---

### Task 1: JSON-RPC endpoint skeleton (`R/mcp-server.R`)

**Files:**
- Create: `R/mcp-server.R`
- Create: `tests/testthat/test-mcp-server.R`
- Modify: `DESCRIPTION` (Collate: add `'mcp-server.R'` alphabetically)

**Interfaces:**
- Produces: `mcpEnabled()` → logical; `mcpToolInfo()` → `list(name, description)`;
  `mcpHttpHandler(uiHandler, wsHandler)` → shiny handler function(req) → NULL | httpResponse | promise;
  `mcpDispatch(body, uiHandler, wsHandler)` → list (JSON-RPC response) | promise | NULL (notification);
  `mcpResult(id, result)`, `mcpError(id, code, message)` → JSON-RPC response lists;
  `named_list(...)` helper; `MCP_PROTOCOL_VERSIONS`.
- Consumes: `httpResponse()` (R/middleware.R), `hybrid_chain()` (existing internal), `safeFromJSON()`.
- Task 3 extends `mcpToolsList()`/`mcpToolCall()`; Task 4 fills `mcpResourcesList()`/`mcpResourcesRead()`.

- [x] **Step 1: Write failing tests** — `tests/testthat/test-mcp-server.R`:

```r
mcp_req <- function(body, method = "POST", path = "/mcp") {
  req <- new.env(parent = emptyenv())
  req$REQUEST_METHOD <- method
  req$PATH_INFO <- path
  req$QUERY_STRING <- ""
  req$HTTP_HOST <- "127.0.0.1"
  if (!is.null(body)) {
    json <- jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")
    req$rook.input <- fakeRookInput(charToRaw(json))
  }
  req
}

fakeRookInput <- function(bytes) {
  list(read = function(...) bytes, rewind = function() NULL)
}

mcp_post <- function(handler, method, params = NULL, id = 1) {
  body <- list(jsonrpc = "2.0", id = id, method = method)
  if (!is.null(params)) body$params <- params
  resp <- handler(mcp_req(body))
  resp <- wait_for_result(resp)
  expect_s3_class(resp, "httpResponse")
  jsonlite::parse_json(rawToChar(as_bytes(resp$content)))
}

as_bytes <- function(x) if (is.raw(x)) x else charToRaw(enc2utf8(x))

wait_for_result <- function(x, timeout = 5) {
  if (!promises::is.promising(x)) return(x)
  result <- NULL; done <- FALSE
  promises::then(x, function(value) { result <<- value; done <<- TRUE })
  end <- Sys.time() + timeout
  while (!done && Sys.time() < end) later::run_now(0.05)
  if (!done) stop("timed out waiting for promise")
  result
}

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
  out <- mcp_post(h, "initialize",
    params = list(protocolVersion = "2025-06-18", capabilities = named_list()))
  expect_equal(out$result$protocolVersion, "2025-06-18")
  expect_equal(out$result$serverInfo$name, "shiny")
  expect_true(is.list(out$result$capabilities$tools))
  out2 <- mcp_post(h, "initialize",
    params = list(protocolVersion = "9999-01-01", capabilities = named_list()))
  expect_equal(out2$result$protocolVersion, "2025-06-18")
})

test_that("notifications get 202, unknown methods get -32601, ping works", {
  h <- mcpHttpHandler(function(req) NULL, function(ws) TRUE)
  notif <- h(mcp_req(list(jsonrpc = "2.0", method = "notifications/initialized")))
  expect_equal(notif$status, 202L)
  out <- mcp_post(h, "bogus/method")
  expect_equal(out$error$code, -32601L)
  out <- mcp_post(h, "ping")
  expect_identical(out$result, structure(list(), names = character(0)) %||% list())
})

test_that("tools/list includes the visible app tool with ui metadata", {
  h <- mcpHttpHandler(function(req) NULL, function(ws) TRUE)
  out <- mcp_post(h, "tools/list")
  tools <- out$result$tools
  names_ <- vapply(tools, function(t) t$name, character(1))
  expect_true("open_shiny_app" %in% names_)
  app_tool <- tools[[which(names_ == "open_shiny_app")]]
  expect_equal(app_tool$`_meta`$ui$resourceUri, "ui://shiny/app")
})
```

- [x] **Step 2: Run tests, verify failure** — `Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-mcp-server.R")'` → errors: `mcpHttpHandler` not found.

- [x] **Step 3: Implement `R/mcp-server.R`:**

```r
MCP_PROTOCOL_VERSIONS <- c("2024-11-05", "2025-03-26", "2025-06-18")
MCP_RESOURCE_URI <- "ui://shiny/app"
MCP_RESOURCE_MIME <- "text/html;profile=mcp-app"

named_list <- function(...) {
  out <- list(...)
  if (length(out) == 0) names(out) <- character(0)
  out
}

mcpEnabled <- function() {
  isTRUE(.globals$mcp$enabled)
}

mcpToolInfo <- function() {
  cfg <- .globals$mcp
  list(
    name = cfg$appId %||% "open_shiny_app",
    description = cfg$description %||% paste(
      "Open the interactive Shiny application so the user can view and",
      "interact with it. Call this when the user wants to see the app."
    )
  )
}

mcpCorsHeaders <- function() {
  list(
    "Access-Control-Allow-Origin" = "*",
    "Access-Control-Allow-Methods" = "GET, POST, OPTIONS",
    "Access-Control-Allow-Headers" =
      "Content-Type, Authorization, Mcp-Session-Id, Mcp-Protocol-Version, Last-Event-Id"
  )
}

mcpJsonResponse <- function(payload, status = 200L) {
  httpResponse(
    status = status,
    content_type = "application/json",
    content = enc2utf8(as.character(jsonlite::toJSON(
      payload, auto_unbox = TRUE, null = "null", force = TRUE
    ))),
    headers = mcpCorsHeaders()
  )
}

mcpResult <- function(id, result) {
  list(jsonrpc = "2.0", id = id, result = result)
}

mcpError <- function(id, code, message) {
  list(jsonrpc = "2.0", id = id, error = list(code = code, message = message))
}

mcpHttpHandler <- function(uiHandler, wsHandler) {
  force(uiHandler); force(wsHandler)
  function(req) {
    path <- sub("/$", "", req$PATH_INFO %||% "")
    if (!identical(path, "/mcp")) return(NULL)

    method <- req$REQUEST_METHOD
    if (identical(method, "OPTIONS")) {
      return(httpResponse(204L, "text/plain", "", headers = mcpCorsHeaders()))
    }
    if (!identical(method, "POST")) {
      return(httpResponse(
        405L, "text/plain",
        "shiny MCP endpoint: use POST (streamable HTTP, JSON responses).",
        headers = c(mcpCorsHeaders(), list(Allow = "POST, OPTIONS"))
      ))
    }

    body <- tryCatch({
      raw <- req$rook.input$read()
      safeFromJSON(rawToChar(raw), simplifyVector = FALSE)
    }, error = function(e) NULL)
    if (is.null(body) || !is.list(body)) {
      return(mcpJsonResponse(mcpError(NULL, -32700L, "Parse error"), 400L))
    }

    out <- tryCatch(
      mcpDispatch(body, uiHandler, wsHandler),
      error = function(e) mcpError(body$id, -32603L, conditionMessage(e))
    )
    if (is.null(out)) {
      # Notification: no response body
      return(httpResponse(202L, "application/json", "", headers = mcpCorsHeaders()))
    }
    hybrid_chain(out, mcpJsonResponse)
  }
}

mcpDispatch <- function(body, uiHandler, wsHandler) {
  method <- body$method
  if (!is.character(method) || length(method) != 1) {
    return(mcpError(body$id, -32600L, "Invalid Request"))
  }
  if (grepl("^notifications/", method)) return(NULL)

  switch(method,
    "initialize" = mcpResult(body$id, mcpInitializeResult(body)),
    "ping" = mcpResult(body$id, named_list()),
    "tools/list" = mcpResult(body$id, list(tools = mcpToolsList())),
    "tools/call" = mcpToolCall(body, uiHandler, wsHandler),
    "resources/list" = mcpResult(body$id, list(resources = mcpResourcesList())),
    "resources/read" = mcpResourcesRead(body, uiHandler),
    mcpError(body$id, -32601L, "Method not found")
  )
}

mcpInitializeResult <- function(body) {
  requested <- body$params$protocolVersion %||% ""
  version <- if (requested %in% MCP_PROTOCOL_VERSIONS) {
    requested
  } else {
    MCP_PROTOCOL_VERSIONS[length(MCP_PROTOCOL_VERSIONS)]
  }
  list(
    protocolVersion = version,
    capabilities = list(
      tools = list(listChanged = FALSE),
      resources = list(subscribe = FALSE, listChanged = FALSE)
    ),
    serverInfo = list(
      name = "shiny",
      version = as.character(get_package_version("shiny"))
    )
  )
}

mcpToolsList <- function() {
  info <- mcpToolInfo()
  list(
    list(
      name = info$name,
      description = info$description,
      inputSchema = list(type = "object", properties = named_list()),
      `_meta` = list(ui = list(resourceUri = MCP_RESOURCE_URI))
    )
  )
}

mcpToolCall <- function(body, uiHandler, wsHandler) {
  name <- body$params$name %||% ""
  info <- mcpToolInfo()
  if (identical(name, info$name)) {
    return(mcpResult(body$id, list(
      content = list(list(
        type = "text",
        text = "The Shiny app is now displayed in the conversation."
      ))
    )))
  }
  mcpError(body$id, -32602L, sprintf("Unknown tool: %s", name))
}

mcpResourcesList <- function() list()

mcpResourcesRead <- function(body, uiHandler) {
  mcpError(body$id, -32602L, "Unknown resource")
}
```

- [x] **Step 4: Run tests, verify pass** — same command as Step 2. Expected: all pass.

- [x] **Step 5: Add `'mcp-server.R'` to DESCRIPTION Collate** (alphabetically, after `'map.R'`/near `'middleware.R'`), run `Rscript -e 'devtools::document()'`, re-run tests.

- [x] **Step 6: Commit** — `git add R/mcp-server.R tests/testthat/test-mcp-server.R DESCRIPTION && git commit -m "feat(mcp): JSON-RPC endpoint skeleton for MCP Apps"`

---

### Task 2: Wire `/mcp` into the app handler chain

**Files:**
- Modify: `R/server.R:134-321` (`createAppHandlers`), `R/server.R:389-403` (`startHttpuvApp`)
- Test: `tests/testthat/test-mcp-server.R` (append)

**Interfaces:**
- Consumes: `mcpEnabled()`, `mcpHttpHandler(uiHandler, wsHandler)` from Task 1.
- Produces: when `mcpConfigure()` is called before `createAppHandlers()` time, the returned `$http` handler serves `/mcp`; `startHttpuvApp` adds `"mcp" = excludeStaticPath()`.

- [x] **Step 1: Failing test** (append to `test-mcp-server.R`):

```r
test_that("createAppHandlers mounts /mcp only when enabled", {
  ui <- fluidPage("hello mcp")
  app <- shinyApp(ui, function(input, output) {})
  make_handler <- function() {
    h <- createAppHandlers(app$httpHandler, function() function(input, output) {})
    h$http
  }
  withr::with_options(list(shiny.mcp = TRUE), {
    resp <- wait_for_result(make_handler()(mcp_req(
      list(jsonrpc = "2.0", id = 1, method = "ping"))))
    expect_equal(resp$status, 200L)
  })
  withr::with_options(list(shiny.mcp = FALSE), {
    expect_null(make_handler()(mcp_req(
      list(jsonrpc = "2.0", id = 1, method = "ping"))))
  })
})
```

- [x] **Step 2: Run, verify failure** (the disabled case passes but enabled case gets NULL).

- [x] **Step 3: Implement.** In `createAppHandlers()` restructure so the ws handler is a named local, and include the MCP handler ahead of app handlers:

```r
  wsHandler <- function(ws) {
    # ... entire existing body of the current `ws = function(ws)` unchanged ...
  }

  appHandlers <- list(
    http = joinHandlers(c(
      sessionHandler,
      if (mcpEnabled()) mcpHttpHandler(joinHandlers(httpHandlers), wsHandler),
      httpHandlers,
      sys.www.root,
      resourcePathHandler,
      reactLogHandler
    )),
    ws = wsHandler
  )
```

(`joinHandlers` already drops NULLs.) In `startHttpuvApp()` staticPaths list add:

```r
      "session" = excludeStaticPath(),
      "mcp" = if (mcpEnabled()) excludeStaticPath(),
```

…using `dropNulls()` around the list (match existing style; `dropNulls` exists in utils).

- [x] **Step 4: Run tests, verify pass.**

- [x] **Step 5: Commit** — `git commit -am "feat(mcp): mount /mcp handler in app handler chain when enabled"`

---

### Task 3: Tunnel — `McpConnection` + `_shiny_*` tools (`R/mcp-tunnel.R`)

**Files:**
- Create: `R/mcp-tunnel.R`
- Create: `tests/testthat/test-mcp-tunnel.R`
- Modify: `R/mcp-server.R` (`mcpToolsList`, `mcpToolCall`)
- Modify: `DESCRIPTION` Collate

**Interfaces:**
- Produces: `McpConnection` R6 class with ws duck-type (`$send(msg)`, `$close(code)`, `$request`, `$onMessage(cb)`, `$onClose(cb)`) plus tunnel side (`$id`, `$dispatchFrames(frames)`, `$receiveFrames(timeoutSecs = 15)` → promise of `list(frames = <list>, closed = <bool>)`, `$notifyClosed()`, `$lastActivity`);
  registry helpers `mcpConnRegister(conn)`, `mcpConnGet(id)`, `mcpConnRemove(id)`; `mcpTunnelToolsList()`; `mcpTunnelToolCall(name, params, id, wsHandler)` → JSON-RPC result/error or promise.
- Consumes: `named_list`, `mcpResult`, `mcpError` (Task 1); `createUniqueId` (existing); `Map` (R/map.R); `later`, `promises`.

- [x] **Step 1: Failing tests** — `tests/testthat/test-mcp-tunnel.R`:

```r
test_that("McpConnection satisfies the ws duck type and queues frames", {
  conn <- McpConnection$new()
  got <- list()
  conn$onMessage(function(binary, msg) got[[length(got) + 1]] <<- list(binary, msg))
  conn$dispatchFrames(list(list(data = "{\"method\":\"init\"}", binary = FALSE)))
  expect_length(got, 1)
  expect_false(got[[1]][[1]])

  conn$send("{\"values\":{}}")
  res <- wait_for_result(conn$receiveFrames())
  expect_length(res$frames, 1)
  expect_equal(res$frames[[1]]$data, "{\"values\":{}}")
  expect_false(res$closed)
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
})

test_that("full tunnel: connect/send init/receive config from a real session", {
  ui <- fluidPage(textOutput("out"))
  server <- function(input, output) {
    output$out <- renderText(paste0("n=", input$n))
  }
  app <- shinyApp(ui, server)
  handlers <- createAppHandlers(app$httpHandler, function() server)

  con_res <- mcpTunnelToolCall("_shiny_connect", named_list(), 1, handlers$ws)
  cid <- con_res$result$structuredContent$connectionId
  expect_true(is.character(cid) && nzchar(cid))

  init <- jsonlite::toJSON(list(
    method = "init",
    data = list(n = 5, .clientdata_url_search = "")
  ), auto_unbox = TRUE)
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
  all_data <- vapply(rec$result$structuredContent$frames,
                     function(f) f$data, character(1))
  # First outbound message is the config message
  expect_true(any(grepl("workerId", all_data)))

  # Reactive output should flush; pump until we see it
  seen_value <- FALSE
  end <- Sys.time() + 10
  while (!seen_value && Sys.time() < end) {
    pump_shiny(0.1)
    rec <- wait_for_result(
      mcpTunnelToolCall("_shiny_receive", list(connectionId = cid), 4, handlers$ws),
      timeout = 10)
    frames <- rec$result$structuredContent$frames
    if (length(frames) &&
        any(grepl("n=5", vapply(frames, function(f) f$data, character(1))))) {
      seen_value <- TRUE
    }
  }
  expect_true(seen_value)

  close_res <- mcpTunnelToolCall("_shiny_close", list(connectionId = cid), 5, handlers$ws)
  expect_false(isTRUE(close_res$result$isError))
  err <- mcpTunnelToolCall("_shiny_send",
    list(connectionId = cid, frames = list()), 6, handlers$ws)
  expect_true(!is.null(err$error) || isTRUE(err$result$isError))
})
```

With test helper (put in `tests/testthat/helper-mcp.R`, moving `mcp_req`/`fakeRookInput`/`mcp_post`/`wait_for_result`/`as_bytes` there too):

```r
# Replicates one iteration of serviceApp() without blocking on httpuv
pump_shiny <- function(seconds = 0.1) {
  end <- Sys.time() + seconds
  while (Sys.time() < end) {
    timerCallbacks$executeElapsed()
    flushReact()
    flushPendingSessions()
    later::run_now(0.02)
  }
}
```

- [x] **Step 2: Run, verify failure** (`McpConnection` not found).

- [x] **Step 3: Implement `R/mcp-tunnel.R`:**

```r
McpConnection <- R6::R6Class("McpConnection",
  public = list(
    request = NULL,
    lastActivity = NULL,
    initialize = function() {
      private$id_ <- createUniqueId(16)
      self$lastActivity <- Sys.time()
      req <- new.env(parent = emptyenv())
      req$PATH_INFO <- "/websocket/"
      req$QUERY_STRING <- ""
      req$REQUEST_METHOD <- "GET"
      self$request <- req
    },
    # ---- websocket duck type (consumed by ShinySession/createAppHandlers) ----
    send = function(msg) {
      if (private$closed_) return(invisible())
      frame <- if (is.raw(msg)) {
        list(data = jsonlite::base64_enc(msg), binary = TRUE)
      } else {
        list(data = msg, binary = FALSE)
      }
      private$outbox_[[length(private$outbox_) + 1]] <- frame
      private$resolvePending()
      invisible()
    },
    close = function(code = 1000L) {
      # Server-initiated close (e.g. session$close()); tell the client.
      if (private$closed_) return(invisible())
      private$closed_ <- TRUE
      private$resolvePending()
      private$fireClose()
      invisible()
    },
    onMessage = function(cb) private$messageCallbacks_ <- c(private$messageCallbacks_, cb),
    onClose = function(cb) private$closeCallbacks_ <- c(private$closeCallbacks_, cb),
    # ---- tunnel side ----
    dispatchFrames = function(frames) {
      self$lastActivity <- Sys.time()
      for (frame in frames) {
        binary <- isTRUE(frame$binary)
        msg <- if (binary) jsonlite::base64_dec(frame$data) else frame$data
        for (cb in private$messageCallbacks_) cb(binary, msg)
      }
      invisible()
    },
    receiveFrames = function(timeoutSecs = 15) {
      self$lastActivity <- Sys.time()
      if (length(private$outbox_) > 0 || private$closed_) {
        return(promises::promise_resolve(private$drain()))
      }
      promises::promise(function(resolve, reject) {
        private$pending_ <- resolve
        later::later(function() {
          if (!is.null(private$pending_)) {
            resolve_fn <- private$pending_
            private$pending_ <- NULL
            resolve_fn(private$drain())
          }
        }, timeoutSecs)
      })
    },
    notifyClosed = function() {
      # Client-initiated close (_shiny_close or GC)
      if (private$closed_) return(invisible())
      private$closed_ <- TRUE
      private$resolvePending()
      private$fireClose()
      invisible()
    },
    isClosed = function() private$closed_
  ),
  active = list(
    id = function() private$id_
  ),
  private = list(
    id_ = NULL,
    outbox_ = list(),
    pending_ = NULL,
    closed_ = FALSE,
    closeFired_ = FALSE,
    messageCallbacks_ = list(),
    closeCallbacks_ = list(),
    drain = function() {
      frames <- private$outbox_
      private$outbox_ <- list()
      list(frames = frames, closed = private$closed_)
    },
    resolvePending = function() {
      if (!is.null(private$pending_)) {
        resolve_fn <- private$pending_
        private$pending_ <- NULL
        resolve_fn(private$drain())
      }
    },
    fireClose = function() {
      if (private$closeFired_) return()
      private$closeFired_ <- TRUE
      for (cb in private$closeCallbacks_) cb()
    }
  )
)

mcpConnections <- NULL
on_load({
  mcpConnections <- Map$new()
})

MCP_CONN_IDLE_TIMEOUT_SECS <- 60

mcpConnRegister <- function(conn) {
  mcpConnections$set(conn$id, conn)
  mcpScheduleSweep()
  invisible(conn)
}

mcpConnGet <- function(id) {
  if (is.null(id) || !is.character(id)) return(NULL)
  mcpConnections$get(id)
}

mcpConnRemove <- function(id) {
  mcpConnections$remove(id)
}

mcpSweepScheduled <- FALSE
mcpScheduleSweep <- function() {
  if (mcpSweepScheduled) return(invisible())
  utils::assignInMyNamespace("mcpSweepScheduled", TRUE)
  later::later(function() {
    utils::assignInMyNamespace("mcpSweepScheduled", FALSE)
    now <- Sys.time()
    for (id in mcpConnections$keys()) {
      conn <- mcpConnections$get(id)
      if (is.null(conn)) next
      idle <- as.numeric(difftime(now, conn$lastActivity, units = "secs"))
      if (idle > MCP_CONN_IDLE_TIMEOUT_SECS) {
        conn$notifyClosed()
        mcpConnections$remove(id)
      }
    }
    if (mcpConnections$size() > 0) mcpScheduleSweep()
  }, 30)
  invisible()
}

mcpToolResult <- function(id, structured = NULL, text = "ok") {
  result <- list(content = list(list(type = "text", text = text)))
  if (!is.null(structured)) result$structuredContent <- structured
  mcpResult(id, result)
}

mcpToolErrorResult <- function(id, text) {
  mcpResult(id, list(
    content = list(list(type = "text", text = text)),
    isError = TRUE
  ))
}

mcpTunnelToolsList <- function() {
  app_only <- list(ui = list(visibility = list("app")))
  conn_arg <- list(connectionId = list(type = "string"))
  list(
    list(
      name = "_shiny_connect",
      description = "Internal: open a Shiny session for the app iframe.",
      inputSchema = list(type = "object", properties = named_list()),
      `_meta` = app_only
    ),
    list(
      name = "_shiny_send",
      description = "Internal: deliver client frames to the Shiny session.",
      inputSchema = list(
        type = "object",
        properties = c(conn_arg, list(frames = list(type = "array"))),
        required = list("connectionId", "frames")
      ),
      `_meta` = app_only
    ),
    list(
      name = "_shiny_receive",
      description = "Internal: long-poll for frames from the Shiny session.",
      inputSchema = list(
        type = "object", properties = conn_arg,
        required = list("connectionId")
      ),
      `_meta` = app_only
    ),
    list(
      name = "_shiny_close",
      description = "Internal: close the Shiny session.",
      inputSchema = list(
        type = "object", properties = conn_arg,
        required = list("connectionId")
      ),
      `_meta` = app_only
    )
  )
}

mcpTunnelToolCall <- function(name, params, id, wsHandler) {
  if (identical(name, "_shiny_connect")) {
    conn <- McpConnection$new()
    ok <- wsHandler(conn)
    if (!isTRUE(ok)) {
      return(mcpToolErrorResult(id, "Failed to open Shiny session"))
    }
    mcpConnRegister(conn)
    return(mcpToolResult(id, list(connectionId = conn$id), "connected"))
  }

  conn <- mcpConnGet(params$connectionId)
  if (is.null(conn) || conn$isClosed()) {
    return(mcpToolErrorResult(id, "Unknown or closed connectionId"))
  }

  if (identical(name, "_shiny_send")) {
    conn$dispatchFrames(params$frames %||% list())
    return(mcpToolResult(id))
  }
  if (identical(name, "_shiny_receive")) {
    return(promises::then(conn$receiveFrames(), function(res) {
      mcpToolResult(id, res, "frames")
    }))
  }
  if (identical(name, "_shiny_close")) {
    conn$notifyClosed()
    mcpConnRemove(conn$id)
    return(mcpToolResult(id, NULL, "closed"))
  }
  mcpError(id, -32602L, sprintf("Unknown tool: %s", name))
}
```

Update `R/mcp-server.R`:

```r
mcpToolsList <- function() {
  info <- mcpToolInfo()
  c(
    list(list(
      name = info$name,
      description = info$description,
      inputSchema = list(type = "object", properties = named_list()),
      `_meta` = list(ui = list(resourceUri = MCP_RESOURCE_URI))
    )),
    mcpTunnelToolsList()
  )
}

mcpToolCall <- function(body, uiHandler, wsHandler) {
  name <- body$params$name %||% ""
  info <- mcpToolInfo()
  if (identical(name, info$name)) {
    return(mcpResult(body$id, list(
      content = list(list(
        type = "text",
        text = "The Shiny app is now displayed in the conversation."
      ))
    )))
  }
  if (startsWith(name, "_shiny_")) {
    return(mcpTunnelToolCall(name, body$params$arguments %||% named_list(),
                             body$id, wsHandler))
  }
  mcpError(body$id, -32602L, sprintf("Unknown tool: %s", name))
}
```

Note: `Map` (R/map.R) — verify it has `$keys()`/`$size()`; if the methods are named differently (e.g. `$keys()` vs `ls()`), adapt sweeper accordingly.

- [x] **Step 4: Run tests, verify pass** (both files).

- [x] **Step 5: Add `'mcp-tunnel.R'` to Collate; commit** — `git commit -am "feat(mcp): postMessage tunnel connection and _shiny_* tools"`

---

### Task 4: `ui://shiny/app` resource — single-file HTML (`R/mcp-app.R`)

**Files:**
- Create: `R/mcp-app.R`
- Create: `tests/testthat/test-mcp-app.R`
- Modify: `R/mcp-server.R` (`mcpResourcesList`, `mcpResourcesRead`)
- Modify: `DESCRIPTION` Collate

**Interfaces:**
- Produces: `renderMcpAppHtml(uiHandler)` → single-file HTML string (inlined assets + bridge `<script>` before `</body>`); `inlineHtmlAssets(html)`; `resolveAssetFile(urlPath)` → local file path or NULL.
- Consumes: `.globals$resourcePaths` (each value has `$path`), `system_file()`, Task 1 helpers. Bridge asset `inst/www/shared/shiny-mcp-bridge.js` (Task 5 creates it; until then a placeholder file with `/* shiny mcp bridge placeholder */` keeps tests green).

- [x] **Step 1: Failing tests** — `tests/testthat/test-mcp-app.R`:

```r
test_that("resources/list and resources/read serve a self-contained app page", {
  ui <- fluidPage(sliderInput("n", "N", 1, 10, 5), plotOutput("p"))
  app <- shinyApp(ui, function(input, output) {})
  withr::with_options(list(shiny.mcp = TRUE), {
    handlers <- createAppHandlers(app$httpHandler, function() function(input, output) {})
    h <- handlers$http

    out <- mcp_post(h, "resources/list")
    res <- out$result$resources[[1]]
    expect_equal(res$uri, "ui://shiny/app")
    expect_equal(res$mimeType, "text/html;profile=mcp-app")

    out <- mcp_post(h, "resources/read", params = list(uri = "ui://shiny/app"))
    content <- out$result$contents[[1]]
    expect_equal(content$mimeType, "text/html;profile=mcp-app")
    html <- content$text
    # All scripts and stylesheets inlined: no external src/href files remain
    expect_false(grepl("<script[^>]+src=", html))
    expect_false(grepl("<link[^>]+href=", html))
    # Key payloads present
    expect_match(html, "Shiny.createSocket|shiny-mcp-bridge", fixed = FALSE)
    expect_match(html, "sliderInput|shiny-bound|slider", fixed = FALSE)
    # jquery + shiny.js inlined (look for distinctive strings)
    expect_match(html, "jQuery", fixed = TRUE)

    out <- mcp_post(h, "resources/read", params = list(uri = "ui://nope"))
    expect_equal(out$error$code, -32602L)
  })
})
```

- [x] **Step 2: Run, verify failure.**

- [x] **Step 3: Implement `R/mcp-app.R`:**

```r
mcpFakePageRequest <- function() {
  req <- new.env(parent = emptyenv())
  req$REQUEST_METHOD <- "GET"
  req$PATH_INFO <- "/"
  req$QUERY_STRING <- ""
  req$HTTP_HOST <- "127.0.0.1"
  req
}

renderMcpAppHtml <- function(uiHandler) {
  resp <- uiHandler(mcpFakePageRequest())
  if (is.null(resp) || !inherits(resp, "httpResponse")) {
    stop("Unable to render app UI for MCP resource")
  }
  html <- resp$content
  if (is.raw(html)) html <- rawToChar(html)
  html <- inlineHtmlAssets(html)
  bridge <- mcpBridgeScript()
  sub("</body>", paste0(bridge, "\n</body>"), html, fixed = TRUE)
}

mcpBridgeScript <- function() {
  path <- system_file("www/shared/shiny-mcp-bridge.js", package = "shiny")
  js <- readChar(path, file.info(path)$size, useBytes = TRUE)
  Encoding(js) <- "UTF-8"
  paste0("<script>\n", escapeScriptContent(js), "\n</script>")
}

escapeScriptContent <- function(x) {
  gsub("</(script)", "<\\\\/\\1", x, ignore.case = TRUE)
}

escapeStyleContent <- function(x) {
  gsub("</(style)", "<\\\\/\\1", x, ignore.case = TRUE)
}

# Map a URL path from the rendered page to a local file, using the
# resource paths registered by createWebDependency()/addResourcePath()
# plus shiny's shared www dir.
resolveAssetFile <- function(urlPath) {
  urlPath <- sub("^\\./", "", sub("^/", "", URLdecode(urlPath)))
  urlPath <- sub("[?#].*$", "", urlPath)
  parts <- strsplit(urlPath, "/", fixed = TRUE)[[1]]
  if (length(parts) < 2) return(NULL)
  prefix <- parts[1]
  rest <- paste(parts[-1], collapse = "/")

  target <- .globals$resourcePaths[[prefix]]
  root <- if (!is.null(target)) {
    if (inherits(target, "staticPath")) target$path else as.character(target)
  } else if (identical(prefix, "shared")) {
    system_file("www/shared", package = "shiny")
  } else {
    return(NULL)
  }
  candidate <- file.path(root, rest)
  if (file.exists(candidate)) candidate else NULL
}

inlineHtmlAssets <- function(html) {
  # <script src="..."></script>
  html <- inlineTagPattern(
    html,
    pattern = "<script[^>]*\\ssrc=\"([^\"]+)\"[^>]*>\\s*</script>",
    build = function(file) {
      js <- readAssetText(file)
      paste0("<script>\n", escapeScriptContent(js), "\n</script>")
    }
  )
  # <link rel="stylesheet" href="..."/>
  inlineTagPattern(
    html,
    pattern = "<link[^>]*\\shref=\"([^\"]+)\"[^>]*/?>",
    build = function(file) {
      css <- readAssetText(file)
      paste0("<style>\n", escapeStyleContent(css), "\n</style>")
    },
    onlyIf = function(tag) grepl("stylesheet", tag, fixed = TRUE)
  )
}

readAssetText <- function(file) {
  txt <- readChar(file, file.info(file)$size, useBytes = TRUE)
  Encoding(txt) <- "UTF-8"
  txt
}

inlineTagPattern <- function(html, pattern, build, onlyIf = NULL) {
  matches <- gregexpr(pattern, html, ignore.case = TRUE)[[1]]
  if (identical(as.integer(matches[1]), -1L)) return(html)

  lengths <- attr(matches, "match.length")
  out <- character(0)
  last <- 1
  for (i in seq_along(matches)) {
    start <- matches[i]
    end <- start + lengths[i] - 1
    tag <- substr(html, start, end)
    replacement <- tag
    if (is.null(onlyIf) || isTRUE(onlyIf(tag))) {
      url <- sub(pattern, "\\1", tag, ignore.case = TRUE)
      file <- resolveAssetFile(url)
      if (!is.null(file)) replacement <- build(file)
      # If unresolvable, drop external refs entirely (sandbox can't load them)
      else replacement <- ""
    }
    out <- c(out, substr(html, last, start - 1), replacement)
    last <- end + 1
  }
  out <- c(out, substr(html, last, nchar(html)))
  paste(out, collapse = "")
}
```

Update `R/mcp-server.R`:

```r
mcpResourcesList <- function() {
  list(list(
    uri = MCP_RESOURCE_URI,
    name = "Shiny application",
    description = "The interactive Shiny application UI.",
    mimeType = MCP_RESOURCE_MIME,
    `_meta` = list(ui = list(prefersBorder = TRUE))
  ))
}

mcpResourcesRead <- function(body, uiHandler) {
  uri <- body$params$uri %||% ""
  if (!identical(uri, MCP_RESOURCE_URI)) {
    return(mcpError(body$id, -32602L, sprintf("Unknown resource: %s", uri)))
  }
  html <- renderMcpAppHtml(uiHandler)
  mcpResult(body$id, list(contents = list(list(
    uri = MCP_RESOURCE_URI,
    mimeType = MCP_RESOURCE_MIME,
    text = html
  ))))
}
```

Also create placeholder `inst/www/shared/shiny-mcp-bridge.js` containing `/* shiny-mcp-bridge placeholder; built by npm run build (Task 5) */` so R code paths work before Task 5.

- [x] **Step 4: Run tests, verify pass** (all three mcp test files).

- [x] **Step 5: Add `'mcp-app.R'` to Collate; commit** — `git commit -am "feat(mcp): ui://shiny/app single-file resource with inlined assets"`

---

### Task 5: TypeScript bridge (`srcts/src/mcp/`) + build

**Files:**
- Create: `srcts/src/mcp/tunnel-socket.ts`, `srcts/src/mcp/index.ts`
- Create: `srcts/src/mcp/__tests__/tunnel-socket.test.ts`
- Create: `srcts/build/mcp-bridge.ts`
- Modify: `package.json` (devDependency `@modelcontextprotocol/ext-apps`; scripts `bundle_mcp`, include in `build`)
- Produces artifact: `inst/www/shared/shiny-mcp-bridge.js` (committed)

**Interfaces:**
- Consumes (server, Tasks 3-4): tools `_shiny_connect` → `structuredContent.connectionId: string`; `_shiny_send {connectionId, frames: [{data, binary}]}`; `_shiny_receive {connectionId}` → `structuredContent: {frames: [{data, binary}], closed: boolean}`; `_shiny_close {connectionId}`.
- Consumes (client): `window.Shiny.createSocket` extension point; WebSocket interface expected by `shinyapp.ts` (`send`, `close`, `readyState` truthy when open, settable `onopen/onmessage/onclose`).
- Produces: IIFE bundle that, on load, instantiates the ext-apps `App`, connects, and installs `Shiny.createSocket`.

- [x] **Step 1: Install dep** — `npm install --save-dev @modelcontextprotocol/ext-apps`. Then inspect `node_modules/@modelcontextprotocol/ext-apps/dist/*.d.ts` for the exact `App` API (`connect()`, `callServerTool()`, teardown callback name) and adjust the code below to the real names before writing it.

- [x] **Step 2: Write failing TS test** — `srcts/src/mcp/__tests__/tunnel-socket.test.ts` (node test runner style used by `npm run test_types`):

```ts
import assert from "node:assert";
import test from "node:test";
import { McpTunnelWebSocket, type ToolCaller } from "../tunnel-socket";

function makeHost(receiveBatches: Array<{ frames: any[]; closed: boolean }>) {
  const sent: any[] = [];
  const caller: ToolCaller = async (name, args) => {
    if (name === "_shiny_connect") return { connectionId: "abc" };
    if (name === "_shiny_send") {
      sent.push(args);
      return {};
    }
    if (name === "_shiny_receive") {
      const batch = receiveBatches.shift();
      if (batch) return batch;
      return await new Promise(() => {}); // hang (no more data)
    }
    if (name === "_shiny_close") return {};
    throw new Error("unknown tool " + name);
  };
  return { caller, sent };
}

test("opens, delivers frames in order, sends, and closes", async () => {
  const { caller, sent } = makeHost([
    { frames: [{ data: '{"config":{}}', binary: false }], closed: false },
    { frames: [], closed: true },
  ]);
  const sock = new McpTunnelWebSocket(caller);
  const events: string[] = [];
  sock.onopen = () => events.push("open");
  sock.onmessage = (e: { data: any }) => events.push("msg:" + e.data);
  sock.onclose = () => events.push("close");
  await sock.start();
  // allow poll loop to drain
  await new Promise((r) => setTimeout(r, 20));
  sock.send('{"method":"init","data":{}}');
  await new Promise((r) => setTimeout(r, 20));
  assert.deepEqual(events[0], "open");
  assert.ok(events.includes('msg:{"config":{}}'));
  assert.ok(events.includes("close"));
  assert.equal(sent.length, 1);
  assert.equal(sent[0].frames[0].data, '{"method":"init","data":{}}');
});
```

Run: `npx tsx --test srcts/src/mcp/__tests__/tunnel-socket.test.ts` → FAIL (module missing).

- [x] **Step 3: Implement `srcts/src/mcp/tunnel-socket.ts`:**

```ts
export type ToolCaller = (
  name: string,
  args: Record<string, unknown>
) => Promise<Record<string, unknown>>;

type Frame = { data: string; binary?: boolean };
type ReceiveResult = { frames?: Frame[]; closed?: boolean };

function b64ToArrayBuffer(b64: string): ArrayBuffer {
  const bin = atob(b64);
  const bytes = new Uint8Array(bin.length);
  for (let i = 0; i < bin.length; i++) bytes[i] = bin.charCodeAt(i);
  return bytes.buffer;
}

function arrayBufferToB64(buf: ArrayBuffer): string {
  const bytes = new Uint8Array(buf);
  let bin = "";
  for (let i = 0; i < bytes.length; i++) bin += String.fromCharCode(bytes[i]);
  return btoa(bin);
}

export class McpTunnelWebSocket {
  readyState = 0; // CONNECTING
  binaryType = "arraybuffer";
  allowReconnect = false;

  onopen: (() => void) | null = null;
  onmessage: ((event: { data: string | ArrayBuffer }) => void) | null = null;
  onclose: ((event: { code: number }) => void) | null = null;
  onerror: ((event: unknown) => void) | null = null;

  private connectionId: string | null = null;

  constructor(private callTool: ToolCaller) {}

  async start(): Promise<void> {
    try {
      const result = await this.callTool("_shiny_connect", {});
      this.connectionId = String(result.connectionId);
      this.readyState = 1; // OPEN
      this.onopen?.();
      void this.pollLoop();
    } catch (err) {
      this.fail(err);
    }
  }

  send(data: string | ArrayBuffer): void {
    if (this.readyState !== 1 || this.connectionId === null) return;
    const frame: Frame =
      typeof data === "string"
        ? { data, binary: false }
        : { data: arrayBufferToB64(data), binary: true };
    this.callTool("_shiny_send", {
      connectionId: this.connectionId,
      frames: [frame],
    }).catch((err) => this.fail(err));
  }

  close(code = 1000): void {
    if (this.readyState >= 2) return;
    this.readyState = 2; // CLOSING
    const id = this.connectionId;
    this.connectionId = null;
    if (id !== null) {
      void this.callTool("_shiny_close", { connectionId: id }).catch(() => {
        /* already closing */
      });
    }
    this.finishClose(code);
  }

  private async pollLoop(): Promise<void> {
    while (this.readyState === 1 && this.connectionId !== null) {
      let result: ReceiveResult;
      try {
        result = (await this.callTool("_shiny_receive", {
          connectionId: this.connectionId,
        })) as ReceiveResult;
      } catch (err) {
        this.fail(err);
        return;
      }
      for (const frame of result.frames ?? []) {
        if (this.readyState !== 1) break;
        const data = frame.binary ? b64ToArrayBuffer(frame.data) : frame.data;
        this.onmessage?.({ data });
      }
      if (result.closed) {
        this.finishClose(1000);
        return;
      }
    }
  }

  private fail(err: unknown): void {
    this.onerror?.(err);
    this.finishClose(1006);
  }

  private finishClose(code: number): void {
    if (this.readyState === 3) return;
    this.readyState = 3; // CLOSED
    this.onclose?.({ code });
  }
}
```

- [x] **Step 4: Run TS test, verify pass** — `npx tsx --test srcts/src/mcp/__tests__/tunnel-socket.test.ts`.

- [x] **Step 5: Implement `srcts/src/mcp/index.ts`** (adjust to the real ext-apps API found in Step 1):

```ts
import { App } from "@modelcontextprotocol/ext-apps";
import { McpTunnelWebSocket, type ToolCaller } from "./tunnel-socket";

declare global {
  interface Window {
    Shiny?: { createSocket?: () => unknown };
  }
}

async function main(): Promise<void> {
  const app = new App({ name: "shiny", version: "0.1.0" });

  const callTool: ToolCaller = async (name, args) => {
    const result = await app.callServerTool({ name, arguments: args });
    if (result.isError) {
      const text = result.content?.find((c: any) => c.type === "text")?.text;
      throw new Error(text ?? "MCP tool error");
    }
    return (result.structuredContent ?? {}) as Record<string, unknown>;
  };

  let socket: McpTunnelWebSocket | null = null;

  // Install before Shiny's DOM-ready initialization runs.
  if (window.Shiny) {
    window.Shiny.createSocket = () => {
      socket = new McpTunnelWebSocket(callTool);
      // start() is async; shinyapp assigns handlers right after createSocket
      // returns, so defer to the next macrotask.
      setTimeout(() => void socket?.start(), 0);
      return socket;
    };
  }

  // Graceful teardown (verify exact API name in the SDK).
  app.onteardown = async () => {
    socket?.close();
    return {};
  };

  await app.connect();
}

void main();
```

- [x] **Step 6: Build script `srcts/build/mcp-bridge.ts`:**

```ts
import type { BuildOptions } from "esbuild";
import { banner, build, outDir } from "./_build";

const opts: BuildOptions = {
  entryPoints: ["srcts/src/mcp/index.ts"],
  bundle: true,
  sourcemap: false,
  format: "iife",
  banner: banner,
};

// eslint-disable-next-line @typescript-eslint/no-floating-promises
build({ ...opts, outfile: outDir + "shiny-mcp-bridge.js" });
```

`package.json`: add `"bundle_mcp": "tsx srcts/build/mcp-bridge.ts"` and append `&& npm run bundle_mcp` to the `build` script. Run `npm run bundle_mcp`; verify `inst/www/shared/shiny-mcp-bridge.js` is a non-placeholder bundle.

- [x] **Step 7: Re-run R test file test-mcp-app.R** (bridge now real; assertions still pass).

- [x] **Step 8: Commit** — `git add srcts package.json package-lock.json inst/www/shared/shiny-mcp-bridge.js && git commit -m "feat(mcp): postMessage bridge bundle overriding Shiny.createSocket"`

---

### Task 6: End-to-end verification with basic-host

**Files:** none committed (scratch demo app + local clone of ext-apps).

- [x] **Step 1:** Scratch app `/tmp/mcp-demo/app.R`:

```r
library(shiny)
mcpConfigure()
shinyApp(
  ui = fluidPage(
    titlePanel("MCP demo"),
    sliderInput("n", "Observations", 10, 500, 100),
    plotOutput("hist"),
    textOutput("txt")
  ),
  server = function(input, output) {
    output$hist <- renderPlot(hist(rnorm(input$n), col = "steelblue"))
    output$txt <- renderText(paste("n =", input$n))
  }
)
```

Run: `Rscript -e 'shiny::runApp("/tmp/mcp-demo", port = 7788)'` (background).

- [x] **Step 2:** Smoke-test endpoint with curl: `initialize`, `tools/list`, `resources/read` (assert HTML > 100 KB and contains `createSocket`), `_shiny_connect` → `_shiny_send` init → `_shiny_receive` returns config frame.

- [x] **Step 3:** `git clone https://github.com/modelcontextprotocol/ext-apps /tmp/ext-apps && cd /tmp/ext-apps/examples/basic-host && npm install && SERVERS='["http://127.0.0.1:7788/mcp"]' npm start` (port 8080).

- [x] **Step 4:** Browser automation: open `http://localhost:8080`, call the `open_shiny_app` tool, verify the iframe renders the slider + histogram, move the slider, verify plot + `n = …` text update reactively. Screenshot for the report.

- [x] **Step 5:** Fix any fallout found (iterate Tasks 1-5 tests alongside), commit fixes.

---

### Task 7: NEWS + final checks

- [x] **Step 1:** Add NEWS.md bullet under the development version:
  `* Experimental: Shiny apps can be served as MCP Apps (SEP-1865). Calling mcpConfigure() mounts a Model Context Protocol endpoint at /mcp that lets MCP hosts (Claude Desktop, VS Code, MCPJam, ...) render the live app in a sandboxed iframe. (#XXXX)`
- [x] **Step 2:** Run full R test suite (`devtools::test()`) and `npm run test_types`; fix regressions.
- [x] **Step 3:** Commit — `git commit -am "docs(mcp): NEWS entry for experimental MCP Apps support"`.

---

## Self-review notes

- Spec coverage: endpoint (T1/T2), tunnel + long-poll + GC (T3), single-file resource + bridge injection (T4), bridge/createSocket override + build (T5), E2E success criterion (T6), docs (T7). Phase-2 items (uploads, downloads, dataobj, host theming, tool-input) intentionally out of scope per spec.
- Known judgment calls: no piggyback of frames on `_shiny_send` responses (strict ordering via single in-flight `_shiny_receive`); unresolvable asset refs are dropped rather than left dangling; shared-secret apps unsupported over the tunnel (documented).
- The ext-apps `App` API names (`callServerTool`, teardown hook) must be verified against the installed package in Task 5 Step 1 and code adjusted accordingly.
