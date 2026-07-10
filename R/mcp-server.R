# Experimental support for serving a Shiny app as an MCP App (SEP-1865).
#
# When `options(shiny.mcp = TRUE)` is set before the app starts, a Model
# Context Protocol endpoint (JSON-RPC over streamable HTTP, JSON responses)
# is mounted at `/mcp` on the app's own httpuv server. MCP hosts that
# support the Apps extension (`io.modelcontextprotocol/ui`) can then call
# the app's tool and render the live application in a sandboxed iframe;
# reactivity is tunneled over postMessage -> tools/call (see mcp-tunnel.R
# and srcts/src/mcp/).

MCP_PROTOCOL_VERSIONS <- c("2024-11-05", "2025-03-26", "2025-06-18")
MCP_RESOURCE_URI <- "ui://shiny/app"
MCP_RESOURCE_MIME <- "text/html;profile=mcp-app"

mcpEnabled <- function() {
  isTRUE(getOption("shiny.mcp", FALSE))
}

mcpToolInfo <- function() {
  opt <- getOption("shiny.mcp.tool", list())
  list(
    name = opt$name %||% "open_shiny_app",
    description = opt$description %||% paste(
      "Open the interactive Shiny application so the user can view and",
      "interact with it. Call this when the user wants to see the app."
    )
  )
}

mcpCorsHeaders <- function() {
  list(
    "Access-Control-Allow-Origin" = "*",
    "Access-Control-Allow-Methods" = "GET, POST, OPTIONS",
    "Access-Control-Allow-Headers" = paste(
      "Content-Type, Authorization, Mcp-Session-Id, Mcp-Protocol-Version,",
      "Last-Event-Id"
    )
  )
}

mcpJsonResponse <- function(payload, status = 200L) {
  httpResponse(
    status = status,
    content_type = "application/json",
    content = enc2utf8(as.character(jsonlite::toJSON(
      payload,
      auto_unbox = TRUE, null = "null", force = TRUE
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

# Returns a Shiny HTTP handler serving the MCP endpoint at /mcp.
#
# `uiHandler` renders the app's UI page (used for resources/read);
# `wsHandler` is the app's websocket handler from createAppHandlers(),
# which accepts any object satisfying the httpuv WebSocket duck type
# (used by _shiny_connect to open tunneled sessions).
mcpHttpHandler <- function(uiHandler, wsHandler) {
  force(uiHandler)
  force(wsHandler)

  function(req) {
    path <- sub("/$", "", req$PATH_INFO %||% "")
    if (!identical(path, "/mcp")) {
      return(NULL)
    }

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

    body <- tryCatch(
      {
        raw <- req$rook.input$read()
        safeFromJSON(rawToChar(raw), simplifyVector = FALSE)
      },
      error = function(e) NULL
    )
    if (is.null(body) || !is.list(body)) {
      return(mcpJsonResponse(mcpError(NULL, -32700L, "Parse error"), 400L))
    }

    out <- tryCatch(
      mcpDispatch(body, uiHandler, wsHandler),
      error = function(e) mcpError(body$id, -32603L, conditionMessage(e))
    )
    if (is.null(out)) {
      # Notification: no response body
      return(httpResponse(
        202L, "application/json", "",
        headers = mcpCorsHeaders()
      ))
    }
    hybrid_chain(out, mcpJsonResponse)
  }
}

mcpDispatch <- function(body, uiHandler, wsHandler) {
  method <- body$method
  if (!is.character(method) || length(method) != 1) {
    return(mcpError(body$id, -32600L, "Invalid Request"))
  }
  if (grepl("^notifications/", method)) {
    return(NULL)
  }

  switch(method,
    "initialize" = mcpResult(body$id, mcpInitializeResult(body)),
    "ping" = mcpResult(body$id, empty_named_list()),
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
  c(
    list(list(
      name = info$name,
      description = info$description,
      inputSchema = list(type = "object", properties = empty_named_list()),
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
    return(mcpTunnelToolCall(
      name, body$params$arguments %||% empty_named_list(), body$id, wsHandler
    ))
  }
  mcpError(body$id, -32602L, sprintf("Unknown tool: %s", name))
}

mcpResourcesList <- function() {
  list()
}

mcpResourcesRead <- function(body, uiHandler) {
  mcpError(body$id, -32602L, "Unknown resource")
}
