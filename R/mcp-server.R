# Experimental support for serving a Shiny app as an MCP App (SEP-1865).
#
# When `mcpConfigure()` is called before the app starts, a Model Context
# Protocol endpoint (JSON-RPC over streamable HTTP, JSON responses) is
# mounted at `/mcp` on the app's own httpuv server. MCP hosts that support
# the Apps extension (`io.modelcontextprotocol/ui`) can then call the app's
# tool and render the live application in a sandboxed iframe; reactivity is
# tunneled over postMessage -> tools/call (see mcp-tunnel.R and
# srcts/src/mcp/).

MCP_PROTOCOL_VERSIONS <- c("2024-11-05", "2025-03-26", "2025-06-18")
MCP_RESOURCE_URI <- "ui://shiny/app"
MCP_RESOURCE_MIME <- "text/html;profile=mcp-app"

mcpEnabled <- function() {
  isTRUE(.globals$mcp$enabled)
}

# Direct-connect fast path: declare the app's origin in the resource CSP so
# spec-compliant hosts let the iframe open a real WebSocket to the app,
# skipping the tools/call tunnel for reactivity. The bridge feature-detects
# and falls back to the tunnel, so this is safe to leave on even for hosts
# that ignore declared CSP (e.g. claude.ai today).
mcpDirectEnabled <- function() {
  isTRUE(.globals$mcp$direct)
}

mcpDisplayModes <- function() {
  .globals$mcp$displayModes %||% "inline"
}

# Optional app identity (validated in mcpConfigure()).
mcpAppId <- function() {
  .globals$mcp$appId
}

mcpResourceUri <- function() {
  id <- mcpAppId()
  if (is.null(id)) MCP_RESOURCE_URI else paste0("ui://shiny/", id)
}

# "_shiny_connect" -> "<appId>_shiny_connect" when an appId is set.
mcpTunnelToolName <- function(base) {
  id <- mcpAppId()
  if (is.null(id)) base else paste0(id, base)
}

# Inverse of mcpTunnelToolName(): strip the appId prefix from an incoming
# tools/call name so dispatch can match the canonical `_shiny_*` names.
mcpTunnelLocalName <- function(name) {
  id <- mcpAppId()
  if (!is.null(id) && startsWith(name, paste0(id, "_shiny_"))) {
    return(substring(name, nchar(id) + 1))
  }
  name
}

# The app's externally reachable base URL (may include a path, e.g. Posit
# Connect's /content/<guid>), for the direct-connect fast path. Priority:
#
# 1. mcpConfigure(origin=) — explicit full URL, for nonstandard proxies.
# 2. RStudio-Connect-App-Base-Url — Posit Connect sends the app's external
#    base URL (https://<server>/content/<guid>) with every request it
#    proxies to Shiny content. Verified against a real Connect deployment.
# 3. X-RSC-Request — the full external URL of the request, sent by Connect
#    to API content; strip the trailing /mcp to get the content base.
# 4. X-Redx-Frontend-Name — shinyapps.io sends the external host + path
#    (no scheme, e.g. "barret.shinyapps.io/my-app/"); combine with
#    X-Forwarded-Proto. Verified against a real shinyapps.io deployment.
# 5. rsconnect deployment records (rsconnect/**/*.dcf next to the app) —
#    the "output files" written by deployment carry the content `url`. Only
#    trusted when the record's host matches the request's Host header, so a
#    local run of a deployed app dir never points the iframe at production.
# 6. Origin derived from the request (X-Forwarded-Host falling back to
#    Host, + X-Forwarded-Proto) — right for localhost and root-mounted
#    https tunnels, but pathless.
# 7. The local httpuv origin (stdio transport has no request).
mcpDirectBase <- function(req) {
  reqHeader <- function(name) {
    value <- tryCatch(req[[name]], error = function(e) NULL)
    if (is.character(value) && nzchar(value %||% "")) value else NULL
  }
  stripSlash <- function(x) sub("/+$", "", x)

  origin_opt <- .globals$mcp$origin
  if (is.character(origin_opt) && nzchar(origin_opt)) {
    return(stripSlash(origin_opt))
  }

  connect_base <- reqHeader("HTTP_RSTUDIO_CONNECT_APP_BASE_URL")
  if (!is.null(connect_base)) {
    return(stripSlash(connect_base))
  }

  rsc <- reqHeader("HTTP_X_RSC_REQUEST")
  if (!is.null(rsc)) {
    return(stripSlash(sub("/mcp/?$", "", rsc)))
  }

  proto <- reqHeader("HTTP_X_FORWARDED_PROTO") %||% "http"

  redx <- reqHeader("HTTP_X_REDX_FRONTEND_NAME")
  if (!is.null(redx)) {
    return(stripSlash(sprintf("%s://%s", proto, redx)))
  }

  host <- reqHeader("HTTP_X_FORWARDED_HOST") %||% reqHeader("HTTP_HOST")
  deployed <- mcpDeployedUrl(
    getShinyOption("appDir", default = getwd()),
    host
  )
  if (!is.null(deployed)) {
    return(deployed)
  }

  if (is.null(host)) {
    return(.globals$mcpLocalOrigin)
  }
  sprintf("%s://%s", proto, host)
}

# Look up the deployed URL for this app from deployment records written
# next to the app, requiring an exact host match against the serving
# request. Sources, in order:
# - {rsconnect} records:      rsconnect/**/*.dcf            (field `url`)
# - Posit Publisher records:  .posit/publish/deployments/*.toml (`direct_url`)
mcpDeployedUrl <- function(appDir, host) {
  if (is.null(host) || !nzchar(host %||% "")) {
    return(NULL)
  }
  candidates <- c(
    mcpRsconnectRecordUrls(appDir),
    mcpPublisherRecordUrls(appDir)
  )
  for (url in candidates) {
    if (!is.character(url) || is.na(url) || !nzchar(url)) next
    url_host <- sub("^https?://([^/]+).*$", "\\1", url)
    if (identical(url_host, host)) {
      return(sub("/+$", "", url))
    }
  }
  NULL
}

mcpRsconnectRecordUrls <- function(appDir) {
  records <- tryCatch(
    list.files(
      file.path(appDir, "rsconnect"),
      pattern = "\\.dcf$", recursive = TRUE, full.names = TRUE
    ),
    error = function(e) character(0)
  )
  vapply(records, function(record) {
    tryCatch(
      unname(read.dcf(record, fields = "url")[1, "url"]),
      error = function(e) NA_character_
    )
  }, character(1), USE.NAMES = FALSE)
}

# Posit Publisher's deployment records are TOML; extract `direct_url`
# ("the link to use when accessing a deployed ...") without a TOML parser.
mcpPublisherRecordUrls <- function(appDir) {
  records <- tryCatch(
    list.files(
      file.path(appDir, ".posit", "publish", "deployments"),
      pattern = "\\.toml$", full.names = TRUE
    ),
    error = function(e) character(0)
  )
  urls <- character(0)
  for (record in records) {
    lines <- tryCatch(
      readLines(record, warn = FALSE),
      error = function(e) character(0)
    )
    hits <- regmatches(
      lines,
      regexpr("^\\s*direct_url\\s*=\\s*['\"]([^'\"]+)['\"]", lines)
    )
    for (hit in hits) {
      urls <- c(urls, sub("^\\s*direct_url\\s*=\\s*['\"]([^'\"]+)['\"].*$", "\\1", hit))
    }
  }
  urls
}

# CSP source expressions are origins; extract one from a (possibly
# path-bearing) base URL.
mcpBaseOrigin <- function(base) {
  m <- regmatches(base, regexpr("^https?://[^/]+", base))
  if (length(m) == 0) base else m
}

mcpWsOrigin <- function(origin) {
  sub("^http", "ws", origin)
}

mcpToolInfo <- function() {
  list(
    name = mcpToolName(),
    description = .globals$mcp$description %||% paste(
      "Open the interactive Shiny application so the user can view and",
      "interact with it. Call this when the user wants to see the app."
    ),
    inputSchema = mcpArgumentsSchema(.globals$mcp$arguments)
  )
}

# inputSchema for update_<appId>_app: the declared arguments allow-list plus a
# required `session` string identifying the running instance to update.
mcpUpdateToolInfo <- function() {
  schema <- mcpArgumentsSchema(.globals$mcp$arguments)
  schema$properties$session <- list(
    type = "string",
    description = paste(
      "Id of the running app instance to update, as reported by the app when",
      "it opened. Required."
    )
  )
  schema$required <- as.list(unique(c(unlist(schema$required), "session")))
  list(
    name = mcpUpdateToolName(),
    description = paste(
      "Change parameters of the already-open app in place. Do NOT re-open the",
      "app; use this to update the running instance identified by `session`."
    ),
    inputSchema = schema
  )
}

# TRUE when a companion update_* tool should be published (author declared
# arguments the model may push into a running session).
mcpHasUpdateTool <- function() {
  length(.globals$mcp$arguments) > 0
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
      mcpDispatch(body, uiHandler, wsHandler, req = req),
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

mcpDispatch <- function(body, uiHandler, wsHandler, req = NULL) {
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
    "resources/read" = mcpResourcesRead(body, uiHandler, req),
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
  update_entry <- if (mcpHasUpdateTool()) {
    upd <- mcpUpdateToolInfo()
    list(list(
      name = upd$name,
      description = upd$description,
      inputSchema = upd$inputSchema
    ))
  } else {
    list()
  }
  # unname: a partially named list would serialize as a JSON object rather
  # than the array that tools/list requires.
  unname(c(
    list(list(
      name = info$name,
      description = info$description,
      inputSchema = info$inputSchema,
      `_meta` = list(ui = list(resourceUri = mcpResourceUri()))
    )),
    update_entry,
    mcpTunnelToolsList(),
    lapply(mcpAuthorTools(), function(tool) {
      dropNulls(list(
        name = S7::prop(tool, "name"),
        title = S7::prop(tool, "annotations")$title,
        description = S7::prop(tool, "description"),
        inputSchema = mcpToolInputSchema(tool)
      ))
    })
  ))
}

mcpReservedToolNames <- function() {
  c(
    mcpToolInfo()$name,
    if (mcpHasUpdateTool()) mcpUpdateToolName(),
    vapply(mcpTunnelToolsList(), function(t) t$name, character(1))
  )
}

# Author-declared tools registered via registerMcpTool(). Named list of
# ellmer::ToolDef objects, keyed by tool name; validation happens eagerly at
# registration time.
mcpAuthorTools <- function() {
  .globals$mcpAuthorTools %||% list()
}

# Convert an ellmer ToolDef's typed @arguments into a JSON Schema list for
# tools/list. Mirrors mcptools::tool_as_json() so the two stay aligned: a
# dummy provider drives ellmer's provider-agnostic base as_json() method.
mcpToolInputSchema <- function(tool) {
  as_json <- getNamespace("ellmer")[["as_json"]]
  schema <- as_json(ellmer::Provider("dummy", "dummy", "dummy"), S7::prop(tool, "arguments"))
  schema$description <- NULL
  if (is.null(schema$properties)) {
    schema$properties <- empty_named_list()
  }
  dropNulls(schema)
}

# Convert a handler's return value into an MCP CallToolResult.
mcpAuthorToolResult <- function(id, value) {
  if (is.null(value)) {
    return(mcpResult(id, list(
      content = list(list(type = "text", text = "ok"))
    )))
  }
  if (is.character(value) || is.numeric(value) || is.logical(value)) {
    return(mcpResult(id, list(
      content = list(list(
        type = "text",
        text = paste(as.character(value), collapse = "\n")
      ))
    )))
  }
  if (is.list(value)) {
    json <- as.character(jsonlite::toJSON(
      value,
      auto_unbox = TRUE, null = "null", force = TRUE
    ))
    return(mcpResult(id, list(
      content = list(list(type = "text", text = json)),
      structuredContent = value
    )))
  }
  mcpResult(id, list(
    content = list(list(type = "text", text = paste(
      utils::capture.output(print(value)),
      collapse = "\n"
    )))
  ))
}

mcpAuthorToolCall <- function(tool, params, id) {
  args <- params$arguments %||% list()
  hybrid_chain(
    tryCatch(rlang::exec(tool, !!!args), error = function(e) e),
    function(value) {
      if (inherits(value, "condition")) {
        return(mcpToolErrorResult(id, conditionMessage(value)))
      }
      mcpAuthorToolResult(id, value)
    },
    catch = function(e) {
      mcpToolErrorResult(id, conditionMessage(e))
    }
  )
}

# Handle a model call to update_<appId>_app: push the (allow-list-filtered)
# arguments into the running session named by `session`, inside that session's
# reactive domain so its mcpUpdates() observer re-fires.
mcpUpdateAppCall <- function(params, id) {
  args <- params$arguments %||% list()
  token <- args$session
  if (!is.character(token) || length(token) != 1 || !nzchar(token)) {
    return(mcpToolErrorResult(
      id,
      "update requires a `session` id identifying the running app instance."
    ))
  }
  session <- appsByToken$get(token)
  if (is.null(session) || !isMcpSession(session)) {
    return(mcpToolErrorResult(id, sprintf(
      paste(
        "No running app session with id '%s'. Open the app first, or use the",
        "id the app reported for the instance you want to change."
      ),
      token
    )))
  }
  pushed <- mcpFilterArguments(args[setdiff(names(args), "session")])
  withReactiveDomain(session, {
    rv <- mcpServerUpdatesFor(session)
    rv(utils::modifyList(isolate(rv()) %||% list(), pushed))
  })
  session$requestFlush()
  mcpResult(id, list(content = list(list(
    type = "text",
    text = "Updated the running app in place."
  ))))
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
  if (mcpHasUpdateTool() && identical(name, mcpUpdateToolName())) {
    return(mcpUpdateAppCall(body$params, body$id))
  }
  localName <- mcpTunnelLocalName(name)
  if (startsWith(localName, "_shiny_")) {
    return(mcpTunnelToolCall(
      localName,
      body$params$arguments %||% empty_named_list(),
      body$id,
      wsHandler
    ))
  }
  authorTool <- mcpAuthorTools()[[name]]
  if (!is.null(authorTool)) {
    return(mcpAuthorToolCall(authorTool, body$params, body$id))
  }
  mcpError(body$id, -32602L, sprintf("Unknown tool: %s", name))
}

mcpResourcesList <- function() {
  list(list(
    uri = mcpResourceUri(),
    name = "Shiny application",
    description = "The interactive Shiny application UI.",
    mimeType = MCP_RESOURCE_MIME,
    `_meta` = list(ui = list(prefersBorder = TRUE))
  ))
}

mcpResourcesRead <- function(body, uiHandler, req = NULL) {
  uri <- body$params$uri %||% ""
  if (!identical(uri, mcpResourceUri())) {
    return(mcpError(body$id, -32602L, sprintf("Unknown resource: %s", uri)))
  }

  base <- if (mcpDirectEnabled()) mcpDirectBase(req)
  config <- dropNulls(list(
    appId = mcpAppId(),
    directBase = base,
    displayModes = as.list(mcpDisplayModes())
  ))

  ui_meta <- list(prefersBorder = TRUE)
  if (!is.null(base)) {
    origin <- mcpBaseOrigin(base)
    ui_meta$csp <- list(
      connectDomains = list(origin, mcpWsOrigin(origin))
    )
  }

  html <- renderMcpAppHtml(uiHandler, config = config)
  mcpResult(body$id, list(contents = list(list(
    uri = mcpResourceUri(),
    mimeType = MCP_RESOURCE_MIME,
    text = html,
    `_meta` = list(ui = ui_meta)
  ))))
}
