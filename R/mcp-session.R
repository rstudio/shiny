#' Interact with the MCP host from a Shiny app
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' When a Shiny app is served as an MCP App (by setting
#' `options(shiny.mcp = TRUE)` before the app starts), it renders inside an
#' MCP host's conversation (e.g. Claude, Claude Desktop, VS Code Copilot)
#' and can exchange information with the host:
#'
#' * `isMcpSession()` reports whether the current session is connected
#'   through an MCP host (as opposed to a regular browser).
#' * `mcpToolInput()` returns the arguments the model supplied when it
#'   called the app's tool, as a named list (reactive read). Declare the
#'   expected arguments with
#'   `options(shiny.mcp.tool = list(inputSchema = ...))`.
#' * `mcpHostContext()` returns the host's context (theme, locale, display
#'   mode, ...) as a named list (reactive read).
#' * `mcpUpdateModelContext()` updates the model's context for future
#'   conversation turns. Each call overwrites the previous update; the host
#'   typically delivers it with the user's next message.
#' * `mcpSendMessage()` sends a user-role message to the host's chat
#'   interface, which may trigger a model response.
#'
#' Outside of an MCP session, `mcpUpdateModelContext()` and
#' `mcpSendMessage()` do nothing and return `FALSE`, so apps can call them
#' unconditionally.
#'
#' @section Deployment and the direct-connect fast path:
#' On hosts that honor the declared content security policy, the app's
#' iframe connects over a real WebSocket for native-latency reactivity,
#' falling back to the tools/call tunnel automatically. The websocket URL
#' is derived, in order, from: `options(shiny.mcp.origin = )` (a full base
#' URL, which may include a path); Posit Connect's
#' `RStudio-Connect-App-Base-Url` (or `X-RSC-Request`) header;
#' shinyapps.io's `X-Redx-Frontend-Name` header;
#' a deployment record next to the app whose URL matches the serving host
#' (rsconnect's `rsconnect/**/*.dcf` or Posit Publisher's
#' `.posit/publish/deployments/*.toml`) — so apps deployed
#' under a sub-path such as `/content/<guid>` work without configuration;
#' or the origin of the MCP request itself. Set
#' `options(shiny.mcp.direct = FALSE)` to always use the tunnel.
#'
#' @section Multiple apps behind one server:
#' A gateway can merge several Shiny MCP endpoints into a single MCP
#' server, so one connector exposes many apps. For that to work each app
#' must set a unique `options(shiny.mcp.appId = "<id>")` (letters, digits,
#' `_`, `-`): the app's internal tunnel tools are then prefixed with the
#' id and its UI resource is published as `ui://shiny/<id>`, so tools and
#' resources from different apps do not collide when merged. App-facing
#' tool names must also be unique across the merged apps.
#'
#' @section Additional tools:
#' Beyond the tool that opens the app, authors can expose plain R functions
#' as MCP tools the model may call directly, via
#' `options(shiny.mcp.tools = list(...))`. Each tool is a list with a
#' `name`, a `description`, an optional `inputSchema` (JSON Schema as a
#' nested list), and a `handler` function that receives the parsed
#' arguments as a named list. Handlers run in the server R process outside
#' of any Shiny session and may return a character vector (sent as text), a
#' list (sent as JSON `structuredContent`), or a promise. Errors are
#' reported to the model as tool errors.
#'
#' ```r
#' options(shiny.mcp.tools = list(
#'   list(
#'     name = "get_sample_stats",
#'     description = "Summary statistics for a normal sample of size n.",
#'     inputSchema = list(
#'       type = "object",
#'       properties = list(n = list(type = "integer")),
#'       required = list("n")
#'     ),
#'     handler = function(args) {
#'       x <- rnorm(args$n)
#'       list(n = args$n, mean = mean(x), sd = stats::sd(x))
#'     }
#'   )
#' ))
#' ```
#'
#' @param text A character string. For `mcpUpdateModelContext()`, the
#'   human/model-readable context text; for `mcpSendMessage()`, the message
#'   to send to the chat.
#' @param data An optional named list of structured, machine-readable
#'   context data (serialized as JSON).
#' @param session The Shiny session object.
#'
#' @return
#' `isMcpSession()` returns `TRUE` or `FALSE`. `mcpToolInput()` and
#' `mcpHostContext()` return a named list, or `NULL` when nothing has been
#' received (they must be called within a reactive context).
#' `mcpUpdateModelContext()` and `mcpSendMessage()` invisibly return `TRUE`
#' if the message was sent to the host and `FALSE` otherwise.
#'
#' @examples
#' \dontrun{
#' options(shiny.mcp = TRUE)
#' options(shiny.mcp.tool = list(
#'   name = "open_sales_dashboard",
#'   description = "Show the interactive sales dashboard.",
#'   inputSchema = list(
#'     type = "object",
#'     properties = list(
#'       region = list(type = "string", description = "Region to focus on")
#'     )
#'   )
#' ))
#'
#' server <- function(input, output, session) {
#'   # React to arguments the model passed to the tool
#'   observe({
#'     region <- mcpToolInput()$region
#'     if (!is.null(region)) {
#'       updateSelectInput(session, "region", selected = region)
#'     }
#'   })
#'
#'   # Keep the model informed about what the user is looking at
#'   observe({
#'     mcpUpdateModelContext(
#'       text = paste("The user is viewing region", input$region),
#'       data = list(region = input$region)
#'     )
#'   })
#' }
#' }
#'
#' @name mcp-session
NULL

#' @rdname mcp-session
#' @export
isMcpSession <- function(session = getDefaultReactiveDomain()) {
  if (is.null(session)) {
    return(FALSE)
  }
  req <- tryCatch(suppressWarnings(session$request), error = function(e) NULL)
  if (is.null(req)) {
    return(FALSE)
  }
  # Tunneled sessions carry a marker on the fake websocket request;
  # direct-connect sessions (a real WebSocket opened by the MCP bridge)
  # identify themselves with ?mcp=1 on the websocket URL.
  if (identical(
    tryCatch(req$HTTP_MCP_TUNNEL, error = function(e) NULL),
    "1"
  )) {
    return(TRUE)
  }
  query <- tryCatch(req$QUERY_STRING, error = function(e) NULL)
  is.character(query) && grepl("(^\\??|&)mcp=1(&|$)", query)
}

#' @rdname mcp-session
#' @export
mcpToolInput <- function(session = getDefaultReactiveDomain()) {
  if (is.null(session)) {
    stop("mcpToolInput() must be called from within a Shiny session")
  }
  mcpParseClientData(session$clientData$mcp_tool_input)
}

#' @rdname mcp-session
#' @export
mcpHostContext <- function(session = getDefaultReactiveDomain()) {
  if (is.null(session)) {
    stop("mcpHostContext() must be called from within a Shiny session")
  }
  mcpParseClientData(session$clientData$mcp_host_context)
}

mcpParseClientData <- function(json) {
  if (is.null(json) || !is.character(json) || !nzchar(json)) {
    return(NULL)
  }
  tryCatch(
    safeFromJSON(json, simplifyVector = FALSE),
    error = function(e) NULL
  )
}

#' @rdname mcp-session
#' @export
mcpUpdateModelContext <- function(
  text = NULL,
  data = NULL,
  session = getDefaultReactiveDomain()
) {
  if (is.null(text) && is.null(data)) {
    stop("mcpUpdateModelContext() requires `text` and/or `data`")
  }
  if (!isMcpSession(session)) {
    return(invisible(FALSE))
  }
  params <- dropNulls(list(
    content = if (!is.null(text)) {
      list(list(type = "text", text = text))
    },
    structuredContent = data
  ))
  session$sendCustomMessage("shiny.mcp.updateModelContext", params)
  invisible(TRUE)
}

#' @rdname mcp-session
#' @export
mcpSendMessage <- function(
  text,
  session = getDefaultReactiveDomain()
) {
  if (!is.character(text) || length(text) != 1 || !nzchar(text)) {
    stop("mcpSendMessage() requires a non-empty character string `text`")
  }
  if (!isMcpSession(session)) {
    return(invisible(FALSE))
  }
  session$sendCustomMessage("shiny.mcp.sendMessage", list(
    role = "user",
    content = list(list(type = "text", text = text))
  ))
  invisible(TRUE)
}

#' @param mode The display mode to request: `"inline"`, `"fullscreen"`, or
#'   `"pip"`. The host may decline; observe `mcpHostContext()$displayMode`
#'   for the mode actually in effect. Declare the modes the app supports
#'   with `options(shiny.mcp.displayModes = c("inline", "fullscreen"))`
#'   (default: all three).
#' @rdname mcp-session
#' @export
mcpRequestDisplayMode <- function(
  mode = c("inline", "fullscreen", "pip"),
  session = getDefaultReactiveDomain()
) {
  mode <- match.arg(mode)
  if (!isMcpSession(session)) {
    return(invisible(FALSE))
  }
  session$sendCustomMessage("shiny.mcp.requestDisplayMode", list(mode = mode))
  invisible(TRUE)
}
