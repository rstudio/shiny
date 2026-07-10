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
  identical(
    tryCatch(req$HTTP_MCP_TUNNEL, error = function(e) NULL),
    "1"
  )
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
