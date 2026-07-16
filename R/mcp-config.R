# MCP App configuration. Replaces the former options(shiny.mcp.*): a single
# validated entry point that stores a defaulted config list in .globals$mcp.
# An app is served over MCP only if it calls mcpConfigure(enabled = TRUE)
# (the default); with no call at all, MCP is never enabled.

MCP_DISPLAY_MODES <- c("inline", "fullscreen", "pip")

#' Configure a Shiny app as an MCP App
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Call `mcpConfigure()` at the top level of your app (in `app.R`, `global.R`,
#' or before [runApp()]) to serve it as an MCP App (SEP-1865). An app is
#' exposed over MCP **only** when it calls `mcpConfigure()` with
#' `enabled = TRUE` (the default); an app that never calls `mcpConfigure()` is
#' never served over MCP.
#'
#' @param appId Optional unique identifier (letters, digits, `_`, `-`) so a
#'   gateway can merge several Shiny MCP endpoints. Derives the app-opening
#'   tool name (`open_<appId>_app`) and the UI resource URI
#'   (`ui://shiny/<appId>`).
#' @param description Human/model-readable description of the app-opening tool;
#'   the model reads it to decide whether to open the app.
#' @param arguments Optional named list of \pkg{ellmer} type objects (e.g.
#'   [ellmer::type_integer()]) declaring the arguments the model may pass when
#'   opening the app. Published as the tool's input schema and used as an
#'   allow-list. Read within the app with [mcpUpdates()] (post-init) or via
#'   input/bookmark restoration (init).
#' @param direct Whether to advertise the direct-connect fast path (default
#'   `TRUE`).
#' @param displayModes Display modes the app supports; a subset of
#'   `c("inline", "fullscreen", "pip")`.
#' @param origin Optional explicit external base URL for the direct-connect
#'   fast path (may include a path).
#' @param stdio Whether to also speak MCP over stdin/stdout (default `FALSE`;
#'   not supported on Windows).
#' @param enabled Whether MCP serving is enabled (default `TRUE`). Placed last
#'   so typical calls lead with the meaningful config.
#'
#' @return Invisibly, the stored configuration list.
#' @seealso [mcpUpdates()] and the other `mcp*` session helpers;
#'   [registerMcpTool()] for model-callable tools.
#' @export
mcpConfigure <- function(
  appId = NULL,
  description = NULL,
  arguments = NULL,
  direct = TRUE,
  displayModes = c("inline", "fullscreen", "pip"),
  origin = NULL,
  stdio = FALSE,
  enabled = TRUE
) {
  assertMcpFlag(enabled, "enabled")
  assertMcpFlag(direct, "direct")
  assertMcpFlag(stdio, "stdio")
  assertMcpOptionalString(description, "description")
  assertMcpOptionalString(origin, "origin")
  appId <- assertMcpAppId(appId)
  arguments <- assertMcpArguments(arguments)

  modes <- intersect(as.character(displayModes), MCP_DISPLAY_MODES)
  if (length(modes) == 0) modes <- "inline"

  .globals$mcp <- list(
    enabled = enabled,
    appId = appId,
    description = description,
    arguments = arguments,
    direct = direct,
    displayModes = modes,
    origin = origin,
    stdio = stdio
  )
  invisible(.globals$mcp)
}

assertMcpFlag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1 || is.na(x)) {
    stop("`", name, "` must be a single TRUE or FALSE.", call. = FALSE)
  }
}

assertMcpOptionalString <- function(x, name) {
  if (is.null(x)) return(invisible(NULL))
  if (!is.character(x) || length(x) != 1 || is.na(x)) {
    stop("`", name, "` must be NULL or a single string.", call. = FALSE)
  }
}

assertMcpAppId <- function(appId) {
  if (is.null(appId)) return(NULL)
  if (!is.character(appId) || length(appId) != 1 || is.na(appId) || !nzchar(appId)) {
    stop("`appId` must be NULL or a single non-empty string.", call. = FALSE)
  }
  if (!grepl("^[A-Za-z0-9_-]+$", appId)) {
    stop(
      "`appId` may only contain letters, digits, '_' and '-'.",
      call. = FALSE
    )
  }
  appId
}

assertMcpArguments <- function(arguments) {
  if (is.null(arguments)) return(NULL)
  if (!is.list(arguments) || length(arguments) == 0) {
    stop("`arguments` must be NULL or a non-empty named list.", call. = FALSE)
  }
  nms <- names(arguments)
  if (is.null(nms) || any(!nzchar(nms))) {
    stop("`arguments` must be a named list.", call. = FALSE)
  }
  rlang::check_installed("ellmer", "to declare MCP tool `arguments`.")
  for (el in arguments) {
    if (!inherits(el, "ellmer::Type")) {
      stop(
        "Each element of `arguments` must be an ellmer type ",
        "(e.g. `ellmer::type_integer()`); received an object of class '",
        class(el)[1], "'.",
        call. = FALSE
      )
    }
  }
  arguments
}

# open_shiny_app, or open_<appId>_app when an appId is configured.
mcpToolName <- function() {
  id <- .globals$mcp$appId
  if (is.null(id)) "open_shiny_app" else paste0("open_", id, "_app")
}

# Convert the configured `arguments` (named list of ellmer types) into the
# JSON Schema published as the tool's inputSchema. Mirrors
# mcpToolInputSchema() (R/mcp-server.R) so the two conversions stay aligned.
mcpArgumentsSchema <- function(arguments) {
  if (is.null(arguments) || length(arguments) == 0) {
    return(list(type = "object", properties = empty_named_list()))
  }
  obj <- rlang::exec(ellmer::type_object, !!!arguments)
  as_json <- getNamespace("ellmer")[["as_json"]]
  schema <- as_json(ellmer::Provider("dummy", "dummy", "dummy"), obj)
  schema$description <- NULL
  if (is.null(schema$properties)) {
    schema$properties <- empty_named_list()
  }
  dropNulls(schema)
}
