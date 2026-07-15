#' Register an MCP tool the model can call directly
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' When a Shiny app is served as an MCP App (`options(shiny.mcp = TRUE)`),
#' `registerMcpTool()` exposes plain R functions as MCP tools the model may
#' call directly, in addition to the tool that opens the app. Define each
#' tool with [ellmer::tool()] and register it at the top level of your
#' `app.R`, before the app starts:
#'
#' ```r
#' library(shiny)
#' options(shiny.mcp = TRUE)
#'
#' registerMcpTool(
#'   ellmer::tool(
#'     function(n) {
#'       x <- rnorm(n)
#'       list(n = n, mean = mean(x), sd = stats::sd(x))
#'     },
#'     name = "get_sample_stats",
#'     description = "Summary statistics for a normal sample of size n.",
#'     arguments = list(n = ellmer::type_integer("Sample size", required = TRUE))
#'   )
#' )
#' ```
#'
#' Handlers run in the server R process, outside of any Shiny session, and
#' may return a character vector (sent as text), a list (sent as JSON
#' `structuredContent`), or a promise. Errors are reported to the model as
#' tool errors.
#'
#' Tools are keyed by their `name`; registering a tool whose name is already
#' registered replaces it. A tool may not use a name reserved by Shiny's MCP
#' server (the app-opening tool or the internal `_shiny_*` tunnel tools).
#'
#' @param ... One or more tools created with [ellmer::tool()].
#'
#' @return Invisibly, `NULL`.
#'
#' @seealso [mcpToolInput()] and the other `mcp*` session helpers.
#' @export
registerMcpTool <- function(...) {
  rlang::check_installed("ellmer", "to define MCP tools with `ellmer::tool()`.")
  tools <- list(...)
  reserved <- mcpReservedToolNames()
  for (tool in tools) {
    if (!inherits(tool, "ellmer::ToolDef")) {
      stop(
        "`registerMcpTool()` accepts tools created with `ellmer::tool()`; ",
        "received an object of class '", class(tool)[1], "'.",
        call. = FALSE
      )
    }
    name <- S7::prop(tool, "name")
    if (name %in% reserved) {
      stop(
        "Cannot register an MCP tool named '", name, "': that name is ",
        "reserved by Shiny's MCP server.",
        call. = FALSE
      )
    }
    .globals$mcpAuthorTools[[name]] <- tool
  }
  invisible()
}
