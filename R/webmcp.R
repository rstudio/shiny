#' Control how agents can access an input or output
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Sets the `data-shiny-drive-access` attribute that the client-side `Shiny.drive` (and
#' the WebMCP tools built on it, see `options(shiny.webmcp = TRUE)`) use to
#' decide whether an input or output is exposed, and how.
#'
#' Without it, inputs are `"read-write"`, except password inputs
#' (`"write-only"`) and file inputs (`"read-only"`). Outputs are always
#' read-only, so only `"exclude"` applies to them.
#'
#' The attribute applies only to the input or output it is set on, not to
#' other inputs or outputs nested inside it. The reverse also holds: an
#' excluded output inside another output (e.g. a [uiOutput()]) is still part of
#' that parent output's content.
#'
#' This keeps unneeded inputs and outputs away from agents. It is not a
#' security boundary: anything shown in the page can still be read by other
#' means.
#'
#' @param tag An input (e.g. [textInput()]) or output (e.g. [plotOutput()]).
#'   For [tabsetPanel()] and similar, the attribute is set on the tab input
#'   inside it.
#' @param access One of:
#'   * `"read-only"`: the value can be read but not set.
#'   * `"write-only"`: the value can be set but not read back.
#'   * `"exclude"`: not listed, read, or set at all.
#' @return `tag`, with the `data-shiny-drive-access` attribute set.
#'
#' @examples
#' textInput("api_key", "API key") |> webMcpAccess("exclude")
#' selectInput("region", "Region", c("East", "West")) |> webMcpAccess("read-only")
#' verbatimTextOutput("debug_log") |> webMcpAccess("exclude")
#' tabsetPanel(id = "tab", tabPanel("A"), tabPanel("B")) |> webMcpAccess("read-only")
#' @export
webMcpAccess <- function(tag, access) {
  access <- match.arg(access, c("read-only", "write-only", "exclude"))
  if (!inherits(tag, "shiny.tag")) {
    stop("`tag` must be a single HTML tag, like an input or output.", call. = FALSE)
  }
  # Tabsets wrap the bound `ul.shiny-tab-input` in a `div.tabbable`
  tq <- tagQuery(tag)
  tabs <- tq$find(".shiny-tab-input")
  if (tabs$length() > 0) {
    tabs$removeAttrs("data-shiny-drive-access")$addAttrs("data-shiny-drive-access" = access)
    return(tq$allTags())
  }
  tag$attribs[["data-shiny-drive-access"]] <- access
  tag
}

# WebMCP polyfill, for browsers without a native `document.modelContext`. It
# uses the native API when present. Added to the page when
# `options(shiny.webmcp = TRUE)`. Loaded from a CDN, so offline apps get no
# polyfill (the tools still work through `Shiny.webmcp.callTool()`).
webMcpPolyfillDependency <- function() {
  version <- "6.0.3"
  htmlDependency(
    name = "mcp-b-global",
    version = version,
    src = c(href = paste0("https://unpkg.com/@mcp-b/global@", version, "/dist")),
    script = "index.iife.js"
  )
}
