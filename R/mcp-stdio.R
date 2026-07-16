# Stdio transport for the MCP Apps endpoint (see mcp-server.R).
#
# With `mcpConfigure(stdio = TRUE)`, the running app also speaks MCP over
# stdin/stdout — newline-delimited JSON-RPC, the framing local desktop
# hosts (Claude Desktop, `claude mcp add` without --transport http) use
# when they launch a server process themselves:
#
#   { "mcpServers": { "my-app": {
#       "command": "Rscript",
#       "args": ["-e", "mcpConfigure(stdio = TRUE); shiny::runApp('path/to/app', launch.browser=FALSE)"]
#   } } }
#
# stdin is read non-blockingly from a `later`-scheduled poll loop, so it
# coexists with the httpuv/reactive event loop that runApp() drives; both
# transports dispatch through the same mcpDispatch(). stdout is reserved
# for protocol messages — app code must not print to it (use message(),
# which goes to stderr). Not supported on Windows (non-blocking stdin).

mcpStdioEnabled <- function() {
  isTRUE(.globals$mcp$stdio)
}

mcpStdioHandleLine <- function(line, dispatch, write) {
  if (!nzchar(trimws(line))) {
    return(invisible())
  }

  body <- tryCatch(
    safeFromJSON(line, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(body) || !is.list(body)) {
    write(mcpStdioSerialize(mcpError(NULL, -32700L, "Parse error")))
    return(invisible())
  }

  out <- tryCatch(
    dispatch(body),
    error = function(e) mcpError(body$id, -32603L, conditionMessage(e))
  )
  if (is.null(out)) {
    # Notification: no response
    return(invisible())
  }
  hybrid_chain(
    out,
    function(response) {
      write(mcpStdioSerialize(response))
    },
    catch = function(e) {
      write(mcpStdioSerialize(
        mcpError(body$id, -32603L, conditionMessage(e))
      ))
    }
  )
  invisible()
}

mcpStdioSerialize <- function(payload) {
  as.character(jsonlite::toJSON(
    payload,
    auto_unbox = TRUE, null = "null", force = TRUE
  ))
}

# Start the stdin poll loop. Returns a function that stops it.
mcpStdioStart <- function(dispatch, pollSecs = 0.05) {
  if (.Platform$OS.type == "windows") {
    warning(
      "options(shiny.mcp.stdio) is not supported on Windows ",
      "(non-blocking stdin); use the HTTP transport at /mcp instead.",
      call. = FALSE
    )
    return(invisible(function() invisible()))
  }

  con <- file("stdin", open = "r", blocking = FALSE)
  out <- stdout()
  stopped <- FALSE

  write <- function(x) {
    writeLines(x, con = out)
    flush(out)
  }

  poll <- function() {
    if (stopped) {
      return(invisible())
    }
    lines <- tryCatch(
      readLines(con, n = 100L, warn = FALSE),
      error = function(e) character(0)
    )
    for (line in lines) {
      tryCatch(
        mcpStdioHandleLine(line, dispatch, write),
        error = function(e) {
          message("shiny MCP stdio: error handling message: ",
                  conditionMessage(e))
        }
      )
    }
    later::later(poll, pollSecs)
    invisible()
  }
  later::later(poll, 0)

  invisible(function() {
    stopped <<- TRUE
    close(con)
    invisible()
  })
}
