# The MCP Apps tunnel: a fake "websocket" satisfying the duck type that
# ShinySession/createAppHandlers() expect ($send, $close, $request,
# $onMessage, $onClose), backed by frame queues that the app iframe drives
# through the app-only `_shiny_*` tools (see mcp-server.R).
#
# Client -> server frames arrive via `_shiny_send` and are fed to the
# onMessage callbacks. Server -> client frames (session output) are queued
# and drained by `_shiny_receive`, which long-polls: it resolves as soon as
# frames are available, or after `timeoutSecs` with an empty batch (the
# client immediately re-polls). Ordering is guaranteed by the client keeping
# a single `_shiny_receive` in flight.

McpConnection <- R6::R6Class(
  "McpConnection",
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
      if (private$closed_) {
        return(invisible())
      }
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
      # Server-initiated close (e.g. session$close()); tell the client via
      # the `closed` flag on the next receive, and run onClose callbacks.
      if (private$closed_) {
        return(invisible())
      }
      private$closed_ <- TRUE
      private$resolvePending()
      private$fireClose()
      invisible()
    },
    onMessage = function(cb) {
      private$messageCallbacks_ <- c(private$messageCallbacks_, cb)
      invisible()
    },
    onClose = function(cb) {
      private$closeCallbacks_ <- c(private$closeCallbacks_, cb)
      invisible()
    },

    # ---- tunnel side ----
    dispatchFrames = function(frames) {
      self$lastActivity <- Sys.time()
      for (frame in frames) {
        binary <- isTRUE(frame$binary)
        msg <- if (binary) jsonlite::base64_dec(frame$data) else frame$data
        for (cb in private$messageCallbacks_) {
          cb(binary, msg)
        }
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
      # Client-initiated close (`_shiny_close`) or GC.
      if (private$closed_) {
        return(invisible())
      }
      private$closed_ <- TRUE
      private$resolvePending()
      private$fireClose()
      invisible()
    },
    isClosed = function() {
      private$closed_
    }
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
      if (private$closeFired_) {
        return()
      }
      private$closeFired_ <- TRUE
      for (cb in private$closeCallbacks_) {
        cb()
      }
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
  if (is.null(id) || !is.character(id)) {
    return(NULL)
  }
  mcpConnections$get(id)
}

mcpConnRemove <- function(id) {
  mcpConnections$remove(id)
}

# Periodically close connections whose client stopped polling (e.g. the
# iframe was destroyed without `ui/resource-teardown`). Healthy clients
# long-poll at least every ~15s, so 60s of silence means the client is gone.
mcpScheduleSweep <- function() {
  if (isTRUE(.globals$mcpSweepScheduled)) {
    return(invisible())
  }
  .globals$mcpSweepScheduled <- TRUE
  later::later(function() {
    .globals$mcpSweepScheduled <- FALSE
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
    if (mcpConnections$size() > 0) {
      mcpScheduleSweep()
    }
  }, 30)
  invisible()
}

mcpToolResult <- function(id, structured = NULL, text = "ok") {
  result <- list(content = list(list(type = "text", text = text)))
  if (!is.null(structured)) {
    result$structuredContent <- structured
  }
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
      inputSchema = list(type = "object", properties = empty_named_list()),
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
        type = "object",
        properties = conn_arg,
        required = list("connectionId")
      ),
      `_meta` = app_only
    ),
    list(
      name = "_shiny_close",
      description = "Internal: close the Shiny session.",
      inputSchema = list(
        type = "object",
        properties = conn_arg,
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
    if (!isTRUE(ok) || conn$isClosed()) {
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
    timeoutSecs <- params$timeoutSecs %||% 15
    if (!is.numeric(timeoutSecs) || timeoutSecs < 0 || timeoutSecs > 30) {
      timeoutSecs <- 15
    }
    return(promises::then(conn$receiveFrames(timeoutSecs), function(res) {
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
