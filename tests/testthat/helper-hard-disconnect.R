# FakeWebSocket: records sent JSON payloads and close calls for assertions.
# Implements the subset of the httpuv websocket interface that ShinySession
# uses via private$websocket.
FakeWebSocket <- R6::R6Class(
  "FakeWebSocket",
  public = list(
    sent = NULL,        # character vector of JSON payloads passed to send()
    closeCode = NULL,   # last numeric code passed to close()
    closeReason = NULL, # last character reason passed to close()
    closeCalled = FALSE,
    request = NULL,     # minimal stub so ShinySession init doesn't choke
    initialize = function() {
      self$sent <- character(0)
      self$request <- list(HTTP_GUID = NULL)
    },
    send = function(payload) {
      self$sent <- c(self$sent, as.character(payload))
      invisible()
    },
    close = function(code = NULL, reason = NULL) {
      self$closeCalled <- TRUE
      self$closeCode <- code
      self$closeReason <- reason
      invisible()
    },
    onMessage = function(fn) invisible(),
    onClose = function(fn) invisible()
  )
)

# Decode the most recently sent JSON payload into an R list.
fake_ws_last_message <- function(ws) {
  if (length(ws$sent) == 0L) return(NULL)
  jsonlite::fromJSON(ws$sent[length(ws$sent)], simplifyVector = FALSE)
}

# Return all sent payloads that match a top-level key (e.g., "custom").
fake_ws_messages_with <- function(ws, key) {
  parsed <- lapply(ws$sent, function(p) jsonlite::fromJSON(p, simplifyVector = FALSE))
  Filter(function(m) !is.null(m[[key]]), parsed)
}
