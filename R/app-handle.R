# Handle returned by runApp() when blocking = FALSE
ShinyAppHandle <- R6::R6Class("ShinyAppHandle",
  cloneable = FALSE,

  public = list(
    initialize = function(appUrl, cleanupFn) {
      private$appUrl <- appUrl
      private$cleanupFn <- cleanupFn

      reg.finalizer(self, function(e) {
        tryCatch(e$stop(), error = function(cnd) NULL)
      }, onexit = TRUE)
    },

    stop = function() {
      if (self$status() != "running") {
        return(invisible(self))
      }
      private$captureResult()
      private$cleanupFn()
      private$cleanupFn <- NULL
      invisible(self)
    },

    url = function() private$appUrl,

    status = function() {
      if (!private$stopped) {
        "running"
      } else if (!is.null(private$resultError)) {
        "error"
      } else {
        "success"
      }
    },

    result = function() {
      if (self$status() == "running") {
        stop("App is still running. Use status() to check if the app has stopped.")
      }
      if (!is.null(private$resultError)) {
        stop(private$resultError)
      }
      private$resultValue
    },

    print = function(...) {
      cat("Shiny app handle\n")
      cat("  URL:   ", private$appUrl, "\n", sep = "")
      cat("  Status:", self$status(), "\n")
      invisible(self)
    }
  ),

  private = list(
    appUrl = NULL,
    cleanupFn = NULL,
    # Whether this handle has captured the result. Distinct from .globals$stopped
    # which tracks whether a stop was requested (set by stopApp() or stop()).
    stopped = FALSE,
    resultValue = NULL,
    resultError = NULL,

    captureResult = function() {
      if (private$stopped) return()
      private$stopped <- TRUE
      if (isTRUE(.globals$reterror)) {
        private$resultError <- .globals$retval
      } else if (!is.null(.globals$retval)) {
        private$resultValue <- .globals$retval$value
      }
    }
  )
)
