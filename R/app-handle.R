# Handle returned by runApp() when blocking = FALSE
ShinyAppHandle <- R6::R6Class("ShinyAppHandle",
  cloneable = FALSE,

  public = list(
    initialize = function(server, appUrl, cleanupFn) {
      private$server <- server
      private$appUrl <- appUrl
      private$cleanupFn <- cleanupFn

      # Weak reference for cleanup if handle is discarded
      reg.finalizer(self, function(e) {
        if (e$isRunning()) {
          tryCatch(e$stop(), error = function(cnd) NULL, warning = function(cnd) NULL)
        }
      }, onexit = TRUE)
    },

    stop = function() {
      if (!self$isRunning()) {
        warning("App is not running")
        return(invisible(self))
      }
      self$.captureResult()
      .globals$stopped <- TRUE
      private$cleanupFn()
      private$server <- NULL
      private$cleanupFn <- NULL
      invisible(self)
    },

    isRunning = function() !private$stopped,
    getUrl = function() private$appUrl,
    getServer = function() private$server,

    # Access return value from stopApp() - captured at stop time
    result = function() private$result_,
    error = function() private$error_,

    # Internal: called by service loop when app self-terminates
    .captureResult = function() {
      if (private$stopped) return()
      private$stopped <- TRUE
      if (isTRUE(.globals$reterror)) {
        private$error_ <- .globals$retval
      } else if (!is.null(.globals$retval)) {
        private$result_ <- .globals$retval$value
      }
    },

    print = function(...) {
      cat("Shiny app handle\n")
      cat("  URL:   ", private$appUrl, "\n", sep = "")
      cat("  Status:", if (self$isRunning()) "running" else "stopped", "\n")
      invisible(self)
    }
  ),

  private = list(
    server = NULL,
    appUrl = NULL,
    cleanupFn = NULL,
    stopped = FALSE,
    result_ = NULL,
    error_ = NULL
  )
)
