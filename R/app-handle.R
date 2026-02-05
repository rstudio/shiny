# Handle returned by runApp() when blocking = FALSE
ShinyAppHandle <- R6::R6Class("ShinyAppHandle",
  cloneable = FALSE,

  public = list(
    initialize = function(appUrl, cleanupFn) {
      private$url_ <- appUrl
      private$cleanupFn <- cleanupFn

      reg.finalizer(self, function(e) {
        if (e$status() == "running") {
          tryCatch(e$stop(), error = function(cnd) NULL, warning = function(cnd) NULL)
        }
      }, onexit = TRUE)
    },

    stop = function() {
      if (self$status() != "running") {
        warning("App is not running")
        return(invisible(self))
      }
      self$.captureResult()
      .globals$stopped <- TRUE
      private$cleanupFn()
      private$cleanupFn <- NULL
      invisible(self)
    },

    url = function() private$url_,

    status = function() {
      if (!private$stopped) {
        "running"
      } else if (!is.null(private$error_)) {
        "error"
      } else {
        "success"
      }
    },

    result = function() {
      if (self$status() == "running") {
        stop("App is still running. Use status() to check if the app has stopped.")
      }
      if (!is.null(private$error_)) {
        stop(private$error_)
      }
      private$result_
    },

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
      cat("  URL:   ", private$url_, "\n", sep = "")
      cat("  Status:", self$status(), "\n")
      invisible(self)
    }
  ),

  private = list(
    url_ = NULL,
    cleanupFn = NULL,
    stopped = FALSE,
    result_ = NULL,
    error_ = NULL
  )
)
