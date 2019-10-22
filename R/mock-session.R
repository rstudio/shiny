# Promise helpers taken from:
#   https://github.com/rstudio/promises/blob/master/tests/testthat/common.R
# Block until all pending later tasks have executed
wait_for_it <- function() {
  while (!later::loop_empty()) {
    later::run_now()
    Sys.sleep(0.1)
  }
}

# Block until the promise is resolved/rejected. If resolved, return the value.
# If rejected, throw (yes throw, not return) the error.
#' @importFrom promises %...!%
#' @importFrom promises %...>%
extract <- function(promise) {
  promise_value <- NULL
  error <- NULL
  promise %...>%
    (function(value) promise_value <<- value) %...!%
    (function(reason) error <<- reason)

  wait_for_it()
  if (!is.null(error))
    stop(error)
  else
    promise_value
}

# TODO: is there a way to get this behavior without exporting these functions? R6?
# TODO: clientData is documented as a reactiveValues, which this is not. Is it possible that
#   users are currently assigning into clientData? That would not work as expected here.
#' @noRd
#' @export
`$.mockclientdata` <- function(x, name) {
  if (name == "allowDataUriScheme") { return(TRUE) }
  if (name == "pixelratio") { return(1) }
  if (name == "url_protocol") { return("http:") }
  if (name == "url_hostname") { return("mocksession") }
  if (name == "url_port") { return(1234) }
  if (name == "url_pathname") { return("/mockpath") }
  if (name == "url_hash") { return("#mockhash") }
  if (name == "url_hash_initial") { return("#mockhash") }
  if (name == "url_search") { return("?mocksearch=1") }

  clientRE <- "^output_(.+)_([^_]+)$"
  if(grepl(clientRE, name)) {
    # TODO: use proper regex group matching here instead of redundantly parsing
    el <- sub(clientRE, "\\1", name)
    att <- sub(clientRE, "\\2", name)

    if (att == "width") {
      return(600)
    } else if (att == "height") {
      return(400)
    } else if (att == "hidden") {
      return(FALSE)
    }
  }
  warning("Unexpected clientdata attribute accessed: ", name)
  return(NULL)
}

#' @noRd
#' @export
`[[.mockclientdata` <- `$.mockclientdata`

#' @noRd
#' @export
`[.mockclientdata` <- function(values, name) {
  stop("Single-bracket indexing of mockclientdata is not allowed.")
}

#' @include timer.R
MockShinySession <- R6Class(
  'MockShinySession',
  portable = FALSE,
  class = FALSE,
  public = list(
    env = NULL,
    # Needed for rendering HTML (i.e. renderUI)
    singletons = character(0),
    # Define a mock client data that always returns a size for plots
    clientData = structure(list(), class="mockclientdata"),
    reactlog = function(logEntry){},
    incrementBusyCount = function(){},
    output = NULL,
    input = NULL,
    userData = NULL,

    initialize = function() {
      private$.input <- ReactiveValues$new(dedupe = FALSE, label = "input")
      private$flushCBs <- Callbacks$new()
      private$flushedCBs <- Callbacks$new()
      private$endedCBs <- Callbacks$new()
      private$timer <- MockableTimerCallbacks$new()

      self$userData <- new.env(parent=emptyenv())

      # create output
      out <- .createOutputWriter(self)
      class(out) <- "shinyoutput"
      self$output <- out

      # Create a read-only copy of the inputs reactive.
      self$input <- .createReactiveValues(private$.input, readonly = TRUE)
    },
    onFlush = function(fun, once=TRUE) {
      if (!isTRUE(once)) {
        return(private$flushCBs$register(fun))
      } else {
        dereg <- private$flushCBs$register(function() {
          dereg()
          fun()
        })
        return(dereg)
      }
    },
    onFlushed = function(fun, once=TRUE) {
      if (!isTRUE(once)) {
        return(private$flushedCBs$register(fun))
      } else {
        dereg <- private$flushedCBs$register(function() {
          dereg()
          fun()
        })
        return(dereg)
      }
    },
    onEnded = function(sessionEndedCallback) {
      private$endedCBs$register(sessionEndedCallback)
    },

    isEnded = function(){ private$closed },
    isClosed = function(){ private$closed },
    close = function(){ private$closed <- TRUE },

    #FIXME: this is wrong. Will need to be more complex.
    cycleStartAction = function(callback){ callback() },

    # Needed for image rendering. Base64-encode the given file.
    fileUrl = function(name, file, contentType='application/octet-stream') {
      bytes <- file.info(file)$size
      if (is.na(bytes))
        return(NULL)

      fileData <- readBin(file, 'raw', n=bytes)
      b64 <- rawToBase64(fileData)
      return(paste('data:', contentType, ';base64,', b64, sep=''))
    },

    setInputs = function(...) {
      vals <- list(...)
      mapply(names(vals), vals, FUN = function(name, value) {
        private$.input$set(name, value)
      })
      private$flush()
    },

    .scheduleTask = function(millis, callback) {
      id <- private$timer$schedule(millis, callback)

      # Return a deregistration callback
      function() {
        invisible(private$timer$unschedule(id))
      }
    },
    elapse = function(millis) {
      msLeft <- millis

      while (msLeft > 0){
        t <- private$timer$timeToNextEvent()

        if (is.infinite(t) || t <= 0 || msLeft < t){
          # Either there's no good upcoming event or we can't make it to it in the allotted time.
          break
        }
        msLeft <- msLeft - t
        private$timer$elapse(t)

        # timerCallbacks must run before flushReact.
        private$timer$executeElapsed()
        private$flush()
      }

      private$timer$elapse(msLeft)

      # Run again in case our callbacks resulted in a scheduled
      # function that needs executing.
      private$timer$executeElapsed()
      private$flush()
    },

    .now = function() {
      # Contract is to return Sys.time, which is seconds, not millis.
      private$timer$getElapsed()/1000
    },

    defineOutput = function(name, func, label) {
      if (!is.null(private$outs[[name]]$obs)) {
        private$outs[[name]]$obs$destroy()
      }

      obs <- observe({
        # We could just stash the promise, but we get an "unhandled promise error". This bypasses
        prom <- NULL
        tryCatch({
          v <- func(self, name)
          if (!promises::is.promise(v)){
            # Make our sync value into a promise
            prom <- promises::promise(function(resolve, reject){ resolve(v) })
          } else {
            prom <- v
          }
        }, error=function(e){
          # Error running value()
          prom <<- promises::promise(function(resolve, reject){ reject(e) })
        })

        private$outs[[name]]$promise <- hybrid_chain(
          prom,
          function(v){
            list(val = v, err = NULL)
          }, catch=function(e){
            list(val = NULL, err = e)
          })
      })
      private$outs[[name]] <- list(obs = obs, func = func, promise = NULL)
    },

    getOutput = function(name) {
      # Unlike the real outputs, we're going to return the last value rather than the unevaluated function
      if (is.null(private$outs[[name]])) {
        stop("The test referenced an output that hasn't been defined yet: output$", name)
      }

      if (is.null(private$outs[[name]]$promise)) {
        # Means the output was defined but the observer hasn't had a chance to run
        # yet. Run flushReact() now to force the observer to run.
        flushReact()

        if (is.null(private$outs[[name]]$promise)) {
          stop("output$", name, " encountered an unexpected error resolving its promise")
        }
      }

      # Make promise return
      v <- extract(private$outs[[name]]$promise)
      if (!is.null(v$err)){
        stop(v$err)
      } else {
        v$val
      }
    },

    registerDataObj = function(name, data, filterFunc) {},
    allowReconnect = function(value) {},
    reload = function() {},
    resetBrush = function(brushId) {
      warning("session$brush isn't meaningfully mocked on the MockShinySession")
    },
    sendCustomMessage = function(type, message) {},
    sendBinaryMessage = function(type, message) {},
    sendInputMessage = function(inputId, message) {},
    setBookmarkExclude = function(names) {
      warning("Bookmarking isn't meaningfully mocked in MockShinySession")
    },
    getBookmarkExclude = function() {
      warning("Bookmarking isn't meaningfully mocked in MockShinySession")
    },
    onBookmark = function(fun) {
      warning("Bookmarking isn't meaningfully mocked in MockShinySession")
    },
    onBookmarked = function(fun) {
      warning("Bookmarking isn't meaningfully mocked in MockShinySession")
    },
    doBookmark = function() {
      warning("Bookmarking isn't meaningfully mocked in MockShinySession")
    },
    onRestore = function(fun) {},
    onRestored = function(fun) {},
    exportTestValues = function() {},
    getTestSnapshotUrl = function(input=TRUE, output=TRUE, export=TRUE, format="json") {},
    ns = function(id) {
      paste0("mock-session-", id) # TODO: does this need to be more complex/intelligent?
    }
  ),
  private = list(
    .input = NULL,
    flushCBs = NULL,
    flushedCBs = NULL,
    endedCBs = NULL,
    timer = NULL,
    closed = FALSE,
    outs = list(),
    returnedVal = NULL,

    flush = function(){
      isolate(private$flushCBs$invoke(..stacktraceon = TRUE))
      flushReact()
      isolate(private$flushedCBs$invoke(..stacktraceon = TRUE))
      later::run_now()
    }
  ),
  active = list(
    # If assigning to `returned`, proactively flush
    returned = function(value){
      if(missing(value)){
        return(private$returnedVal)
      }
      # When you assign to returned, that implies that you just ran
      # the module. So we should proactively flush. We have to do this
      # here since flush is private.
      private$returnedVal <- value
      private$flush()
    },
    request = function(value) {
      if (!missing(value)){
        stop("session$request can't be assigned to")
      }
      warning("session$request doesn't currently simulate a realistic request on MockShinySession")
      new.env(parent=emptyenv())
    }
  )
)
