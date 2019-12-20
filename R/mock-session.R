# Promise helpers taken from:
#   https://github.com/rstudio/promises/blob/master/tests/testthat/common.R
# Block until all pending later tasks have executed
wait_for_it <- function() {
  while (!later::loop_empty()) {
    later::run_now(0.1)
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

#' Mock Shiny Session
#'
#' @description
#' An R6 class suitable for testing that simulates the `session` parameter
#' provided to Shiny server functions or modules.
#'
#' @include timer.R
#' @export
MockShinySession <- R6Class(
  'MockShinySession',
  portable = FALSE,
  class = FALSE,
  public = list(
    #' @field env The environment associated with the session.
    env = NULL,
    #' @field singletons Hardcoded as empty. Needed for rendering HTML (i.e. renderUI)
    singletons = character(0),
    #' @field clientData Mock client data that always returns a size for plots
    clientData = structure(list(), class="mockclientdata"),
    #' @description No-op
    #' @param logEntry Not used
    reactlog = function(logEntry){},
    #' @description No-op
    incrementBusyCount = function(){},
    #' @field output The shinyoutputs associated with the session
    output = NULL,
    #' @field input The reactive inputs associated with the session
    input = NULL,
    #' @field userData An environment initialized as empty.
    userData = NULL,
    #' @field progressStack A stack of progress objects
    progressStack = 'Stack',

    #' @description Create a new MockShinySession
    initialize = function() {
      private$.input <- ReactiveValues$new(dedupe = FALSE, label = "input")
      private$flushCBs <- Callbacks$new()
      private$flushedCBs <- Callbacks$new()
      private$endedCBs <- Callbacks$new()
      private$timer <- MockableTimerCallbacks$new()
      self$progressStack <- Stack$new()

      self$userData <- new.env(parent=emptyenv())

      # create output
      out <- .createOutputWriter(self)
      class(out) <- "shinyoutput"
      self$output <- out

      # Create a read-only copy of the inputs reactive.
      self$input <- .createReactiveValues(private$.input, readonly = TRUE)
    },
    #' @description Define a callback to be invoked before a reactive flush
    #' @param fun The function to invoke
    #' @param once If `TRUE`, will only run once. Otherwise, will run every time reactives are flushed.
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
    #' @description Define a callback to be invoked after a reactive flush
    #' @param fun The function to invoke
    #' @param once If `TRUE`, will only run once. Otherwise, will run every time reactives are flushed.
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
    #' @description Define a callback to be invoked when the session ends
    #' @param sessionEndedCallback The callback to invoke when the session has ended.
    onEnded = function(sessionEndedCallback) {
      private$endedCBs$register(sessionEndedCallback)
    },

    #' @description Returns `FALSE` if the session has not yet been closed
    isEnded = function(){ private$closed },
    #' @description Returns `FALSE` if the session has not yet been closed
    isClosed = function(){ private$closed },
    #' @description Closes the session
    close = function(){ private$closed <- TRUE },

    #FIXME: this is wrong. Will need to be more complex.
    #' @description Unsophisticated mock implementation that merely invokes
    #'   the given callback immediately.
    #' @param callback The callback ato be invoked.
    cycleStartAction = function(callback){ callback() },

    #' @description Base64-encode the given file. Needed for image rendering.
    #' @param name Not used
    #' @param file The file to be encoded
    #' @param contentType The content type of the base64-encoded string
    fileUrl = function(name, file, contentType='application/octet-stream') {
      bytes <- file.info(file)$size
      if (is.na(bytes))
        return(NULL)

      fileData <- readBin(file, 'raw', n=bytes)
      b64 <- rawToBase64(fileData)
      return(paste('data:', contentType, ';base64,', b64, sep=''))
    },

    #' @description Sets reactive values associated with the `session$inputs` object
    #'   and flushes the reactives.
    #' @param ... The inputs to set.
    #' @examples
    #' s <- MockShinySession$new()
    #' s$setInputs(x=1, y=2)
    setInputs = function(...) {
      vals <- list(...)
      mapply(names(vals), vals, FUN = function(name, value) {
        private$.input$set(name, value)
      })
      private$flush()
    },

    #' @description An internal method which shouldn't be used by others.
    #' @param millis The number of milliseconds on which to schedule a callback
    #' @param callback The function to schedule
    .scheduleTask = function(millis, callback) {
      id <- private$timer$schedule(millis, callback)

      # Return a deregistration callback
      function() {
        invisible(private$timer$unschedule(id))
      }
    },

    #' @description Simulate the passing of time by the given number of milliseconds.
    #' @param millis The number of milliseconds to advance time.
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

    #' @description An internal method which shouldn't be used by others.
    .now = function() {
      # Returns elapsed time in milliseconds
      private$timer$getElapsed()
    },

    #' @description An internal method which shouldn't be used by others.
    #' @param name The name of the output
    #' @param func The render definition
    #' @param label Not used
    defineOutput = function(name, func, label) {
      force(name)

      if (!is.null(private$outs[[name]]$obs)) {
        private$outs[[name]]$obs$destroy()
      }

      if (is.null(func)) func <- missingOutput

      if (!is.function(func))
        stop(paste("Unexpected", class(func), "output for", name))

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

    #' @description An internal method which shouldn't be used by others.
    #' @param name The name of the output
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

    #' @description No-op
    #' @param name Not used
    #' @param data Not used
    #' @param filterFunc Not used
    registerDataObj = function(name, data, filterFunc) {},
    #' @description No-op
    #' @param value Not used
    allowReconnect = function(value) {},
    #' @description No-op
    reload = function() {},
    #' @description No-op
    #' @param brushId Not used
    resetBrush = function(brushId) {
      warning("session$brush isn't meaningfully mocked on the MockShinySession")
    },
    #' @description No-op
    #' @param type Not used
    #' @param message Not used
    sendCustomMessage = function(type, message) {},
    #' @description No-op
    #' @param type Not used
    #' @param message Not used
    sendBinaryMessage = function(type, message) {},
    #' @description No-op
    #' @param inputId Not used
    #' @param message Not used
    sendInputMessage = function(inputId, message) {},
    #' @description No-op
    #' @param names Not used
    setBookmarkExclude = function(names) {
      warning("Bookmarking isn't meaningfully mocked in MockShinySession")
    },
    #' @description No-op
    getBookmarkExclude = function() {
      warning("Bookmarking isn't meaningfully mocked in MockShinySession")
    },
    #' @description No-op
    #' @param fun Not used
    onBookmark = function(fun) {},
    #' @description No-op
    #' @param fun Not used
    onBookmarked = function(fun) {},
    #' @description No-op
    doBookmark = function() {
      warning("Bookmarking isn't meaningfully mocked in MockShinySession")
    },
    #' @description No-op
    #' @param fun Not used
    onRestore = function(fun) {},
    #' @description No-op
    #' @param fun Not used
    onRestored = function(fun) {},
    #' @description No-op
    exportTestValues = function() {},
    #' @description No-op
    #' @param input Not used
    #' @param output Not used
    #' @param export Not used
    #' @param format Not used
    getTestSnapshotUrl = function(input=TRUE, output=TRUE, export=TRUE, format="json") {},
    #' @description Returns the given id prefixed by `mock-session-`.
    #' @param id The id to modify.
    ns = function(id) {
      paste0("mock-session-", id) # TODO: does this need to be more complex/intelligent?
    },
    #' @description Trigger a reactive flush right now.
    flushReact = function(){
      private$flush()
    },
    makeScope = function(namespace) {
      ns <- NS(namespace)
      createSessionProxy(
        self,
        input = .createReactiveValues(private$.input, readonly = TRUE, ns = ns),
        output = structure(.createOutputWriter(self, ns = ns), class = "shinyoutput"),
        makeScope = function(namespace) self$makeScope(ns(namespace))
      )
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
      shiny:::flushReact() # namespace to avoid calling our own method
      isolate(private$flushedCBs$invoke(..stacktraceon = TRUE))
      later::run_now()
    }
  ),
  active = list(
    # If assigning to `returned`, proactively flush
    #' @field returned The value returned from the module
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
    #' @field request An empty environment where the request should be. The request isn't meaningfully mocked currently.
    request = function(value) {
      if (!missing(value)){
        stop("session$request can't be assigned to")
      }
      warning("session$request doesn't currently simulate a realistic request on MockShinySession")
      new.env(parent=emptyenv())
    }
  )
)

