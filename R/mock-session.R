# TODO: is there a way to get this behavior without exporting these? R6?
#' @noRd
#' @export
`$.mockclientdata` <- function(x, name) {
  if (name == "pixelratio"){
    return(1)
  }

  clientRE <- "^output_(.+)_([^_]+)$"
  if(grepl(clientRE, name)){
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

    initialize = function() {
      private$.input <- ReactiveValues$new(dedupe = FALSE, label = "input")
      private$flushCBs <- Callbacks$new()
      private$flushedCBs <- Callbacks$new()
      private$endedCBs <- Callbacks$new()
      private$timer <- MockableTimerCallbacks$new()

      # create output
      out <- .createOutputWriter(self)
      class(out) <- "shinyoutput"
      self$output <- out

      # Create a read-only copy of the inputs reactive.
      self$input <- .createReactiveValues(private$.input, readonly = TRUE)
    },
    onFlush = function(fun, once) {
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
    onFlushed = function(fun, once) {
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
      # TODO: is there really not a way to access `names` from inside an lapply?
      lapply(names(vals), function(k){
        v <- vals[[k]]
        private$.input$set(k, v)
      })

      private$flush()
    },


    scheduleTask = function(millis, callback) {
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

      # TODO: needed? We're guaranteed to not have anything to run given the above loop, right?
      private$timer$executeElapsed()
      private$flush()
    },

    now = function() {
      # Contract is to return Sys.time, which is seconds, not millis.
      private$timer$getElapsed()/1000
    },

    defineOutput = function(name, value, label) {
      obs <- observe({
        # We could just stash the promise, but we get an "unhandled promise error". This bypasses
        prom <- NULL
        tryCatch({
          v <- value(self, name) #TODO: I'm not clear what `name` is supposed to be
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
      private$outs[[name]] <- list(obs = obs, func = value, promise = NULL)
    },

    getOutput = function(name) {
      # Unlike the real outputs, we're going to return the last value rather than the unevaluated function
      if (is.null(private$outs[[name]]$promise)) {
        stop("The test referenced an output that hasn't been defined yet: output$", name)
      }
      # Make promise return
      v <- extract(private$outs[[name]]$promise)
      if (!is.null(v$err)){
        stop(v$err)
      } else {
        v$val
      }
    },

    registerDataObj = function(name, data, filterFunc) {}
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
    }
  )
)
