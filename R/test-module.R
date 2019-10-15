# TODO:
#  - implement testServer

# Promise helpers taken from:
#   https://github.com/rstudio/promises/blob/master/tests/testthat/common.R
# Block until all pending later tasks have executed
# FIXME: will this work with multiple promises pending in parallel?
wait_for_it <- function() {
  while (!later::loop_empty()) {
    later::run_now()
    Sys.sleep(0.1)
  }
}

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

#' Test a shiny module
#' @param module The module under test
#' @param expr Test code containing expectations. The test expression will run
#'   in the module's environment, meaning that the module's parameters (e.g.
#'   `input`, `output`, and `session`) will be available along with any other
#'   values created inside of the module.
#' @param args A list of arguments to pass into the module beyond `input`,
#'   `output`, and `session`.
#' @param initialState A list describing the initial values for `input`. If no
#'   initial state is given, `input` will initialize as an empty list.
#' @param ... Additional named arguments to be passed on to the module function.
#' @export
testModule <- function(module, expr, args, ...) {
  # Capture the environment from the module
  # Inserts `session$env <- environment()` at the top of the function
  fn_body <- body(module)
  fn_body[seq(3, length(fn_body)+1)] <- fn_body[seq(2, length(fn_body))]
  fn_body[[2]] <- quote(session$env <- environment())
  body(module) <- fn_body

  # Substitute expr for later evaluation
  expr <- substitute(expr)
  .input <- ReactiveValues$new(dedupe = FALSE, label = "input")

  # Create the mock session
  session <- new.env(parent=emptyenv())

  # The onFlush* methods return a deregistration function
  flushCBs <- Callbacks$new()
  session$onFlush <- function(fun, once){
    if (!isTRUE(once)) {
      return(flushCBs$register(fun))
    } else {
      dereg <- flushCBs$register(function() {
        dereg()
        fun()
      })
      return(dereg)
    }
  }
  flushedCBs <- Callbacks$new()
  session$onFlushed <- function(fun, once){
    if (!isTRUE(once)) {
      return(flushedCBs$register(fun))
    } else {
      dereg <- flushedCBs$register(function() {
        dereg()
        fun()
      })
      return(dereg)
    }
  }

  isClosed <- FALSE
  session$isEnded <- function(){ isClosed }
  session$isClosed <- function(){ isClosed }
  session$close <- function(){ isClosed <<- TRUE }
  session$cycleStartAction <- function(callback){ callback() } #FIXME: this is wrong. Will need to be more complex.
  endedCBs <- Callbacks$new()
  session$onEnded <- function(sessionEndedCallback){
    endedCBs$register(sessionEndedCallback)
  }
  outputs <- list()
  session$defineOutput <- function(name, value, label){
    obs <- observe({
      # We could just stash the promise, but we get an "unhandled promise error". This bypasses
      prom <- NULL
      tryCatch({
        v <- value(session, name) #TODO: I'm not clear what `name` is supposed to be
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

      outputs[[name]]$promise <<- hybrid_chain(
        prom,
        function(v){
          list(val = v, err = NULL)
        }, catch=function(e){
          list(val = NULL, err = e)
        })
    })
    outputs[[name]] <<- list(obs = obs, func = value, promise = NULL)
  }
  session$singletons <- character(0) # Needed for rendering HTML (i.e. renderUI)

  # Define a mock client data that always returns a size for plots
  session$clientData <- structure(list(), class="mockclientdata")

  # Needed for image rendering. Base64-encode the given file.
  session$fileUrl <- function(name, file, contentType='application/octet-stream') {
    bytes <- file.info(file)$size
    if (is.na(bytes))
      return(NULL)

    fileData <- readBin(file, 'raw', n=bytes)
    b64 <- rawToBase64(fileData)
    return(paste('data:', contentType, ';base64,', b64, sep=''))
  }

  session$getOutput <- function(name){
    # Unlike the real outputs, we're going to return the last value rather than the unevaluated function
    if (is.null(outputs[[name]]$promise)) {
      stop("The test referenced an output that hasn't been defined yet: output$", name)
    }
    # Make promise return
    v <- extract(outputs[[name]]$promise)
    if (!is.null(v$err)){
      stop(v$err)
    } else {
      v$val
    }
  }
  # TODO: make private?
  session$flush <- function(){
    isolate(flushCBs$invoke(..stacktraceon = TRUE))
    flushReact()
    isolate(flushedCBs$invoke(..stacktraceon = TRUE))
    later::run_now()
  }
  session$setInputs <- function(...){
    vals <- list(...)
    # TODO: is there really not a way to access `names` from inside an lapply?
    lapply(names(vals), function(k){
      v <- vals[[k]]
      .input$set(k, v)
    })

    session$flush()
  }

  # TODO: private
  timer <- MockableTimerCallbacks$new()

  session$scheduleTask <- function(millis, callback){
    id <- timer$schedule(millis, callback)

    # Return a deregistration callback
    function() {
      invisible(timer$unschedule(id))
    }
  }

  session$elapse <- function(millis){
    timer$elapse(millis)

    # timerCallbacks must run before flushReact.
    timer$executeElapsed()

    session$flush()
  }

  session$reactlog <- function(logEntry){} # TODO: Needed for mock?
  session$incrementBusyCount <- function(){} # TODO: Needed for mock?

  curTime <- 0

  out <- .createOutputWriter(session)
  class(out) <- "shinyoutput"

  # Create a read-only copy of the inputs reactive.
  session$input <- .createReactiveValues(.input, readonly = TRUE)
  session$output <- out

  # Parse the additional arguments
  args <- list(...)
  args[["input"]] <- session$input
  args[["output"]] <- session$output
  args[["session"]] <- session

  # Store the result of the last evaluated expression so that we can stash the returned value
  r <- NULL

  # Initialize the module
  isolate(
    withReactiveDomain(
      session,
      withr::with_options(list(`shiny.allowoutputreads`=TRUE), {
        session$returned <- do.call(module, args)

        # Initial flush
        session$flush()
      })
    )
  )

  # Run the test expression in a reactive context and in the module's environment.
  # We don't need to flush before entering the loop because the first expr that we execute is `{`.
  # So we'll already flush before we get to the good stuff.
  isolate({
    withReactiveDomain(
      session,
      withr::with_options(list(`shiny.allowoutputreads`=TRUE), {
        eval(expr, session$env)
      })
    )
  })

  if (!isClosed){
    session$close()
  }
}

#' Test an app's server-side logic
#' @param expr Test code containing expectations
#' @param dir The directory root of the Shiny application. If `NULL`, this function
#'   will work up the directory hierarchy --- starting with the current directory ---
#'   looking for a directory that contains an `app.R` or `server.R` file.
#' @export
testServer <- function(expr, dir=NULL) {
  stop("NYI")
}
