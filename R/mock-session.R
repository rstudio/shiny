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

#' @noRd
mapNames <- function(func, vals) {
  names(vals) <- vapply(names(vals), func, character(1))
  vals
}

#' Returns a noop implementation of the public method `name` of ShinySession.
#' @include shiny.R
#' @noRd
makeNoop <- function(name, msg = paste0(name, " is a noop.")) {
  if (!(name %in% names(ShinySession$public_methods)))
    stop(name, " is not public method of ShinySession.")
  impl <- ShinySession$public_methods[[name]]
  body(impl) <- rlang::expr({
    # Force arguments
    !!lapply(formalArgs(impl), rlang::sym)
    private$noopWarn(!!name, !!msg)
    NULL
  })
  impl
}

#' Accepts a series of symbols as arguments and generates corresponding noop
#' implementations.
#' @noRd
makeWarnNoops <- function(...) {
  methods <- as.character(rlang::ensyms(...))
  sapply(methods, makeNoop, USE.NAMES = TRUE, simplify = FALSE)
}

#' Returns an implementation of a ShinySession public method that signals an
#' error.
#' @include shiny.R
#' @noRd
makeError <- function(name, msg = paste0(name, " is for internal use only.")) {
  if (!(name %in% names(ShinySession$public_methods)))
    stop(name, " is not public method of ShinySession.")
  impl <- ShinySession$public_methods[[name]]
  body(impl) <- rlang::expr({
    base::stop(!!msg)
  })
  impl
}

#' Accepts a series of named arguments. Each name corresponds to a ShinySession
#' public method that should signal an error, and each argument corresponds to
#' an error message.
#' @noRd
makeErrors <- function(...) {
  errors <- rlang::list2(...)
  mapply(makeError, names(errors), errors, USE.NAMES = TRUE, SIMPLIFY = FALSE)
}

#' @noRd
makeExtraMethods <- function() {
  c(makeWarnNoops(
    "allowReconnect",
    "decrementBusyCount",
    "doBookmark",
    "exportTestValues",
    "flushOutput",
    "getBookmarkExclude",
    "getCurrentOutputInfo",
    "getTestSnapshotUrl",
    "incrementBusyCount",
    "manageHiddenOutputs",
    "manageInputs",
    "onBookmark",
    "onBookmarked",
    "onInputReceived",
    "onRestore",
    "onRestored",
    "outputOptions",
    "reactlog",
    "registerDataObj",
    "reload",
    "resetBrush",
    "sendBinaryMessage",
    "sendChangeTabVisibility",
    "sendCustomMessage",
    "sendInputMessage",
    "sendInsertTab",
    "sendInsertUI",
    "sendModal",
    "sendNotification",
    "sendProgress",
    "sendRemoveTab",
    "sendRemoveUI",
    "setBookmarkExclude",
    "setShowcase",
    "showProgress",
    "updateQueryString"
  ), makeErrors(
    `@uploadEnd` = "for internal use only",
    `@uploadInit` = "for internal use only",
    `@uploadieFinish` = "for internal use only",
    createBookmarkObservers = "for internal use only",
    dispatch = "for internal use only",
    handleRequest = "for internal use only",
    requestFlush = "for internal use only",
    saveFileUrl = "for internal use only",
    startTiming = "for internal use only",
    wsClosed = "for internal use only"
  ))
}

#' @description Adds generated instance methods to a MockShinySession instance.
#'   Note that `lock_objects = FALSE` must be set in the call to `R6Class()`
#'   that produced the generator object of the instance.
#' @param instance instance of an R6 object, generally a `MockShinySession`.
#' @param methods named list of method names to method implementation functions.
#'   In our typical usage, each function is derived from a public method of
#'   `ShinySession`. The environment of each implementation function is set to
#'   `instance$.__enclos_env` before the method is added.
#' @noRd
addGeneratedInstanceMethods <- function(instance, methods = makeExtraMethods()) {
  mapply(function(name, impl) {
    environment(impl) <- instance$.__enclos_env__
    instance[[name]] <- impl
  }, names(methods), methods)
}

#' @export
makeMockSession <- function() {
  MockShinySession$new()
}


#' Mock Shiny Session
#'
#' @description
#' An R6 class suitable for testing that simulates the `session` parameter
#' provided to Shiny server functions or modules.
#'
#' @include timer.R
#' @noRd
#' @export
MockShinySession <- R6Class(
  'MockShinySession',
  portable = FALSE,
  lock_objects = FALSE,
  public = list(
    #' @field env The environment associated with the session.
    env = NULL,
    #' @field returned The value returned by the module.
    returned = NULL,
    #' @field singletons Hardcoded as empty. Needed for rendering HTML (i.e. renderUI)
    singletons = character(0),
    #' @field clientData Mock client data that always returns a size for plots
    clientData = structure(list(), class="mockclientdata"),
    #' @field output The shinyoutputs associated with the session
    output = NULL,
    #' @field input The reactive inputs associated with the session
    input = NULL,
    #' @field userData An environment initialized as empty.
    userData = NULL,
    #' @field progressStack A stack of progress objects
    progressStack = 'Stack',
    token = 'character',
    cache = NULL,
    restoreContext = NULL,
    groups = NULL,
    user = NULL,

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

      self$token <- createUniqueId(16)
      self$cache <- MemoryCache$new()

      # Adds various generated noop and error-producing method implementations.
      # Note that noop methods can be configured to produce warnings by setting
      # the option shiny.mocksession.warn = TRUE; see $noopWarn() for details.
      addGeneratedInstanceMethods(self)
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
    isEnded = function(){ private$was_closed },
    #' @description Returns `FALSE` if the session has not yet been closed
    isClosed = function(){ private$was_closed },
    #' @description Closes the session
    close = function(){ private$was_closed <- TRUE },

    #FIXME: this is wrong. Will need to be more complex.
    #' @description Unsophisticated mock implementation that merely invokes
    #   the given callback immediately.
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

    #' @description Sets reactive values associated with the `session$inputs`
    #   object and flushes the reactives.
    #' @param ... The inputs to set. These arguments are processed with
    #   [rlang::list2()] and so are _[dynamic][rlang::dyn-dots]_. Input names
    #   may not be duplicated.
    #' @examples
    # \dontrun{
    # session$setInputs(x=1, y=2)
    # }
    setInputs = function(...) {
      vals <- rlang::dots_list(..., .homonyms = "error")
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

    #' @description Returns the given id prefixed by this namespace's id.
    #' @param id The id to modify.
    ns = function(id) {
      NS(private$nsPrefix, id)
    },
    #' @description Trigger a reactive flush right now.
    flushReact = function(){
      private$flush()
    },
    #' @description Create and return a namespace-specific session proxy.
    #' @param namespace Character vector indicating a namespace.
    makeScope = function(namespace) {
      ns <- NS(namespace)
      createSessionProxy(
        self,
        input = .createReactiveValues(private$.input, readonly = TRUE, ns = ns),
        output = structure(.createOutputWriter(self, ns = ns), class = "shinyoutput"),
        makeScope = function(namespace) self$makeScope(ns(namespace)),
        ns = function(namespace) ns(namespace),
        setInputs = function(...) {
          self$setInputs(!!!mapNames(ns, rlang::dots_list(..., .homonyms = "error")))
        }
      )
    },
    #' @description Set the environment associated with a testServer() call, but
    #   only if it has not previously been set. This ensures that only the
    #   environment of the outermost module under test is the one retained. In
    #   other words, the first assignment wins.
    #' @param env The environment to retain.
    setEnv = function(env) {
      if (is.null(self$env)) {
        stopifnot(all(c("input", "output", "session") %in% ls(env)))
        self$env <- env
      }
    },
    #' @description Set the value returned by the module call and proactively
    #   flush. Note that this method may be called multiple times if modules
    #   are nested. The last assignment, corresponding to an invocation of
    #   setReturned() in the outermost module, wins.
    #' @param value The value returned from the module
    setReturned = function(value) {
      self$returned <- value
      value
    },
    #' @description Get the value returned by the module call.
    getReturned = function() self$returned,
    #' @description Return a distinct character identifier for use as a proxy
    #   namespace.
    genId = function() {
      private$idCounter <- private$idCounter + 1
      paste0("proxy", private$idCounter)
    },
    rootScope = function() {
      self
    },
    unhandledError = function(e) {
      self$close()
    },
    freezeValue = function(x, name) {
      if (!is.reactivevalues(x))
        stop("x must be a reactivevalues object")

      impl <- .subset2(x, 'impl')
      key <- .subset2(x, 'ns')(name)
      impl$freeze(key)
      self$onFlushed(function() impl$thaw(key))
    },
    onSessionEnded = function(sessionEndedCallback) {
      self$onEnded(sessionEndedCallback)
    }
  ),
  private = list(
    .input = NULL,
    flushCBs = NULL,
    flushedCBs = NULL,
    endedCBs = NULL,
    timer = NULL,
    was_closed = FALSE,
    outs = list(),
    nsPrefix = "mock-session",
    idCounter = 0,

    flush = function(){
      isolate(private$flushCBs$invoke(..stacktraceon = TRUE))
      shiny:::flushReact() # namespace to avoid calling our own method
      isolate(private$flushedCBs$invoke(..stacktraceon = TRUE))
      later::run_now()
    },

    noopWarn = function(name, msg) {
      if (getOption("shiny.mocksession.warn", FALSE) == FALSE)
        return(invisible())
      out <- paste0(name, " is not fully implemented by MockShinySession: ", msg)
      out <- paste0(out, "\n", "To disable messages like this, run `options(shiny.mocksession.warn=FALSE)`")
      warning(out, call. = FALSE)
    }

  ),
  active = list(
    #' @field files Part of saveFileUrl; for internal use only.
    files = function() stop("$files is for internal use only."),
    #' @field closed Deprecated in ShinySession so signals an error here.
    closed = function() stop("$closed is deprecated"),
    #' @field session Deprecated in ShinySession so signals an error here.
    session = function() stop("$session is deprecated"),
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
