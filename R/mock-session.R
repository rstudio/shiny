# Promise helpers taken from:
#   https://github.com/rstudio/promises/blob/main/tests/testthat/common.R
# Block until all pending later tasks have executed
wait_for_it <- function() {
  while (!later::loop_empty()) {
    later::run_now(0.1)
  }
}

# Block until the promise is resolved/rejected. If resolved, return the value.
# If rejected, throw (yes throw, not return) the error.
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
    # Evade "no visible binding" note for reference to `private`
    (!!as.symbol("private"))$noopWarn(!!name, !!msg)
    invisible()
  })
  impl
}

#' Accepts a series of symbols as arguments and generates corresponding noop
#' implementations.
#' @noRd
makeWarnNoops <- function(...) {
  methods <- as.character(list(...))
  names(methods) <- methods
  lapply(methods, makeNoop)
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
    # TODO Consider implementing this. Would require a new method like
    # session$getDataObj() to access in a test expression.
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
    "setCurrentTheme",
    "getCurrentTheme",
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
    createBookmarkObservers = "for internal use only",
    dispatch = "for internal use only",
    handleRequest = "for internal use only",
    requestFlush = "for internal use only",
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

#' Mock Shiny Session
#'
#' @description An R6 class suitable for testing purposes. Simulates, to the
#'   extent possible, the behavior of the `ShinySession` class. The `session`
#'   parameter provided to Shiny server functions and modules is an instance of
#'   a `ShinySession` in normal operation.
#'
#'   Most kinds of module and server testing do not require this class be
#'   instantiated manually. See instead [testServer()].
#'
#'   In order to support advanced usage, instances of `MockShinySession` are
#'   **unlocked** so that public methods and fields of instances may be
#'   modified. For example, in order to test authentication workflows, the
#'   `user` or `groups` fields may be overridden. Modified instances of
#'   `MockShinySession` may then be passed explicitly as the `session` argument
#'   of [testServer()].
#'
#' @include timer.R
#' @export
MockShinySession <- R6Class(
  'MockShinySession',
  portable = FALSE,
  lock_objects = FALSE,
  public = list(
    #' @field env The environment associated with the session.
    env = NULL,
    #' @field returned The value returned by the module under test.
    returned = NULL,
    #' @field singletons Hardcoded as empty. Needed for rendering HTML (i.e. renderUI).
    singletons = character(0),
    #' @field clientData Mock client data that always returns a size for plots.
    clientData = structure(list(), class="mockclientdata"),
    #' @field output The shinyoutputs associated with the session.
    output = NULL,
    #' @field input The reactive inputs associated with the session.
    input = NULL,
    #' @field userData An environment initialized as empty.
    userData = NULL,
    #' @field progressStack A stack of progress objects.
    progressStack = 'Stack',
    #' @field token On a real `ShinySession`, used to identify this instance in URLs.
    token = 'character',
    #' @field cache The session cache object.
    cache = NULL,
    #' @field appcache The app cache object.
    appcache = NULL,
    #' @field restoreContext Part of bookmarking support in a real
    #'   `ShinySession` but always `NULL` for a `MockShinySession`.
    restoreContext = NULL,
    #' @field groups Character vector of groups associated with an authenticated
    #'   user. Always `NULL` for a `MockShinySesion`.
    groups = NULL,
    #' @field user The username of an authenticated user. Always `NULL` for a
    #'   `MockShinySession`.
    user = NULL,
    #' @field options A list containing session-level shinyOptions.
    options = NULL,

    #' @description Create a new MockShinySession.
    initialize = function() {
      private$.input <- ReactiveValues$new(dedupe = FALSE, label = "input")
      private$flushCBs <- Callbacks$new()
      private$flushedCBs <- Callbacks$new()
      private$endedCBs <- Callbacks$new()

      private$file_generators <- fastmap()

      private$timer <- MockableTimerCallbacks$new()
      self$progressStack <- fastmap::faststack()

      self$userData <- new.env(parent=emptyenv())

      # create output
      out <- .createOutputWriter(self)
      class(out) <- "shinyoutput"
      self$output <- out

      # Create a read-only copy of the inputs reactive.
      self$input <- .createReactiveValues(private$.input, readonly = TRUE)

      self$token <- createUniqueId(16)

      # Copy app-level options
      self$options <- getCurrentAppState()$options

      self$cache <- cachem::cache_mem()
      self$appcache <- cachem::cache_mem()

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
    close = function(){
      for (output in private$output) {
        output$suspend()
      }
      withReactiveDomain(self, {
        private$endedCBs$invoke(onError = printError, ..stacktraceon = TRUE)
      })
      private$was_closed <- TRUE
    },

    #FIXME: this is wrong. Will need to be more complex.
    #' @description Unsophisticated mock implementation that merely invokes
    #   the given callback immediately.
    #' @param callback The callback to be invoked.
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
    #'  object and flushes the reactives.
    #' @param ... The inputs to set. These arguments are processed with
    #'  [rlang::list2()] and so are _[dynamic][rlang::dyn-dots]_. Input names
    #'  may not be duplicated.
    #' @examples
    #' \dontrun{
    #' session$setInputs(x=1, y=2)
    #' }
    setInputs = function(...) {
      vals <- rlang::dots_list(..., .homonyms = "error")
      mapply(names(vals), vals, FUN = function(name, value) {
        private$.input$set(name, value)
      })
      private$flush()
    },

    #' @description An internal method which shouldn't be used by others.
    #'   Schedules `callback` for execution after some number of `millis`
    #'   milliseconds.
    #' @param millis The number of milliseconds on which to schedule a callback
    #' @param callback The function to schedule.
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
    #' @return Elapsed time in milliseconds.
    .now = function() {
      private$timer$getElapsed()
    },

    #' @description An internal method which shouldn't be used by others.
    #'   Defines an output in a way that sets private$currentOutputName
    #'   appropriately.
    #' @param name The name of the output.
    #' @param func The render definition.
    #' @param label Not used.
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
          v <- private$withCurrentOutput(name, func(self, name))
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

    #' @description An internal method which shouldn't be used by others. Forces
    #'   evaluation of any reactive dependencies of the output function.
    #' @param name The name of the output.
    #' @return The return value of the function responsible for rendering the
    #'   output.
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
      } else if (private$file_generators$has(self$ns(name))) {
        download <- private$file_generators$get(self$ns(name))
        private$renderFile(self$ns(name), download)
      } else {
        v$val
      }
    },

    #' @description Returns the given id prefixed by this namespace's id.
    #' @param id The id to prefix with a namespace id.
    #' @return The id with a namespace prefix.
    ns = function(id) {
      NS(private$nsPrefix, id)
    },
    #' @description Trigger a reactive flush right now.
    flushReact = function(){
      private$flush()
    },
    #' @description Create and return a namespace-specific session proxy.
    #' @param namespace Character vector indicating a namespace.
    #' @return A new session proxy.
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
    #'  only if it has not previously been set. This ensures that only the
    #'  environment of the outermost module under test is the one retained. In
    #'  other words, the first assignment wins.
    #' @param env The environment to retain.
    #' @return The provided `env`.
    setEnv = function(env) {
      if (is.null(self$env)) {
        stopifnot(all(c("input", "output", "session") %in% ls(env)))
        self$env <- env
      }
    },
    #' @description Set the value returned by the module call and proactively
    #'  flush. Note that this method may be called multiple times if modules
    #'  are nested. The last assignment, corresponding to an invocation of
    #'  setReturned() in the outermost module, wins.
    #' @param value The value returned from the module
    #' @return The provided `value`.
    setReturned = function(value) {
      self$returned <- value
      value
    },
    #' @description Get the value returned by the module call.
    #' @return The value returned by the module call
    getReturned = function() self$returned,
    #' @description Generate a distinct character identifier for use as a proxy
    #'   namespace.
    #' @return A character identifier unique to the current session.
    genId = function() {
      private$idCounter <- private$idCounter + 1
      paste0("proxy", private$idCounter)
    },
    #' @description Provides a way to access the root `MockShinySession` from
    #'   any descendant proxy.
    #' @return The root `MockShinySession`.
    rootScope = function() {
      self
    },
    #' @description Add an unhandled error callback.
    #' @param callback The callback to add, which should accept an error object
    #'   as its first argument.
    #' @return A deregistration function.
    onUnhandledError = function(callback) {
      private$unhandledErrorCallbacks$register(callback)
    },
    #' @description Called by observers when a reactive expression errors.
    #' @param e An error object.
    unhandledError = function(e) {
      private$unhandledErrorCallbacks$invoke(e, onError = printError)
      .globals$onUnhandledErrorCallbacks$invoke(e, onError = printError)
      self$close()
    },
    #' @description Freeze a value until the flush cycle completes.
    #' @param x A `ReactiveValues` object.
    #' @param name The name of a reactive value within `x`.
    freezeValue = function(x, name) {
      if (!is.reactivevalues(x))
        stop("x must be a reactivevalues object")

      impl <- .subset2(x, 'impl')
      key <- .subset2(x, 'ns')(name)
      impl$freeze(key)
      self$onFlushed(function() impl$thaw(key))
    },
    #' @description Registers the given callback to be invoked when the session
    #'   is closed (i.e. the connection to the client has been severed). The
    #'   return value is a function which unregisters the callback. If multiple
    #'   callbacks are registered, the order in which they are invoked is not
    #'   guaranteed.
    #' @param sessionEndedCallback Function to call when the session ends.
    onSessionEnded = function(sessionEndedCallback) {
      self$onEnded(sessionEndedCallback)
    },
    #' @description Associated a downloadable file with the session.
    #' @param name The un-namespaced output name to associate with the
    #'   downloadable file.
    #' @param filename A string or function designating the name of the file.
    #' @param contentType A string of the content type of the file. Not used by
    #'   `MockShinySession`.
    #' @param content A function that takes a single argument file that is a
    #'   file path (string) of a nonexistent temp file, and writes the content
    #'   to that file path. (Reactive values and functions may be used from this
    #'   function.)
    registerDownload = function(name, filename, contentType, content) {
      private$file_generators$set(self$ns(name), list(
        filename = if (is.function(filename)) filename else function() filename,
        content = content
      ))
    },
    #' @description Get information about the output that is currently being
    #'   executed.
    #' @return A list with with the `name` of the output. If no output is
    #'   currently being executed, this will return `NULL`.
    getCurrentOutputInfo = function() {
      name <- private$currentOutputName
      if (is.null(name)) NULL else list(name = name)
    }
  ),
  private = list(
    # @field .input Internal ReactiveValues object for normal input sent from client.
    .input = NULL,
    # @field flushCBs `Callbacks` called before flush.
    flushCBs = NULL,
    # @field flushedCBs `Callbacks` called after flush.
    flushedCBs = NULL,
    # @field endedCBs `Callbacks` called when session ends.
    endedCBs = NULL,
    #' @field unhandledErrorCallbacks `Callbacks` called when an unhandled error
    #'   occurs.
    unhandledErrorCallbacks = Callbacks$new(),
    # @field timer `MockableTimerCallbacks` called at particular times.
    timer = NULL,
    # @field was_closed Set to `TRUE` once the session is closed.
    was_closed = FALSE,
    # @field outs List of namespaced output names.
    outs = list(),
    # @field nsPrefix Prefix with which to namespace inputs and outputs.
    nsPrefix = "mock-session",
    # @field idCounter Incremented every time `$genId()` is called.
    idCounter = 0,
    # @field file_generators Map of namespaced output names to lists with
    #   `filename` and `output` elements, each a function. Updated by
    #   `$registerDownload()` and read by `$getOutput()`. Files are generated
    #   on demand when the output is accessed.
    file_generators = NULL,
    # @field currentOutputName Namespaced name of the currently executing
    #'   output, or `NULL` if no output is currently executing.
    currentOutputName = NULL,

    # @description Writes a downloadable file to disk. If the `content` function
    #   associated with a download handler does not write a file, an error is
    #   signaled. Created files are deleted upon session close.
    # @param name The eamespaced output name associated with the downloadable
    #   file.
    # @param download List with two names, `filename` and `content`. Both should
    #   be functions. `filename` should take no arguments and return a string.
    #   `content` should accept a path argument and create a file at that path.
    # @return A path to a temp file.
    renderFile = function(name, download) {
      # We make our own tempdir here because it's not safe to delete the result
      # of tempdir().
      tmpd <- tempfile()
      dir.create(tmpd, recursive = TRUE)
      self$onSessionEnded(function() unlink(tmpd, recursive = TRUE))
      file <- file.path(tmpd, download$filename())
      download$content(file)
      if (!file.exists(file))
        error("downloadHandler for ", name, " did not write a file.")
      file
    },

    # @description Calls `shiny:::flushReact()` and executes all callbacks
    #   related to reactivity.
    flush = function(){
      isolate(private$flushCBs$invoke(..stacktraceon = TRUE))
      shiny:::flushReact() # namespace to avoid calling our own method
      isolate(private$flushedCBs$invoke(..stacktraceon = TRUE))
      later::run_now()
    },

    # @description Produces a warning if the option `shiny.mocksession.warn` is
    #   unset and not `FALSE`.
    # @param name The name of the mocked method.
    # @param msg A message describing why the method is not implemented.
    noopWarn = function(name, msg) {
      if (getOption("shiny.mocksession.warn", FALSE) == FALSE)
        return(invisible())
      out <- paste0(name, " is not fully implemented by MockShinySession: ", msg)
      out <- paste0(out, "\n", "To disable messages like this, run `options(shiny.mocksession.warn=FALSE)`")
      warning(out, call. = FALSE)
    },

    # @description Binds a domain to `expr` and uses `createVarPromiseDomain()`
    #   to ensure `private$currentOutputName` is set to `name` around any of
    #   the promise's callbacks. Domains are something like dynamic scopes but
    #   for promise chains instead of the call stack.
    # @return A promise.
    withCurrentOutput = function(name, expr) {
      if (!is.null(private$currentOutputName)) {
        stop("Nested calls to withCurrentOutput() are not allowed.")
      }

      promises::with_promise_domain(
        createVarPromiseDomain(private, "currentOutputName", name),
        expr
      )
    }
  ),
  active = list(
    #' @field files For internal use only.
    files = function() stop("$files is for internal use only."),
    #' @field downloads For internal use only.
    downloads = function() stop("$downloads is for internal use only."),
    #' @field closed Deprecated in `ShinySession` and signals an error.
    closed = function() stop("$closed is deprecated"),
    #' @field session Deprecated in ShinySession and signals an error.
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
