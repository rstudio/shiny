#' @include utils.R
NULL

#' Web Application Framework for R
#'
#' Shiny makes it incredibly easy to build interactive web applications with R.
#' Automatic "reactive" binding between inputs and outputs and extensive
#' prebuilt widgets make it possible to build beautiful, responsive, and
#' powerful applications with minimal effort.
#'
#' The Shiny tutorial at <https://shiny.rstudio.com/tutorial/> explains
#' the framework in depth, walks you through building a simple application, and
#' includes extensive annotated examples.
#'
#' @seealso [shiny-options] for documentation about global options.
#'
#' @name shiny-package
#' @aliases shiny
"_PACKAGE"

createUniqueId <- function(bytes, prefix = "", suffix = "") {
  withPrivateSeed({
    paste(
      prefix,
      paste(
        format(as.hexmode(sample(256, bytes, replace = TRUE)-1), width=2),
        collapse = ""),
      suffix,
      sep = ""
    )
  })
}

toJSON <- function(x, ...,  dataframe = "columns", null = "null", na = "null",
  auto_unbox = TRUE,
  # Shiny has had a legacy value of 16 significant digits
  # We can use `I(16)` mixed with the default behavior in jsonlite's `use_signif=`
  # https://github.com/jeroen/jsonlite/commit/728efa9
  digits = getOption("shiny.json.digits", I(16)), use_signif = is(digits, "AsIs"),
  force = TRUE, POSIXt = "ISO8601", UTC = TRUE,
  rownames = FALSE, keep_vec_names = TRUE, strict_atomic = TRUE) {

  if (strict_atomic) {
    x <- I(x)
  }

  # I(x) is so that length-1 atomic vectors get put in [].
  jsonlite::toJSON(x, dataframe = dataframe, null = null, na = na,
   auto_unbox = auto_unbox, digits = digits, use_signif = use_signif,
   force = force, POSIXt = POSIXt, UTC = UTC, rownames = rownames,
   keep_vec_names = keep_vec_names, json_verbatim = TRUE, ...)
}

# If the input to jsonlite::fromJSON is not valid JSON, it will try to fetch a
# URL or read a file from disk. We don't want to allow that.
safeFromJSON <- function(txt, ...) {
  if (!jsonlite::validate(txt)) {
    stop("Argument 'txt' is not a valid JSON string.")
  }
  jsonlite::fromJSON(txt, ...)
}

# Call the workerId func with no args to get the worker id, and with an arg to
# set it.
#
# A worker ID is an opaque string that is passed in by the caller. The ID is
# added as a URL parameter (?w=<worker_id>) to any URLs that need to refer back
# to the app. This can be used as a hint for load balancers to direct requests
# to this particular process. Since the worker refers to a process, it's
# inherently global, and should never need to change.
workerId <- local({
  .workerId <- NULL
  function(value) {
    if (missing(value)) {
      .workerId
    } else {
      if (!is.null(.workerId)) {
        if (!identical(value, .workerId)) {
          warning("Ignoring workerId value--",
            "it's already been set to a different value")
        }
      } else {
        .workerId <<- value
      }
    }
  }
})

#' Session object
#'
#' Shiny server functions can optionally include `session` as a parameter
#' (e.g. `function(input, output, session)`). The session object is an
#' environment that can be used to access information and functionality
#' relating to the session. The following list describes the items available
#' in the environment; they can be accessed using the `$` operator (for
#' example, `session$clientData$url_search`).
#'
#' @return
#' \item{allowReconnect(value)}{
#'   If `value` is `TRUE` and run in a hosting environment (Shiny
#'   Server or Connect) with reconnections enabled,  then when the session ends
#'   due to the network connection closing, the client will attempt to
#'   reconnect to the server. If a reconnection is successful, the browser will
#'   send all the current input values to the new session on the server, and
#'   the server will recalculate any outputs and send them back to the client.
#'   If `value` is `FALSE`, reconnections will be disabled (this is
#'   the default state). If `"force"`, then the client browser will always
#'   attempt to reconnect. The only reason to use `"force"` is for testing
#'   on a local connection (without Shiny Server or Connect).
#' }
#' \item{clientData}{
#'   A [reactiveValues()] object that contains information about the client.
#'   \itemize{
#'     \item{`pixelratio` reports the "device pixel ratio" from the web browser,
#'       or 1 if none is reported. The value is 2 for Apple Retina displays.
#'     }
#'     \item{`singletons` - for internal use}
#'     \item{`url_protocol`, `url_hostname`, `url_port`,
#'       `url_pathname`, `url_search`, `url_hash_initial`
#'       and `url_hash` can be used to get the components of the URL
#'       that was requested by the browser to load the Shiny app page.
#'       These values are from the browser's perspective, so neither HTTP
#'       proxies nor Shiny Server will affect these values. The
#'       `url_search` value may be used with [parseQueryString()]
#'       to access query string parameters.
#'     }
#'   }
#'   `clientData` also contains information about each output.
#'   \code{output_\var{outputId}_width} and \code{output_\var{outputId}_height}
#'   give the dimensions (using `offsetWidth` and `offsetHeight`) of
#'   the DOM element that is bound to \code{\var{outputId}}, and
#'   \code{output_\var{outputId}_hidden} is a logical that indicates whether
#'   the element is hidden. These values may be `NULL` if the output is
#'   not bound.
#' }
#' \item{input}{
#'   The session's `input` object (the same as is passed into the Shiny
#'   server function as an argument).
#' }
#' \item{isClosed()}{A function that returns `TRUE` if the client has
#'   disconnected.
#' }
#' \item{ns(id)}{
#'   Server-side version of [`ns <- NS(id)`][NS]. If bare IDs need to be
#'   explicitly namespaced for the current module, `session$ns("name")`
#'   will return the fully-qualified ID.
#' }
#' \item{onEnded(callback)}{
#'   Synonym for `onSessionEnded`.
#' }
#' \item{onFlush(func, once=TRUE)}{
#'   Registers a function to be called before the next time (if `once=TRUE`)
#'   or every time (if `once=FALSE`) Shiny flushes the reactive system.
#'   Returns a function that can be called with no arguments to cancel the
#'   registration.
#' }
#' \item{onFlushed(func, once=TRUE)}{
#'   Registers a function to be called after the next time (if `once=TRUE`)
#'   or every time (if `once=FALSE`) Shiny flushes the reactive system.
#'   Returns a function that can be called with no arguments to cancel the
#'   registration.
#' }
#' \item{onSessionEnded(callback)}{
#'   Registers a function to be called after the client has disconnected.
#'   Returns a function that can be called with no arguments to cancel the
#'   registration.
#' }
#' \item{output}{
#'   The session's `output` object (the same as is passed into the Shiny
#'   server function as an argument).
#' }
#' \item{reactlog}{
#'   For internal use.
#' }
#' \item{registerDataObj(name, data, filterFunc)}{
#'   Publishes any R object as a URL endpoint that is unique to this session.
#'   `name` must be a single element character vector; it will be used
#'   to form part of the URL. `filterFunc` must be a function that takes
#'   two arguments: `data` (the value that was passed into
#'   `registerDataObj`) and `req` (an environment that implements
#'   the Rook specification for HTTP requests). `filterFunc` will be
#'   called with these values whenever an HTTP request is made to the URL
#'   endpoint. The return value of `filterFunc` should be a Rook-style
#'   response.
#' }
#' \item{reload()}{
#'   The equivalent of hitting the browser's Reload button. Only works if the
#'   session is actually connected.
#' }
#' \item{request}{
#'   An environment that implements the [Rook
#'   specification](https://github.com/jeffreyhorner/Rook#the-environment) for
#'   HTTP requests. This is the request that was used to initiate the websocket
#'   connection (as opposed to the request that downloaded the web page for the
#'   app).
#' }
#' \item{userData}{
#'   An environment for app authors and module/package authors to store whatever
#'   session-specific data they want.
#' }
#' \item{user}{
#'   User's log-in information. Useful for identifying users on hosted platforms
#'   such as RStudio Connect and Shiny Server.
#' }
#' \item{groups}{
#'   The `user`'s relevant group information. Useful for determining what
#'   privileges the user should or shouldn't have.
#' }
#' \item{resetBrush(brushId)}{
#'   Resets/clears the brush with the given `brushId`, if it exists on
#'   any `imageOutput` or `plotOutput` in the app.
#' }
#' \item{sendCustomMessage(type, message)}{
#'   Sends a custom message to the web page. `type` must be a
#'   single-element character vector giving the type of message, while
#'   `message` can be any jsonlite-encodable value. Custom messages
#'   have no meaning to Shiny itself; they are used solely to convey information
#'   to custom JavaScript logic in the browser. You can do this by adding
#'   JavaScript code to the browser that calls
#'   \code{Shiny.addCustomMessageHandler(type, function(message){...})}
#'   as the page loads; the function you provide to
#'   `addCustomMessageHandler` will be invoked each time
#'   `sendCustomMessage` is called on the server.
#' }
#' \item{sendBinaryMessage(type, message)}{
#'   Similar to `sendCustomMessage`, but the message must be a raw vector
#'   and the registration method on the client is
#'   \code{Shiny.addBinaryMessageHandler(type, function(message){...})}. The
#'   message argument on the client will be a
#'   [DataView](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView).
#' }
#' \item{sendInputMessage(inputId, message)}{
#'   Sends a message to an input on the session's client web page; if the input
#'   is present and bound on the page at the time the message is received, then
#'   the input binding object's `receiveMessage(el, message)` method will
#'   be called. `sendInputMessage` should generally not be called directly
#'   from Shiny apps, but through friendlier wrapper functions like
#'   [updateTextInput()].
#' }
#' \item{setBookmarkExclude(names)}{
#'   Set input names to be excluded from bookmarking.
#' }
#' \item{getBookmarkExclude()}{
#'   Returns the set of input names to be excluded from bookmarking.
#' }
#' \item{onBookmark(fun)}{
#'   Registers a function that will be called just before bookmarking state.
#' }
#' \item{onBookmarked(fun)}{
#'   Registers a function that will be called just after bookmarking state.
#' }
#' \item{onRestore(fun)}{
#'   Registers a function that will be called when a session is restored, before
#'   all other reactives, observers, and render functions are run.
#' }
#' \item{onRestored(fun)}{
#'   Registers a function that will be called when a session is restored, after
#'   all other reactives, observers, and render functions are run.
#' }
#' \item{doBookmark()}{
#'   Do bookmarking and invoke the onBookmark and onBookmarked callback functions.
#' }
#' \item{exportTestValues()}{
#'   Registers expressions for export in test mode, available at the test
#'   snapshot URL.
#' }
#' \item{getTestSnapshotUrl(input=TRUE, output=TRUE, export=TRUE,
#'   format="json")}{
#'   Returns a URL for the test snapshots. Only has an effect when the
#'   `shiny.testmode` option is set to TRUE. For the input, output, and
#'   export arguments, TRUE means to return all of these values. It is also
#'   possible to specify by name which values to return by providing a
#'   character vector, as in `input=c("x", "y")`. The format can be
#'   "rds" or "json".
#' }
#' \item{setCurrentTheme(theme)}{
#'   Sets the current [bootstrapLib()] theme, which updates the value of
#'   [getCurrentTheme()], invalidates `session$getCurrentTheme()`, and calls
#'   function(s) registered with [registerThemeDependency()] with provided
#'   `theme`. If those function calls return [htmltools::htmlDependency()]s with
#'   `stylesheet`s, then those stylesheets are "refreshed" (i.e., the new
#'   stylesheets are inserted on the page and the old ones are disabled and
#'   removed).
#' }
#' \item{getCurrentTheme()}{
#'   A reactive read of the current [bootstrapLib()] theme.
#' }
#'
#' @name session
NULL

#' Namespaced IDs for inputs/outputs
#'
#' The `NS` function creates namespaced IDs out of bare IDs, by joining
#' them using `ns.sep` as the delimiter. It is intended for use in Shiny
#' modules. See <https://shiny.rstudio.com/articles/modules.html>.
#'
#' Shiny applications use IDs to identify inputs and outputs. These IDs must be
#' unique within an application, as accidentally using the same input/output ID
#' more than once will result in unexpected behavior. The traditional solution
#' for preventing name collisions is *namespaces*; a namespace is to an ID
#' as a directory is to a file. Use the `NS` function to turn a bare ID
#' into a namespaced one, by combining them with `ns.sep` in between.
#'
#' @param namespace The character vector to use for the namespace. This can have
#'   any length, though a single element is most common. Length 0 will cause the
#'   `id` to be returned without a namespace, and length 2 will be
#'   interpreted as multiple namespaces, in increasing order of specificity
#'   (i.e. starting with the top-level namespace).
#' @param id The id string to be namespaced (optional).
#' @return If `id` is missing, returns a function that expects an id string
#'   as its only argument and returns that id with the namespace prepended.
#' @seealso <https://shiny.rstudio.com/articles/modules.html>
#' @export
NS <- function(namespace, id = NULL) {
  if (length(namespace) == 0)
    ns_prefix <- character(0)
  else
    ns_prefix <- paste(namespace, collapse = ns.sep)

  f <- function(id) {
    if (length(id) == 0)
      return(ns_prefix)
    if (length(ns_prefix) == 0)
      return(id)

    paste(ns_prefix, id, sep = ns.sep)
  }

  if (missing(id)) {
    f
  } else {
    f(id)
  }
}

#' @rdname NS
#' @export
ns.sep <- "-"


#' @include utils.R
ShinySession <- R6Class(
  'ShinySession',
  private = list(
    # There are some private items with a leading "."; except for the dot, these
    # items share a name with a public item.
    websocket = 'ANY',
    invalidatedOutputValues = 'Map',
    invalidatedOutputErrors = 'Map',
    inputMessageQueue = 'fastqueue',     # A list of inputMessages to send when flushed
    cycleStartActionQueue = 'fastqueue', # A list of actions to perform to start a cycle
    .outputs = list(),          # Keeps track of all the output observer objects
    .outputOptions = list(),     # Options for each of the output observer objects
    progressKeys = 'character',
    showcase   = FALSE,
    fileUploadContext = 'FileUploadContext',
    .input      = 'ANY', # Internal ReactiveValues object for normal input sent from client
    .clientData = 'ANY', # Internal ReactiveValues object for other data sent from the client
    busyCount = 0L, # Number of observer callbacks that are pending. When 0, we are idle
    closedCallbacks = 'Callbacks',
    flushCallbacks = 'Callbacks',
    flushedCallbacks = 'Callbacks',
    inputReceivedCallbacks = 'Callbacks',
    unhandledErrorCallbacks = 'Callbacks',
    bookmarkCallbacks = 'Callbacks',
    bookmarkedCallbacks = 'Callbacks',
    restoreCallbacks = 'Callbacks',
    restoredCallbacks = 'Callbacks',
    bookmarkExclude = character(0),  # Names of inputs to exclude from bookmarking
    getBookmarkExcludeFuns = list(),
    timingRecorder = 'ShinyServerTimingRecorder',

    testMode = FALSE,                # Are we running in test mode?
    testExportExprs = list(),
    outputValues = list(),           # Saved output values (for testing mode)
    currentOutputName = NULL,        # Name of the currently-running output
    outputInfo = list(),             # List of information for each output
    testSnapshotUrl = character(0),
    currentThemeDependency = NULL,   # ReactiveVal for taking dependency on theme

    sendResponse = function(requestMsg, value) {
      if (is.null(requestMsg$tag)) {
        warning("Tried to send response for untagged message; method: ",
                requestMsg$method)
        return()
      }
      private$sendMessage(
        response = list(tag = requestMsg$tag, value = value)
      )
    },
    sendErrorResponse = function(requestMsg, error) {
      if (is.null(requestMsg$tag))
        return()
      private$sendMessage(
        response = list(tag = requestMsg$tag, error = error)
      )
    },
    write = function(json) {
      if (self$closed){
        return()
      }
      traceOption <- getOption('shiny.trace', FALSE)
      if (isTRUE(traceOption) || traceOption == "send")
        message('SEND ',
           gsub('(?m)base64,[a-zA-Z0-9+/=]+','[base64 data]',json,perl=TRUE))
      private$websocket$send(json)
    },
    sendMessage = function(...) {
      # This function is a wrapper for $write
      msg <- list(...)
      if (any_unnamed(msg)) {
        stop("All arguments to sendMessage must be named.")
      }
      private$write(toJSON(msg))
    },
    getOutputOption = function(outputName, propertyName, defaultValue) {
      opts <- private$.outputOptions[[outputName]]
      if (is.null(opts))
        return(defaultValue)
      result <- opts[[propertyName]]
      if (is.null(result))
        return(defaultValue)
      return(result)
    },
    withCurrentOutput = function(name, expr) {
      if (!is.null(private$currentOutputName)) {
        stop("Nested calls to withCurrentOutput() are not allowed.")
      }

      promises::with_promise_domain(
        createVarPromiseDomain(private, "currentOutputName", name),
        expr
      )
    },
    shouldSuspend = function(name) {
      # Find corresponding hidden state clientData variable, with the format
      # "output_foo_hidden". (It comes from .clientdata_output_foo_hidden
      # on the JS side)
      # Some tricky stuff: instead of accessing names using input$names(),
      # get the names directly via input$.values, to avoid triggering reactivity.
      # Need to handle cases where the output object isn't actually used
      # in the web page; in these cases, there's no output_foo_hidden flag,
      # and hidden should be TRUE. In other words, NULL and TRUE should map to
      # TRUE, FALSE should map to FALSE.
      hidden <- private$.clientData$.values$get(paste0("output_", name, "_hidden"))
      if (is.null(hidden)) hidden <- TRUE

      return(hidden && private$getOutputOption(name, 'suspendWhenHidden', TRUE))
    },

    registerSessionEndCallbacks = function() {
      # This is to be called from the initialization. It registers functions
      # that are called when a session ends.

      # Clear file upload directories, if present
      self$onSessionEnded(private$fileUploadContext$rmUploadDirs)
    },

    # Modules (scopes) call this to register a function that returns a vector
    # of names to exclude from bookmarking. The function should return
    # something like c("scope1-x", "scope1-y"). This doesn't use a Callback
    # object because the return values of the functions are needed, but
    # Callback$invoke() discards return values.
    registerBookmarkExclude = function(fun) {
      len <- length(private$getBookmarkExcludeFuns) + 1
      private$getBookmarkExcludeFuns[[len]] <- fun
    },

    # Save output values and errors. This is only used for testing mode.
    storeOutputValues = function(values = NULL) {
      private$outputValues <- mergeVectors(private$outputValues, values)
    },

    enableTestSnapshot = function() {
      private$testSnapshotUrl <- self$registerDataObj("shinytest", NULL,
        function(data, req) {
          if (!isTRUE(private$testMode)) {
            return()
          }

          params <- parseQueryString(req$QUERY_STRING)
          # The format of the response that will be sent back. Defaults to
          # "json" unless requested otherwise. The only other valid value is
          # "rds".
          format <- params$format %||% "json"
          # Machines can test their snapshot under different locales.
          # R CMD check runs under the `C` locale.
          # However, before this parameter, existing snapshots were most likely not
          #   under the `C` locale is would cause failures. This parameter allows
          #   users to opt-in to the `C` locale.
          # From ?sort:
          #   However, there are some caveats with the radix sort:
          #     If ‘x’ is a ‘character’ vector, all elements must share the
          #   same encoding. Only UTF-8 (including ASCII) and Latin-1
          #   encodings are supported. Collation always follows the "C"
          #   locale.
          # {shinytest2} will always set `sortC=1`
          # {shinytest} does not have `sortC` functionality.
          #    Users should set `options(shiny.snapshotsortc = TRUE)` within their app.
          # The sortingMethod should always be `radix` going forward.
          sortMethod <-
            if (!is.null(params$sortC)) {
              if (params$sortC != "1") {
                stop("The `sortC` parameter can only be `1` or not supplied")
              }
              "radix"
            } else {
              # Allow users to set an option for {shinytest2}.
              if (isTRUE(getShinyOption("snapshotsortc", default = FALSE))) {
                "radix"
              } else {
                "auto"
              }
            }

          values <- list()

          if (!is.null(params$input)) {

            # The isolate and reactiveValuesToList calls are being executed
            # in a non-reactive context, but will produce output in the reactlog
            # Seeing new, unlabelled reactives ONLY when calling shinytest is
            # jarring / frustrating to debug.
            # Since labeling these values is not currently supported in reactlog,
            # it is better to hide them.
            # Hopefully we can replace this with something like
            # `with_reactlog_group("shinytest", {})`, which would visibily explain
            # why the new reactives are added when calling shinytest
            withr::with_options(
              list(shiny.reactlog = FALSE),
              {
                allInputs <- isolate(
                  reactiveValuesToList(self$input, all.names = TRUE)
                )
              }
            )

            # If params$input is "1", return all; otherwise return just the
            # inputs that are named in params$input, like "x,y,z".
            if (params$input == "1") {
              values$input <- allInputs
            } else {
              items <- strsplit(params$input, ",")[[1]]
              items <- intersect(items, names(allInputs))
              values$input <- allInputs[items]
            }

            # Apply preprocessor functions for inputs that have them.
            values$input <- lapply(
              stats::setNames(names(values$input), names(values$input)),
              function(name) {
                preprocess <- private$getSnapshotPreprocessInput(name)
                preprocess(values$input[[name]])
              }
            )

            values$input <- sortByName(values$input, method = sortMethod)
          }

          if (!is.null(params$output)) {

            if (params$output == "1") {
              values$output <- private$outputValues
            } else {
              items <- strsplit(params$output, ",")[[1]]
              items <- intersect(items, names(private$outputValues))
              values$output <- private$outputValues[items]
            }

            # Filter out those outputs that have the snapshotExclude attribute.
            exclude_idx <- vapply(names(values$output), function(name) {
              isTRUE(attr(private$.outputs[[name]], "snapshotExclude", TRUE))
            }, logical(1))
            values$output <- values$output[!exclude_idx]

            # Apply snapshotPreprocess functions for outputs that have them.
            values$output <- lapply(
              stats::setNames(names(values$output), names(values$output)),
              function(name) {
                preprocess <- private$getSnapshotPreprocessOutput(name)
                preprocess(values$output[[name]])
              }
            )

            values$output <- sortByName(values$output, method = sortMethod)
          }

          if (!is.null(params$export)) {

            if (params$export == "1") {
              values$export <- isolate(
                lapply(private$testExportExprs, function(item) {
                  eval(item$expr, envir = item$env)
                })
              )
            } else {
              items <- strsplit(params$export, ",")[[1]]
              items <- intersect(items, names(private$testExportExprs))
              values$export <- isolate(
                lapply(private$testExportExprs[items], function(item) {
                  eval(item$expr, envir = item$env)
                })
              )
            }

            values$export <- sortByName(values$export, method = sortMethod)
          }

          # Make sure input, output, and export are all named lists (at this
          # point, they could be unnamed if they are empty lists). This is so
          # that the resulting object is represented as an object in JSON
          # instead of an array, and so that the RDS data structure is of a
          # consistent type.
          values <- lapply(values, asNamed)

          if (length(values) == 0) {
            return(httpResponse(400, "text/plain",
              "None of export, input, or output requested."
            ))
          }

          if (identical(format, "json")) {
            content <- toJSON(values, pretty = TRUE)
            httpResponse(200, "application/json", content)

          } else if (identical(format, "rds")) {
            tmpfile <- tempfile("shinytest", fileext = ".rds")
            saveRDS(values, tmpfile)
            on.exit(unlink(tmpfile), add = TRUE)

            content <- readBin(tmpfile, "raw", n = file.info(tmpfile)$size)
            httpResponse(200, "application/octet-stream", content)

          } else {
            httpResponse(400, "text/plain", paste("Invalid format requested:", format))
          }
        }
      )
    },

    # Get the snapshotPreprocessOutput function for an output name. If no preprocess
    # function has been set, return the identity function.
    getSnapshotPreprocessOutput = function(name) {
      fun <- attr(private$.outputs[[name]], "snapshotPreprocess", exact = TRUE)
      fun %||% identity
    },

    # Get the snapshotPreprocessInput function for an input name. If no preprocess
    # function has been set, return the identity function.
    getSnapshotPreprocessInput = function(name) {
      fun <- private$.input$getMeta(name, "shiny.snapshot.preprocess")
      fun %||% identity
    },

    # See cycleStartAction
    startCycle = function() {
      # TODO: This should check for busyCount == 0L, and remove the checks from
      # the call sites
      if (private$cycleStartActionQueue$size() > 0) {
        head <- private$cycleStartActionQueue$remove()

        # After we execute the current cycleStartAction (head), there may be
        # more items left on the queue. If the current busyCount > 0, then that
        # means an async task is running; whenever that task finishes, it will
        # decrement the busyCount back to 0 and a startCycle will then be
        # scheduled. But if the current busyCount is 0, it means that either
        # busyCount was incremented and then decremented; OR that running head()
        # never touched busyCount (one example of the latter is that an input
        # changed that didn't actually cause any observers to be invalidated,
        # i.e. an input that's used in the body of an observeEvent). Because of
        # the possibility of the latter case, we need to conditionally schedule
        # a startCycle ourselves to ensure that the remaining queue items get
        # processed.
        #
        # Since we can't actually tell whether head() increment and decremented
        # busyCount, it's possible we're calling startCycle spuriously; that's
        # OK, it's essentially a no-op in that case.
        on.exit({
          if (private$busyCount == 0L && private$cycleStartActionQueue$size() > 0L) {
            later::later(function() {
              if (private$busyCount == 0L) {
                private$startCycle()
              }
            })
          }
        }, add = TRUE)

        head()
      }

      invisible()
    }
  ),
  public = list(
    restoreContext = NULL,
    progressStack = 'Stack', # Stack of progress objects
    input       = 'reactivevalues', # Externally-usable S3 wrapper object for .input
    output      = 'ANY',    # Externally-usable S3 wrapper object for .outputs
    clientData  = 'reactivevalues', # Externally-usable S3 wrapper object for .clientData
    token = 'character',  # Used to identify this instance in URLs
    files = 'Map',        # For keeping track of files sent to client
    downloads = 'Map',
    closed = logical(0),
    request = 'ANY',      # Websocket request object
    singletons = character(0),  # Tracks singleton HTML fragments sent to the page
    userData = 'environment',
    cache = NULL,         # A cache object used in the session
    user = NULL,
    groups = NULL,
    options = NULL,       # For session-specific shinyOptions()

    initialize = function(websocket) {
      private$websocket <- websocket
      self$closed <- FALSE
      # TODO: Put file upload context in user/app-specific dir if possible

      private$inputMessageQueue     <- fastmap::fastqueue()
      private$cycleStartActionQueue <- fastmap::fastqueue()
      private$invalidatedOutputValues <- Map$new()
      private$invalidatedOutputErrors <- Map$new()
      private$fileUploadContext <- FileUploadContext$new()
      private$closedCallbacks <- Callbacks$new()
      private$flushCallbacks <- Callbacks$new()
      private$flushedCallbacks <- Callbacks$new()
      private$inputReceivedCallbacks <- Callbacks$new()
      private$unhandledErrorCallbacks <- Callbacks$new()
      private$.input      <- ReactiveValues$new(dedupe = FALSE, label = "input")
      private$.clientData <- ReactiveValues$new(dedupe = TRUE, label = "clientData")
      private$timingRecorder <- ShinyServerTimingRecorder$new()
      self$progressStack <- fastmap::faststack()
      self$files <- Map$new()
      self$downloads <- Map$new()
      self$userData <- new.env(parent = emptyenv())

      self$input <- .createReactiveValues(private$.input, readonly=TRUE)
      self$clientData <- .createReactiveValues(private$.clientData, readonly=TRUE)

      self$output <- .createOutputWriter(self)

      self$token <- createUniqueId(16)
      private$.outputs <- list()
      private$.outputOptions <- list()

      # Copy app-level options
      self$options <- getCurrentAppState()$options

      self$cache <- cachem::cache_mem(max_size = 200 * 1024^2)

      private$bookmarkCallbacks <- Callbacks$new()
      private$bookmarkedCallbacks <- Callbacks$new()
      private$restoreCallbacks <- Callbacks$new()
      private$restoredCallbacks <- Callbacks$new()

      private$testMode <- getShinyOption("testmode", default = FALSE)
      private$enableTestSnapshot()

      # This `withReactiveDomain` is used only to satisfy the reactlog, so that
      # it knows to scope this reactiveVal to this session.
      # https://github.com/rstudio/shiny/pull/3182
      withReactiveDomain(self,
        private$currentThemeDependency <- reactiveVal(0, label = "Theme Counter")
      )

      private$registerSessionEndCallbacks()

      if (!is.null(websocket$request$HTTP_SHINY_SERVER_CREDENTIALS)) {
        try({
          creds <- safeFromJSON(websocket$request$HTTP_SHINY_SERVER_CREDENTIALS)
          self$user <- creds$user
          self$groups <- creds$groups
        }, silent=FALSE)
      }

      # session$request should throw an error if httpuv doesn't have
      # websocket$request, but don't throw it until a caller actually
      # tries to access session$request
      delayedAssign('request', websocket$request, assign.env = self)

      private$sendMessage(
        config = list(
          workerId = workerId(),
          sessionId = self$token,
          user = self$user
        )
      )
    },
    startTiming = function(guid) {
      if (!is.null(guid)) {
        private$timingRecorder$start(guid)
        self$onFlush(private$timingRecorder$stop)
      }
    },
    requestFlush = function() {
      appsNeedingFlush$set(self$token, self)
    },
    .scheduleTask = function(millis, callback) {
      scheduleTask(millis, callback)
    },
    .now = function(){
      getTimeMs()
    },
    rootScope = function() {
      self
    },
    makeScope = function(namespace) {
      ns <- NS(namespace)

      # Private items for this scope. Can't be part of the scope object because
      # `$<-.session_proxy` doesn't allow assignment on overidden names.
      bookmarkCallbacks <- Callbacks$new()
      restoreCallbacks  <- Callbacks$new()
      restoredCallbacks <- Callbacks$new()
      bookmarkExclude   <- character(0)

      scope <- createSessionProxy(self,
        input = .createReactiveValues(private$.input, readonly = TRUE, ns = ns),
        output = .createOutputWriter(self, ns = ns),
        sendInputMessage = function(inputId, message) {
          .subset2(self, "sendInputMessage")(ns(inputId), message)
        },
        registerDataObj = function(name, data, filterFunc) {
          .subset2(self, "registerDataObj")(ns(name), data, filterFunc)
        },
        ns = ns,
        makeScope = function(namespace) {
          self$makeScope(ns(namespace))
        },

        setBookmarkExclude = function(names) {
          bookmarkExclude <<- names
        },
        getBookmarkExclude = function() {
          bookmarkExclude
        },
        onBookmark = function(fun) {
          if (!is.function(fun) || length(fun) != 1) {
            stop("`fun` must be a function that takes one argument")
          }
          bookmarkCallbacks$register(fun)
        },
        onBookmarked = function(fun) {
          stop("onBookmarked() can't be used in a module.")
        },
        onRestore = function(fun) {
          if (!is.function(fun) || length(fun) != 1) {
            stop("`fun` must be a function that takes one argument")
          }
          restoreCallbacks$register(fun)
        },
        onRestored = function(fun) {
          if (!is.function(fun) || length(fun) != 1) {
            stop("`fun` must be a function that takes one argument")
          }
          restoredCallbacks$register(fun)
        },
        exportTestValues = function(..., quoted_ = FALSE, env_ = parent.frame()) {
          if (quoted_) {
            dots <- list(...)
          } else {
            dots <- eval(substitute(alist(...)))
          }

          if (any_unnamed(dots))
            stop("exportTestValues: all arguments must be named.")

          names(dots) <- ns(names(dots))

          do.call(
            .subset2(self, "exportTestValues"),
            c(dots, quoted_ = TRUE, env_ = env_),
            quote = TRUE
          )
        }
      )

      # Given a char vector, return a logical vector indicating which of those
      # strings are names of things in the namespace.
      filterNamespace <- function(x) {
        nsString <- paste0(namespace, ns.sep)
        substr(x, 1, nchar(nsString)) == nsString
      }

      # Given a char vector of namespaced names, return a char vector of corresponding
      # names with namespace prefix removed.
      unNamespace <- function(x) {
        if (!all(filterNamespace(x))) {
          stop("x contains strings(s) that do not have namespace prefix ", namespace)
        }

        nsString <- paste0(namespace, ns.sep)
        substring(x, nchar(nsString) + 1)
      }

      # Given a restore state object (a list), return a modified version that's
      # scoped to this namespace.
      scopeRestoreState <- function(state) {
        # State is a list. We need to copy and transform some things for the
        # scope.
        scopeState <- state
        # `values` is an environment and we don't want to modify the original.
        scopeState$values <- new.env(parent = emptyenv())

        # Keep only inputs that are in the scope, and rename them
        scopeState$input <- scopeState$input[filterNamespace(names(scopeState$input))]
        names(scopeState$input) <- unNamespace(names(scopeState$input))

        # Same for values. This is an environment so we have to handle a little
        # differently.
        origNames <- names(state$values)
        origNames <- origNames[filterNamespace(origNames)]
        lapply(origNames, function(origName) {
          scopedName <- unNamespace(origName)
          scopeState$values[[scopedName]] <- state$values[[origName]]
        })

        if (!is.null(state$dir)) {
          dir <- file.path(state$dir, namespace)
          if (dirExists(dir))
            scopeState$dir <- dir
        }

        scopeState
      }

      # When scope is created, register these bookmarking callbacks on the main
      # session object. They will invoke the scope's own callbacks, if any are
      # present.
      self$onBookmark(function(state) {
        # Exit if no user-defined callbacks.
        if (bookmarkCallbacks$count() == 0)
          return()

        scopeState <- ShinySaveState$new(scope$input, scope$getBookmarkExclude())

        # Create subdir for this scope
        if (!is.null(state$dir)) {
          scopeState$dir <- file.path(state$dir, namespace)
          if (!dirExists(scopeState$dir)) {
            res <- dir.create(scopeState$dir)
            if (res == FALSE) {
              stop("Error creating subdirectory for scope ", namespace)
            }
          }
        }

        # Invoke the callback on the scopeState object
        bookmarkCallbacks$invoke(scopeState)

        # Copy `values` from scopeState to state, adding namespace
        if (length(scopeState$values) != 0) {
          if (any_unnamed(scopeState$values)) {
            stop("All scope values in must be named.")
          }

          lapply(names(scopeState$values), function(origName) {
            scopedName <- ns(origName)
            state$values[[scopedName]] <- scopeState$values[[origName]]
          })
        }
      })

      self$onRestore(function(state) {
        # Exit if no user-defined callbacks.
        if (restoreCallbacks$count() == 0)
          return()

        scopeState <- scopeRestoreState(state)
        # Invoke user callbacks
        restoreCallbacks$invoke(scopeState)
      })

      self$onRestored(function(state) {
        # Exit if no user-defined callbacks.
        if (restoredCallbacks$count() == 0)
          return()

        scopeState <- scopeRestoreState(state)
        # Invoke user callbacks
        restoredCallbacks$invoke(scopeState)
      })

      # Returns the excluded names with the scope's ns prefix on them.
      private$registerBookmarkExclude(function() {
        excluded <- scope$getBookmarkExclude()
        ns(excluded)
      })

      scope
    },
    ns = function(id) {
      NS(NULL, id)
    },

    # Freeze a value until the flush cycle completes
    freezeValue = function(x, name) {
      if (!is.reactivevalues(x))
        stop("x must be a reactivevalues object")

      impl <- .subset2(x, 'impl')
      key <- .subset2(x, 'ns')(name)

      is_input <- identical(impl, private$.input)

      # There's no good reason for us not to just do force=TRUE, except that we
      # know this fixes problems for freezeReactiveValue(input) but we don't
      # currently even know what you would use freezeReactiveValue(rv) for. In
      # the spirit of not breaking things we don't understand, we're making as
      # targeted a fix as possible, while emitting a deprecation warning (below)
      # that should help us gather more data about the other case.
      impl$freeze(key, invalidate = is_input)

      if (is_input) {
        # Notify the client that this input was frozen. The client will ensure
        # that the next time it sees a value for that input, even if the value
        # has not changed from the last known value of that input, it will be
        # sent to the server anyway.
        private$sendMessage(frozen = list(
          ids = list(key)
        ))
      } else {
        if (getOption("shiny.deprecation.messages", TRUE) && getOption("shiny.deprecation.messages.freeze", TRUE)) {
          rlang::warn(
            "Support for calling freezeReactiveValue() with non-`input` reactiveValues objects is soft-deprecated, and may be removed in a future version of Shiny. (See https://github.com/rstudio/shiny/issues/3063)",
            .frequency = "once", .frequency_id = "freezeReactiveValue")
        }
      }

      self$onFlushed(function() impl$thaw(key))
    },

    onSessionEnded = function(sessionEndedCallback) {
      "Registers the given callback to be invoked when the session is closed
      (i.e. the connection to the client has been severed). The return value
      is a function which unregisters the callback. If multiple callbacks are
      registered, the order in which they are invoked is not guaranteed."
      return(private$closedCallbacks$register(sessionEndedCallback))
    },
    onEnded = function(endedCallback) {
      "Synonym for onSessionEnded"
      return(self$onSessionEnded(endedCallback))
    },
    onInputReceived = function(callback) {
      "Registers the given callback to be invoked when the session receives
      new data from the client."
      return(private$inputReceivedCallbacks$register(callback))
    },
    onUnhandledError = function(callback) {
      "Registers the callback to be invoked when an unhandled error occurs."
      return(private$unhandledErrorCallbacks$register(callback))
    },
    unhandledError = function(e, close = TRUE) {
      "Call the global and session unhandled error handlers and then close the
       session if the error is fatal."
      if (close) {
        class(e) <- c("shiny.error.fatal", class(e))
      }

      private$unhandledErrorCallbacks$invoke(e, onError = printError)
      .globals$onUnhandledErrorCallbacks$invoke(e, onError = printError)

      if (close) self$close()
    },
    close = function() {
      if (!self$closed) {
        private$websocket$close()
      }
    },
    wsClosed = function() {
      self$closed <- TRUE
      for (output in private$.outputs) {
        output$suspend()
      }
      # ..stacktraceon matches with the top-level ..stacktraceoff..
      withReactiveDomain(self, {
        private$closedCallbacks$invoke(onError = printError, ..stacktraceon = TRUE)
      })
    },
    isClosed = function() {
      return(self$closed)
    },
    isEnded = function() {
      return(self$isClosed())
    },
    setShowcase = function(value) {
      private$showcase <- !is.null(value) && as.logical(value)
    },

    allowReconnect = function(value) {
      if (!(identical(value, TRUE) || identical(value, FALSE) || identical(value, "force"))) {
        stop('value must be TRUE, FALSE, or "force"')
      }
      private$write(toJSON(list(allowReconnect = value)))
    },

    defineOutput = function(name, func, label) {
      "Binds an output generating function to this name. The function can either
      take no parameters, or have named parameters for \\code{name} and
      \\code{shinysession} (in the future this list may expand, so it is a good idea
      to also include \\code{...} in your function signature)."

      # jcheng 08/31/2012: User submitted an example of a dynamically calculated
      # name not working unless name was eagerly evaluated. Yikes!
      force(name)

      # If overwriting an output object, destroy the previous copy of it
      if (!is.null(private$.outputs[[name]])) {
        private$.outputs[[name]]$destroy()
      }

      if (is.null(func)) {
        # If func is null, give it an "empty" output function so it can go
        # through the logic below. If we simply returned at this point, the
        # previous output (if any) would continue to show in the client.
        func <- missingOutput
      }

      if (is.function(func)) {
        # Extract any output attributes attached to the render function. These
        # will be attached to the observer after it's created.
        outputAttrs <- attr(func, "outputAttrs", TRUE)

        # Save this for getOutput purposes
        outputAttrs$renderFunc <- func

        funcFormals <- formals(func)
        # ..stacktraceon matches with the top-level ..stacktraceoff.., because
        # the observer we set up below has ..stacktraceon=FALSE
        func <- wrapFunctionLabel(func, paste0("output$", name), ..stacktraceon = TRUE)
        if (length(funcFormals) != 0) {
          orig <- func
          func <- function() {
            orig(name=name, shinysession=self)
          }
        }

        # Preserve source reference and file information when formatting the
        # label for display in the reactive graph
        srcref <- attr(label, "srcref")
        srcfile <- attr(label, "srcfile")
        label <- sprintf('output$%s', name)
        attr(label, "srcref") <- srcref
        attr(label, "srcfile") <- srcfile

        obs <- observe(..stacktraceon = FALSE, {

          private$sendMessage(recalculating = list(
            name = name, status = 'recalculating'
          ))

          # This shinyCallingHandlers should maybe be at a higher level,
          # to include the $then/$catch calls below?
          hybrid_chain(
            hybrid_chain(
              {
                private$withCurrentOutput(name, {
                  shinyCallingHandlers(func())
                })
              },
              catch = function(cond) {
                if (inherits(cond, "shiny.custom.error")) {
                  if (isTRUE(getOption("show.error.messages"))) printError(cond)
                  structure(list(), class = "try-error", condition = cond)
                } else if (inherits(cond, "shiny.output.cancel")) {
                  structure(list(), class = "cancel-output")
                } else if (inherits(cond, "shiny.output.progress")) {
                  structure(list(), class = "progress-output")
                } else if (cnd_inherits(cond, "shiny.silent.error")) {
                  # The error condition might have been chained by
                  # foreign code, e.g. dplyr. Find the original error.
                  while (!inherits(cond, "shiny.silent.error")) {
                    cond <- cond$parent
                  }
                  # Don't let shiny.silent.error go through the normal stop
                  # path of try, because we don't want it to print. But we
                  # do want to try to return the same looking result so that
                  # the code below can send the error to the browser.
                  structure(list(), class = "try-error", condition = cond)
                } else {
                  if (isTRUE(getOption("show.error.messages"))) printError(cond)
                  if (getOption("shiny.sanitize.errors", FALSE)) {
                    cond <- simpleError(paste("An error has occurred. Check your",
                      "logs or contact the app author for",
                      "clarification."))
                  }
                  self$unhandledError(cond, close = FALSE)
                  invisible(structure(list(), class = "try-error", condition = cond))
                }
              }
            ),
            function(value) {
              # Needed so that Shiny knows to flush the outputs. Even if no
              # outputs/errors are queued, it's necessary to flush so that the
              # client knows that progress is over.
              self$requestFlush()

              if (inherits(value, "progress-output")) {
                # This is the case where an output needs to compute for longer
                # than this reactive flush. We put the output into progress mode
                # (i.e. adding .recalculating) with a special flag that means
                # the progress indication should not be cleared until this
                # specific output receives a new value or error.
                self$showProgress(name, persistent=TRUE)

                # It's conceivable that this output already ran successfully
                # within this reactive flush, in which case we could either show
                # the new output while simultaneously making it .recalculating;
                # or we squelch the new output and make whatever output is in
                # the client .recalculating. I (jcheng) decided on the latter as
                # it seems more in keeping with what we do with these kinds of
                # intermediate output values/errors in general, i.e. ignore them
                # and wait until we have a final answer. (Also kind of feels
                # like a bug in the app code if you routinely have outputs that
                # are executing successfully, only to be invalidated again
                # within the same reactive flush--use priority to fix that.)
                private$invalidatedOutputErrors$remove(name)
                private$invalidatedOutputValues$remove(name)

                # It's important that we return so that the existing output in
                # the client remains untouched.
                return()
              }

              private$sendMessage(recalculating = list(
                name = name, status = 'recalculated'
              ))

              if (inherits(value, "cancel-output")) {
                return()
              }

              private$invalidatedOutputErrors$remove(name)
              private$invalidatedOutputValues$remove(name)

              if (inherits(value, 'try-error')) {
                cond <- attr(value, 'condition')
                type <- setdiff(class(cond), c('simpleError', 'error', 'condition'))
                private$invalidatedOutputErrors$set(
                  name,
                  list(message = cond$message,
                    call = utils::capture.output(print(cond$call)),
                    type = if (length(type)) type))
              }
              else
                private$invalidatedOutputValues$set(name, value)
            }
          )
        }, suspended=private$shouldSuspend(name), label=label)

        # If any output attributes were added to the render function attach
        # them to observer.
        lapply(names(outputAttrs), function(name) {
          attr(obs, name) <- outputAttrs[[name]]
        })

        obs$onInvalidate(function() {
          self$showProgress(name)
        })

        private$.outputs[[name]] <- obs
        if (is.null(private$.outputOptions[[name]]))
          private$.outputOptions[[name]] <- list()
      }
      else {
        rlang::abort(c(
          paste0("Unexpected ", class(func)[[1]], " object for output$", name),
          i = "Did you forget to use a render function?"
        ))
      }
    },
    getOutput = function(name) {
      attr(private$.outputs[[name]], "renderFunc", exact = TRUE)
    },
    flushOutput = function() {
      if (private$busyCount > 0)
        return()

      appsNeedingFlush$remove(self$token)

      if (self$isClosed())
        return()

      # This is the only place in the session where the restoreContext is
      # flushed.
      if (!is.null(self$restoreContext))
        self$restoreContext$flushPending()

      # Return TRUE if there's any stuff to send to the client.
      hasPendingUpdates <- function() {
        # Even though progressKeys isn't sent to the client, we use it in this
        # check. This is because if it is non-empty, sending `values` to the
        # client tells it that the flushReact loop is finished, and the client
        # then knows to stop showing progress.
        return(
          length(private$progressKeys) != 0 ||
          length(private$invalidatedOutputValues) != 0 ||
          length(private$invalidatedOutputErrors) != 0 ||
          private$inputMessageQueue$size() != 0
        )
      }

      withReactiveDomain(self, {
        # ..stacktraceon matches with the top-level ..stacktraceoff..
        private$flushCallbacks$invoke(..stacktraceon = TRUE)

        # Schedule execution of onFlushed callbacks
        on.exit({
          withReactiveDomain(self, {
            # ..stacktraceon matches with the top-level ..stacktraceoff..
            private$flushedCallbacks$invoke(..stacktraceon = TRUE)
          })
        }, add = TRUE)

        if (!hasPendingUpdates()) {
          # Normally, if there are no updates, simply return without sending
          # anything to the client. But if we are in test mode, we still want to
          # send a message with blank `values`, so that the client knows that
          # any changed inputs have been received by the server and processed.
          if (isTRUE(private$testMode)) {
            private$sendMessage( values = list() )
          }
          return(invisible())
        }

        private$progressKeys <- character(0)
        values <- as.list(private$invalidatedOutputValues)
        private$invalidatedOutputValues <- Map$new()
        errors <- as.list(private$invalidatedOutputErrors)
        private$invalidatedOutputErrors <- Map$new()
        inputMessages <- private$inputMessageQueue$as_list()
        private$inputMessageQueue$reset()

        if (isTRUE(private$testMode)) {
          private$storeOutputValues(mergeVectors(values, errors))
        }

        private$sendMessage(
          errors = errors,
          values = values,
          inputMessages = inputMessages
        )
      })
    },
    # Schedule an action to execute not (necessarily) now, but when no observers
    # that belong to this session are busy executing. This helps prevent (but
    # does not guarantee) inputs and reactive values from changing underneath
    # async observers as they run.
    cycleStartAction = function(callback) {
      private$cycleStartActionQueue$add(callback)
      # If no observers are running in this session, we're safe to proceed.
      # Otherwise, startCycle() will be called later, via decrementBusyCount().
      if (private$busyCount == 0L) {
        private$startCycle()
      }
    },
    showProgress = function(id, persistent=FALSE) {
      'Send a message to the client that recalculation of the output identified
      by \\code{id} is in progress. There is currently no mechanism for
      explicitly turning off progress for an output component; instead, all
      progress is implicitly turned off when flushOutput is next called.

      You can use persistent=TRUE if the progress for this output component
      should stay on beyond the flushOutput (or any subsequent flushOutputs); in
      that case, progress is only turned off (and the persistent flag cleared)
      when the output component receives a value or error, or, if
      showProgress(id, persistent=FALSE) is called and a subsequent flushOutput
      occurs.'

      # If app is already closed, be sure not to show progress, otherwise we
      # will get an error because of the closed websocket
      if (self$closed)
        return()

      if (!id %in% private$progressKeys) {
        private$progressKeys <- c(private$progressKeys, id)
      }

      self$sendProgress('binding', list(id = id, persistent = persistent))
    },
    sendProgress = function(type, message) {
      private$sendMessage(
        progress = list(type = type, message = message)
      )
    },
    sendNotification = function(type, message) {
      private$sendMessage(
        notification = list(type = type, message = message)
      )
    },
    sendModal = function(type, message) {
      private$sendMessage(
        modal = list(type = type, message = message)
      )
    },

    getCurrentTheme = function() {
      private$currentThemeDependency()
      getCurrentTheme()
    },

    setCurrentTheme = function(theme) {
      # This function does three things: (1) sets theme as the current
      # bootstrapTheme, (2) re-executes any registered theme dependencies, and
      # (3) sends the resulting dependencies to the client.

      if (!is_bs_theme(theme)) {
        stop("`session$setCurrentTheme()` expects a `bslib::bs_theme()` object.", call. = FALSE)
      }

      # Switching Bootstrap versions has weird & complex consequences
      # for the JS logic, so we forbid it
      current_version <- bslib::theme_version(getCurrentTheme())
      next_version <- bslib::theme_version(theme)
      if (!identical(current_version, next_version)) {
        stop(
          "session$setCurrentTheme() cannot be used to change the Bootstrap version ",
          "from ", current_version, " to ", next_version, ". ",
          "Try using `bs_theme(version = ", next_version, ")` for initial theme.",
          call. = FALSE
        )
      }

      # Note that this will automatically scope to the session.
      setCurrentTheme(theme)

      # Invalidate
      private$currentThemeDependency(isolate(private$currentThemeDependency()) + 1)

      # Call any theme dependency functions and make sure we get a list of deps back
      funcs <- getShinyOption("themeDependencyFuncs", default = list())
      deps <- lapply(funcs, function(func) {
        deps <- func(theme)
        if (length(deps) == 0) return(NULL)
        if (inherits(deps, "html_dependency")) return(list(deps))
        is_dep <- vapply(deps, inherits, logical(1), "html_dependency")
        if (all(is_dep)) return(deps)
        stop("All registerThemeDependency() functions must yield htmlDependency() object(s)", call. = FALSE)
      })
      # Work with a flat list of dependencies
      deps <- unlist(dropNulls(deps), recursive = FALSE)
      # Add a special flag to let Shiny.renderDependencies() know that, even
      # though we've already rendered the dependency, that we need to re-render
      # the stylesheets
      deps <- lapply(deps, function(dep) {
        dep$restyle <- TRUE
        dep
      })

      # Send any dependencies to be re-rendered
      if (length(deps)) {
        insertUI(selector = "body", where = "afterEnd", ui = tagList(deps))
      }
    },

    dispatch = function(msg) {
      method <- paste('@', msg$method, sep='')
      func <- try(self[[method]], silent = TRUE)
      if (inherits(func, 'try-error')) {
        private$sendErrorResponse(msg, paste('Unknown method', msg$method))
      }

      value <- try(do.call(func, as.list(append(msg$args, msg$blobs))),
                   silent=TRUE)
      if (inherits(value, 'try-error')) {
        private$sendErrorResponse(msg, conditionMessage(attr(value, 'condition')))
      }
      else {
        private$sendResponse(msg, value)
      }
    },
    sendBinaryMessage = function(type, message) {
      typeBytes <- charToRaw(type)
      if (length(typeBytes) > 255) {
        stop("'type' argument is too long")
      }
      private$write(c(as.raw(length(typeBytes)), typeBytes, message))
    },
    sendCustomMessage = function(type, message) {
      data <- list()
      data[[type]] <- message
      private$sendMessage(custom = data)
    },
    sendInputMessage = function(inputId, message) {
      data <- list(id = inputId, message = message)

      private$inputMessageQueue$add(data)
      # Needed so that Shiny knows to actually flush the input message queue
      self$requestFlush()
    },
    onFlush = function(flushCallback, once = TRUE) {
      if (!isTRUE(once)) {
        return(private$flushCallbacks$register(flushCallback))
      } else {
        dereg <- private$flushCallbacks$register(function() {
          dereg()
          flushCallback()
        })
        return(dereg)
      }
    },
    onFlushed = function(flushedCallback, once = TRUE) {
      if (!isTRUE(once)) {
        return(private$flushedCallbacks$register(flushedCallback))
      } else {
        dereg <- private$flushedCallbacks$register(function() {
          dereg()
          flushedCallback()
        })
        return(dereg)
      }
    },

    getCurrentOutputInfo = function() {
      name <- private$currentOutputName
      if (is.null(name)) {
        return(NULL)
      }

      if (!is.null(private$outputInfo[[name]])) {
        return(private$outputInfo[[name]])
      }

      # The following code will only run the first time this function has been
      # called for this output.

      tmp_info <- list(name = name)

      # cd_names() returns names of all items in clientData, without taking a
      # reactive dependency. It is a function and it's memoized, so that we do
      # the (relatively) expensive isolate(names(...)) call only when needed,
      # and at most one time in this function.
      cd_names <- isolate(names(self$clientData))

      # parseCssColors() currently errors out if you hand it any NAs
      # This'll make sure we're always working with a string (and if
      # that string isn't a valid CSS color, will return NA)
      # https://github.com/rstudio/htmltools/issues/161
      parse_css_colors <- function(x) {
        htmltools::parseCssColors(x %||% "", mustWork = FALSE)
      }


      # This function conditionally adds an item to tmp_info (for "width", it
      # would create tmp_info$width). It is added _if_ there is an entry in
      # clientData like "output_foo_width", where "foo" is the name of the
      # output. The first time `tmp_info$width()` is called, it creates a
      # reactive expression that reads `clientData$output_foo_width`, saves it,
      # then invokes that reactive. On subsequent calls, the reactive already
      # exists, so it simply invokes it.
      #
      # The reason it creates the reactive only on first use is so that it
      # doesn't spuriously create reactives.
      #
      # This function essentially generalizes the code below for names other
      # than just "width".
      #
      # width_name <- paste0("output_", name, "_width")
      # if (width_name %in% cd_names()) {
      #   width_r <- NULL
      #   tmp_info$width <- function() {
      #     if (is.null(width_r)) {
      #       width_r <<- reactive({
      #         parse_css_colors(self$clientData[[width_name]])
      #       })
      #     }
      #
      #     width_r()
      #   }
      # }
      add_conditional_reactive <- function(prop, wrapfun = identity) {
        force(prop)
        force(wrapfun)

        prop_name <- paste0("output_", name, "_", prop)

        # Only add tmp_info$width if clientData has "output_foo_width"
        if (prop_name %in% cd_names) {
          r <- NULL

          # Turn it into a function that creates a reactive on the first
          # invocation of getCurrentOutputInfo()$width() and saves it; future
          # invocations of getCurrentOutputInfo()$width() use the existing
          # reactive and save it.
          tmp_info[[prop]] <<- function() {
            if (is.null(r)) {
              r <<- reactive(label = prop_name, {
                wrapfun(self$clientData[[prop_name]])
              })
            }

            r()
          }
        }
      }


      # Note that all the following clientData values (which are reactiveValues)
      # are wrapped in reactive() so that users can take a dependency on
      # particular output info (i.e., just depend on width/height, or just
      # depend on bg, fg, etc). To put it another way, if getCurrentOutputInfo()
      # simply returned a list of values from self$clientData, than anything
      # that calls getCurrentOutputInfo() would take a reactive dependency on
      # all of these values.
      add_conditional_reactive("width")
      add_conditional_reactive("height")
      add_conditional_reactive("bg",     parse_css_colors)
      add_conditional_reactive("fg",     parse_css_colors)
      add_conditional_reactive("accent", parse_css_colors)
      add_conditional_reactive("font")

      private$outputInfo[[name]] <- tmp_info
      private$outputInfo[[name]]
    },

    createBookmarkObservers = function() {
      # This registers observers for bookmarking to work.

      # Get bookmarking config
      store <- getShinyOption("bookmarkStore", default = "disable")
      if (store == "disable")
        return()

      # Warn if trying to enable save-to-server bookmarking on a version of SS,
      # SSP, or Connect that doesn't support it.
      if (store == "server" && inShinyServer() &&
          is.null(getShinyOption("save.interface", default = NULL)))
      {
        showNotification(
          "This app tried to enable saved-to-server bookmarking, but it is not supported by the hosting environment.",
          duration = NULL, type = "warning", session = self
        )
        return()
      }

      withReactiveDomain(self, {
        # This observer fires when the bookmark button is clicked.
        observeEvent(self$input[["._bookmark_"]], {
          self$doBookmark()
        })

        # If there was an error initializing the current restore context, show
        # notification in the client.
        observe({
          rc <- getCurrentRestoreContext()
          if (!is.null(rc$initErrorMessage)) {
            showNotification(
              paste("Error in RestoreContext initialization:", rc$initErrorMessage),
              duration = NULL, type = "error"
            )
          }
        })

        # Run the onRestore function at the beginning of the flush cycle, but after
        # the server function has been executed.
        observe({
          if (private$restoreCallbacks$count() > 0) {
            tryCatch(
              withLogErrors(
                isolate({
                  rc <- getCurrentRestoreContext()
                  if (rc$active) {
                    restoreState <- getCurrentRestoreContext()$asList()
                    private$restoreCallbacks$invoke(restoreState)
                  }
                })
              ),
              error = function(e) {
                showNotification(
                  paste0("Error calling onRestore callback: ", e$message),
                  duration = NULL, type = "error"
                )
              }
            )
          }
        }, priority = 1000000)

        # Run the onRestored function after the flush cycle completes and information
        # is sent to the client.
        self$onFlushed(function() {
          if (private$restoredCallbacks$count() > 0) {

            tryCatch(
              withLogErrors(
                isolate({
                  rc <- getCurrentRestoreContext()
                  if (rc$active) {
                    restoreState <- getCurrentRestoreContext()$asList()
                    private$restoredCallbacks$invoke(restoreState)
                  }
                })
              ),
              error = function(e) {
                msg <- paste0("Error calling onRestored callback: ", e$message)
                showNotification(msg, duration = NULL, type = "error")
              }
            )
          }
        })

      }) # withReactiveDomain
    },

    setBookmarkExclude = function(names) {
      private$bookmarkExclude <- names
    },
    getBookmarkExclude = function() {
      scopedExcludes <- lapply(private$getBookmarkExcludeFuns, function(f) f())
      scopedExcludes <- unlist(scopedExcludes)

      c(private$bookmarkExclude, scopedExcludes)
    },

    onBookmark = function(fun) {
      if (!is.function(fun) || length(fun) != 1) {
        stop("`fun` must be a function that takes one argument")
      }
      private$bookmarkCallbacks$register(fun)
    },
    onBookmarked = function(fun) {
      if (!is.function(fun) || length(fun) != 1) {
        stop("`fun` must be a function that takes one argument")
      }
      private$bookmarkedCallbacks$register(fun)
    },
    onRestore = function(fun) {
      if (!is.function(fun) || length(fun) != 1) {
        stop("`fun` must be a function that takes one argument")
      }
      private$restoreCallbacks$register(fun)
    },
    onRestored = function(fun) {
      if (!is.function(fun) || length(fun) != 1) {
        stop("`fun` must be a function that takes one argument")
      }
      private$restoredCallbacks$register(fun)
    },
    doBookmark = function() {
      # Get bookmarking store config
      store <- getShinyOption("bookmarkStore", default = "disable")
      if (store == "disable")
        return()

      tryCatch(
        withLogErrors({
          saveState <- ShinySaveState$new(
            input = self$input,
            exclude = self$getBookmarkExclude(),
            onSave = function(state) {
              private$bookmarkCallbacks$invoke(state)
            }
          )

          if (store == "server") {
            url <- saveShinySaveState(saveState)
          } else if (store == "url") {
            url <- encodeShinySaveState(saveState)
          } else {
            stop("Unknown store type: ", store)
          }

          clientData <- self$clientData
          url <- paste0(
            clientData$url_protocol, "//",
            clientData$url_hostname,
            if (nzchar(clientData$url_port)) paste0(":", clientData$url_port),
            clientData$url_pathname,
            "?", url
          )


          # If onBookmarked callback was provided, invoke it; if not call
          # the default.
          if (private$bookmarkedCallbacks$count() > 0) {
            private$bookmarkedCallbacks$invoke(url)
          } else {
            showBookmarkUrlModal(url)
          }
        }),
        error = function(e) {
          msg <- paste0("Error bookmarking state: ", e$message)
          showNotification(msg, duration = NULL, type = "error")
        }
      )
    },

    exportTestValues = function(..., quoted_ = FALSE, env_ = parent.frame()) {
      # Get a named list of unevaluated expressions.
      if (quoted_) {
        dots <- list(...)
      } else {
        dots <- eval(substitute(alist(...)))
      }

      if (any_unnamed(dots))
        stop("exportTestValues: all arguments must be named.")

      # Create a named list where each item is a list with an expression and
      # environment in which to eval the expression.
      items <- lapply(dots, function(expr) {
        list(expr = expr, env = env_)
      })

      private$testExportExprs <- mergeVectors(private$testExportExprs, items)
    },

    getTestSnapshotUrl = function(input = TRUE, output = TRUE, export = TRUE,
                                  format = "json", sortC = FALSE) {
      reqString <- function(group, value) {
        if (isTRUE(value))
          paste0(group, "=1")
        else if (is.character(value))
          paste0(group, "=", paste(value, collapse = ","))
        else
          ""
      }
      paste(
        private$testSnapshotUrl,
        reqString("input", input),
        reqString("output", output),
        reqString("export", export),
        reqString("sortC", sortC),
        paste0("format=", format),
        sep = "&"
      )
    },

    reactlog = function(logEntry) {
      # Use sendCustomMessage instead of sendMessage, because the handler in
      # shiny-showcase.js only has access to public API of the Shiny object.
      if (private$showcase) {
        srcref <- logEntry$srcref
        srcfile <- logEntry$srcfile
        if (!is.null(srcref) && !is.null(srcfile)) {
          # only send needed information, not all of reactlog info.
          self$sendCustomMessage("showcase-src", list(srcref = srcref, srcfile = srcfile))
        }
      }
    },
    reload = function() {
      private$sendMessage(reload = TRUE)
    },
    sendInsertUI = function(selector, multiple, where, content) {
      private$sendMessage(
        `shiny-insert-ui` = list(
          selector = selector,
          multiple = multiple,
          where = where,
          content = content
        )
      )
    },
    sendRemoveUI = function(selector, multiple) {
      private$sendMessage(
        `shiny-remove-ui` = list(
          selector = selector,
          multiple = multiple
        )
      )
    },
    sendInsertTab = function(inputId, liTag, divTag, menuName,
                             target, position, select) {
      private$sendMessage(
        `shiny-insert-tab` = list(
          inputId = inputId,
          liTag = liTag,
          divTag = divTag,
          menuName = menuName,
          target = target,
          position = position,
          select = select
        )
      )
    },
    sendRemoveTab = function(inputId, target) {
      private$sendMessage(
        `shiny-remove-tab` = list(
          inputId = inputId,
          target = target
        )
      )
    },
    sendChangeTabVisibility = function(inputId, target, type) {
      private$sendMessage(
        `shiny-change-tab-visibility` = list(
          inputId = inputId,
          target = target,
          type = type
        )
      )
    },
    updateQueryString = function(queryString, mode) {
      private$sendMessage(updateQueryString = list(
        queryString = queryString, mode = mode))
    },
    resetBrush = function(brushId) {
      private$sendMessage(
        resetBrush = list(
          brushId = brushId
        )
      )
    },

    `@uploadInit` = function(fileInfos) {
      maxSize <- getOption('shiny.maxRequestSize', 5 * 1024 * 1024)
      fileInfos <- lapply(fileInfos, function(fi) {
        if (is.null(fi$type))
          fi$type <- getContentType(fi$name)
        fi
      })
      sizes <- sapply(fileInfos, function(fi){ fi$size })
      if (maxSize > 0 && any(sizes > maxSize)) {
        stop("Maximum upload size exceeded")
      }

      jobId <- private$fileUploadContext$createUploadOperation(fileInfos)
      return(list(jobId=jobId,
                  uploadUrl=paste('session', self$token, 'upload',
                                  paste(jobId, "?w=", workerId(), sep=""),
                                  sep='/')))
    },
    `@uploadEnd` = function(jobId, inputId) {
      fileData <- private$fileUploadContext$getUploadOperation(jobId)$finish()
      private$.input$set(inputId, fileData)

      setSerializer(inputId, serializerFileInput)
      snapshotPreprocessInput(inputId, snapshotPreprocessorFileInput)

      invisible()
    },
    # Provides a mechanism for handling direct HTTP requests that are posted
    # to the session (rather than going through the websocket)
    handleRequest = function(req) {
      # TODO: Turn off caching for the response
      subpath <- req$PATH_INFO

      matches <- regmatches(subpath,
                            regexec("^/([a-z]+)/([^?]*)",
                                    subpath,
                                    ignore.case=TRUE))[[1]]
      if (length(matches) == 0)
        return(httpResponse(400, 'text/html', '<h1>Bad Request</h1>'))

      if (matches[2] == 'file') {
        savedFile <- self$files$get(URLdecode(matches[3]))
        if (is.null(savedFile))
          return(httpResponse(404, 'text/html', '<h1>Not Found</h1>'))

        return(httpResponse(200, savedFile$contentType, savedFile$data))
      }

      if (matches[2] == 'upload' && identical(req$REQUEST_METHOD, "POST")) {
        job <- private$fileUploadContext$getUploadOperation(matches[3])
        if (!is.null(job)) {
          fileName <- req$HTTP_SHINY_FILE_NAME
          fileType <- req$HTTP_SHINY_FILE_TYPE
          fileSize <- req$CONTENT_LENGTH
          job$fileBegin()

          reqInput <- req$rook.input
          while (length(buf <- reqInput$read(2^16)) > 0)
            job$fileChunk(buf)

          job$fileEnd()

          return(httpResponse(200, 'text/plain', 'OK'))
        }
      }


      if (matches[2] == 'download') {

        # A bunch of ugliness here. Filenames can be dynamically generated by
        # the user code, so we don't know what they'll be in advance. But the
        # most reliable way to use non-ASCII filenames for downloads is to
        # put the actual filename in the URL. So we will start with URLs in
        # the form:
        #
        #   /session/$TOKEN/download/$NAME
        #
        # When a request matching that pattern is received, we will calculate
        # the filename and see if it's non-ASCII; if so, we'll redirect to
        #
        #   /session/$TOKEN/download/$NAME/$FILENAME
        #
        # And when that pattern is received, we will actually return the file.
        # Note that this means the filename and contents could be determined
        # a few moments apart from each other (an HTTP roundtrip basically),
        # hopefully that won't be enough to matter for anyone.

        dlmatches <- regmatches(matches[3],
                                regexec("^([^/]+)(/[^/]+)?$",
                                        matches[3]))[[1]]
        dlname <- URLdecode(dlmatches[2])
        download <- self$downloads$get(dlname)
        if (is.null(download))
          return(httpResponse(404, 'text/html', '<h1>Not Found</h1>'))

        filename <- ifelse(is.function(download$filename),
          Context$new(getDefaultReactiveDomain(), '[download]')$run(
            download$filename
          ),
          download$filename)

        # If the URL does not contain the filename, and the desired filename
        # contains non-ASCII characters, then do a redirect with the desired
        # name tacked on the end.
        if (dlmatches[3] == '' && grepl('[^ -~]', filename)) {

          return(httpResponse(302, 'text/html', '<h1>Found</h1>', c(
            'Location' = sprintf('%s/%s',
                                 URLencode(dlname, TRUE),
                                 URLencode(filename, TRUE)),
            'Cache-Control' = 'no-cache')))
        }

        # Make temp file with the same extension as the user-visible filename.
        # If the extension is not used, some functions such as pdf() and zip()
        # may append the extension they expect, meaning the data we want will
        # be written to a file other than our temp file (e.g. file1231.zip
        # instead of file1231.zip).
        ext <- tools::file_ext(filename)
        if (nzchar(ext))
          ext <- paste(".", ext, sep = "")
        tmpdata <- tempfile(fileext = ext)
        return(Context$new(getDefaultReactiveDomain(), '[download]')$run(function() {
          promises::with_promise_domain(reactivePromiseDomain(), {
            promises::with_promise_domain(createStackTracePromiseDomain(), {
              self$incrementBusyCount()
              hybrid_chain(
                # ..stacktraceon matches with the top-level ..stacktraceoff..
                try(..stacktraceon..(download$func(tmpdata)), silent = TRUE),
                function(result) {
                  if (inherits(result, 'try-error')) {
                    unlink(tmpdata)
                    stop(attr(result, "condition", exact = TRUE))
                  }
                  if (!file.exists(tmpdata)) {
                    # If no file was created, return a 404
                    return(httpResponse(404, content = "404 Not found"))
                  }
                  return(httpResponse(
                    200,
                    download$contentType %||% getContentType(filename),
                    # owned=TRUE means tmpdata will be deleted after response completes
                    list(file=tmpdata, owned=TRUE),
                    c(
                      'Content-Disposition' = ifelse(
                        dlmatches[3] == '',
                        paste0(
                          'attachment; filename="',
                          gsub('(["\\\\])', '\\\\\\1', filename),
                          '"'
                        ),
                        'attachment'
                      ),
                      'Cache-Control'='no-cache')))
                },
                finally = function() {
                  self$decrementBusyCount()
                }
              )
            })
          })
        }))
      }

      if (matches[2] == 'dataobj') {
        # /session/$TOKEN/dataobj/$NAME
        dlmatches <- regmatches(matches[3],
                                regexec("^([^/]+)(/[^/]+)?$",
                                        matches[3]))[[1]]
        dlname <- URLdecode(dlmatches[2])
        download <- self$downloads$get(dlname)
        return(download$filter(download$data, req))
      }

      return(httpResponse(404, 'text/html', '<h1>Not Found</h1>'))
    },
    # Send a file to the client
    fileUrl = function(name, file, contentType='application/octet-stream') {
      "Return a URL for a file to be sent to the client. The file will be base64
      encoded and embedded in the URL."
      bytes <- file.info(file)$size
      if (is.na(bytes))
        return(NULL)

      fileData <- readBin(file, 'raw', n=bytes)

      b64 <- rawToBase64(fileData)
      return(paste('data:', contentType, ';base64,', b64, sep=''))
    },
    registerDownload = function(name, filename, contentType, func) {

      self$downloads$set(name, list(filename = filename,
                               contentType = contentType,
                               func = func))
      return(sprintf('session/%s/download/%s?w=%s',
                     URLencode(self$token, TRUE),
                     URLencode(name, TRUE),
                     workerId()))
    },
    # register a data object on the server side (for datatable or selectize, etc)
    registerDataObj = function(name, data, filterFunc) {
      # abusing downloads at the moment
      self$downloads$set(name, list(data = data, filter = filterFunc))
      return(sprintf('session/%s/dataobj/%s?w=%s&nonce=%s',
                     URLencode(self$token, TRUE),
                     URLencode(name, TRUE),
                     workerId(),
                     URLencode(createUniqueId(8), TRUE)))
    },
    # This function suspends observers for hidden outputs and resumes observers
    # for un-hidden outputs.
    manageHiddenOutputs = function(outputsToCheck = NULL) {
      if (is.null(outputsToCheck)) {
        outputsToCheck <- names(private$.outputs)
      }

      # Find hidden state for each output, and suspend/resume accordingly
      for (outputName in outputsToCheck) {
        if (private$shouldSuspend(outputName)) {
          private$.outputs[[outputName]]$suspend()
        } else {
          private$.outputs[[outputName]]$resume()
        }
      }
    },
    # Set the normal and client data input variables. Normally, managing
    # inputs doesn't take immediate effect when there are observers that
    # are pending execution or currently executing (including having
    # started async operations that have yielded control, but not yet
    # completed). The `now` argument can force this. It should generally
    # not be used, but we're adding it to get around a show-stopping bug
    # for Shiny v1.1 (see the call site for more details).
    manageInputs = function(data, now = FALSE) {
      force(data)
      doManageInputs <- function() {
        private$inputReceivedCallbacks$invoke(data)

        data_names <- names(data)

        # Separate normal input variables from client data input variables
        clientdata_idx <- grepl("^.clientdata_", data_names)

        # Set normal (non-clientData) input values
        private$.input$mset(data[data_names[!clientdata_idx]])

        # Strip off .clientdata_ from clientdata input names, and set values
        input_clientdata <- data[data_names[clientdata_idx]]
        names(input_clientdata) <- sub("^.clientdata_", "",
          names(input_clientdata))
        private$.clientData$mset(input_clientdata)

        self$manageHiddenOutputs()
      }
      if (isTRUE(now)) {
        doManageInputs()
      } else {
        self$cycleStartAction(doManageInputs)
      }
    },
    outputOptions = function(name, ...) {
      # If no name supplied, return the list of options for all outputs
      if (is.null(name))
        return(private$.outputOptions)
      if (! name %in% names(private$.outputs))
        stop(name, " is not in list of output objects")

      opts <- list(...)
      # If no options are set, return the options for the specified output
      if (length(opts) == 0)
        return(private$.outputOptions[[name]])

      # Set the appropriate option
      validOpts <- c("suspendWhenHidden", "priority")
      for (optname in names(opts)) {
        if (! optname %in% validOpts)
          stop(optname, " is not a valid option")

        private$.outputOptions[[name]][[optname]] <- opts[[optname]]
      }

      # If any changes to suspendWhenHidden, need to re-run manageHiddenOutputs
      if ("suspendWhenHidden" %in% names(opts)) {
        self$manageHiddenOutputs(name)
      }

      if ("priority" %in% names(opts)) {
        private$.outputs[[name]]$setPriority(opts[['priority']])
      }

      invisible()
    },
    incrementBusyCount = function() {
      if (private$busyCount == 0L) {
        rLog$asyncStart(domain = self)
        private$sendMessage(busy = "busy")
      }
      private$busyCount <- private$busyCount + 1L
    },
    decrementBusyCount = function() {
      private$busyCount <- private$busyCount - 1L
      if (private$busyCount == 0L) {
        rLog$asyncStop(domain = self)
        private$sendMessage(busy = "idle")
        self$requestFlush()
        # We defer the call to startCycle() using later(), to defend against
        # cycles where we continually call startCycle which causes an observer
        # to fire which calls startCycle which causes an observer to fire...
        #
        # It's OK for these cycles to occur, but we must return control to the
        # event loop between iterations (or at least sometimes) in order to not
        # make the whole Shiny app go unresponsive.
        later::later(function() {
          if (private$busyCount == 0L) {
            private$startCycle()
          }
        })
      }
    }
  )
)

.createOutputWriter <- function(shinysession, ns = identity) {
  structure(list(impl=shinysession, ns=ns), class='shinyoutput')
}

#' @export
`$<-.shinyoutput` <- function(x, name, value) {
  name <- .subset2(x, 'ns')(name)

  label <- deparse(substitute(value))
  if (length(substitute(value)) > 1) {
    # value is an object consisting of a call and its arguments. Here we want
    # to find the source references for the first argument (if there are
    # arguments), which generally corresponds to the reactive expression--
    # e.g. in renderTable({ x }), { x } is the expression to trace.
    attr(label, "srcref") <- srcrefFromShinyCall(substitute(value)[[2]])
    srcref <- attr(substitute(value)[[2]], "srcref")
    if (length(srcref) > 0)
      attr(label, "srcfile") <- srcFileOfRef(srcref[[1]])
  }
  .subset2(x, 'impl')$defineOutput(name, value, label)
  return(invisible(x))
}

#' @export
`[[<-.shinyoutput` <- `$<-.shinyoutput`

#' @export
`$.shinyoutput` <- function(x, name) {
  name <- .subset2(x, 'ns')(name)

  if (getOption("shiny.allowoutputreads", FALSE)) {
    .subset2(x, 'impl')$getOutput(name)
  } else {
    rlang::abort(paste0("Can't read output '", name, "'"))
  }
}

#' @export
`[[.shinyoutput` <- `$.shinyoutput`

#' @export
`[.shinyoutput` <- function(values, name) {
  rlang::abort("Can't index shinyoutput with `[`.")
}

#' @export
`[<-.shinyoutput` <- function(values, name, value) {
  rlang::abort("Can't index shinyoutput with `[[`.")
}

#' Set options for an output object.
#'
#' These are the available options for an output object:
#' \itemize{
#'   \item suspendWhenHidden. When `TRUE` (the default), the output object
#'     will be suspended (not execute) when it is hidden on the web page. When
#'     `FALSE`, the output object will not suspend when hidden, and if it
#'     was already hidden and suspended, then it will resume immediately.
#'   \item priority. The priority level of the output object. Queued outputs
#'     with higher priority values will execute before those with lower values.
#' }
#'
#' @examples
#' \dontrun{
#' # Get the list of options for all observers within output
#' outputOptions(output)
#'
#' # Disable suspend for output$myplot
#' outputOptions(output, "myplot", suspendWhenHidden = FALSE)
#'
#' # Change priority for output$myplot
#' outputOptions(output, "myplot", priority = 10)
#'
#' # Get the list of options for output$myplot
#' outputOptions(output, "myplot")
#' }
#'
#' @param x A shinyoutput object (typically `output`).
#' @param name The name of an output observer in the shinyoutput object.
#' @param ... Options to set for the output observer.
#' @export
outputOptions <- function(x, name, ...) {
  if (!inherits(x, "shinyoutput")) {
    stop("x must be a shinyoutput object.")
  }

  if (!missing(name)) {
    name <- .subset2(x, 'ns')(name)
  } else {
    name <- NULL
  }

  .subset2(x, 'impl')$outputOptions(name, ...)
}


#' Get output information
#'
#' Returns information about the currently executing output, including its `name` (i.e., `outputId`);
#' and in some cases, relevant sizing and styling information.
#'
#' @param session The current Shiny session.
#'
#' @return `NULL` if called outside of an output context; otherwise,
#'   a list which includes:
#'   * The `name` of the output (reported for any output).
#'   * If the output is a `plotOutput()` or `imageOutput()`, then:
#'     * `height`: a reactive expression which returns the height in pixels.
#'     * `width`: a reactive expression which returns the width in pixels.
#'  * If the output is a `plotOutput()`, `imageOutput()`, or contains a `shiny-report-theme` class, then:
#'     * `bg`: a reactive expression which returns the background color.
#'     * `fg`: a reactive expression which returns the foreground color.
#'     * `accent`: a reactive expression which returns the hyperlink color.
#'     * `font`: a reactive expression which returns a list of font information, including:
#'       * `families`: a character vector containing the CSS `font-family` property.
#'       * `size`: a character string containing the CSS `font-size` property
#'
#' @export
#' @examples
#'
#' if (interactive()) {
#'   shinyApp(
#'     fluidPage(
#'       tags$style(HTML("body {background-color: black; color: white; }")),
#'       tags$style(HTML("body a {color: purple}")),
#'       tags$style(HTML("#info {background-color: teal; color: orange; }")),
#'       plotOutput("p"),
#'       "Computed CSS styles for the output named info:",
#'       tagAppendAttributes(
#'         textOutput("info"),
#'         class = "shiny-report-theme"
#'       )
#'     ),
#'     function(input, output) {
#'       output$p <- renderPlot({
#'         info <- getCurrentOutputInfo()
#'         par(bg = info$bg(), fg = info$fg(), col.axis = info$fg(), col.main = info$fg())
#'         plot(1:10, col = info$accent(), pch = 19)
#'         title("A simple R plot that uses its CSS styling")
#'       })
#'       output$info <- renderText({
#'         info <- getCurrentOutputInfo()
#'         jsonlite::toJSON(
#'           list(
#'             bg = info$bg(),
#'             fg = info$fg(),
#'             accent = info$accent(),
#'             font = info$font()
#'           ),
#'           auto_unbox = TRUE
#'         )
#'       })
#'     }
#'   )
#' }
#'
#'
getCurrentOutputInfo <- function(session = getDefaultReactiveDomain()) {
  if (is.null(session)) return(NULL)
  session$getCurrentOutputInfo()
}

#' Add callbacks for Shiny session events
#'
#' @description
#' These functions are for registering callbacks on Shiny session events.
#' `onFlush` registers a function that will be called before Shiny flushes the
#' reactive system. `onFlushed` registers a function that will be called after
#' Shiny flushes the reactive system. `onUnhandledError` registers a function to
#' be called when an unhandled error occurs before the session is closed.
#' `onSessionEnded` registers a function to be called after the client has
#' disconnected.
#'
#' These functions should be called within the application's server function.
#'
#' All of these functions return a function which can be called with no
#' arguments to cancel the registration.
#'
#' @section Unhandled Errors:
#' Unhandled errors are errors that aren't otherwise handled by Shiny or by the
#' application logic. In other words, they are errors that will either cause the
#' application to crash or will result in "Error" output in the UI.
#'
#' You can use `onUnhandledError` to register a function that will be called
#' when an unhandled error occurs. This function will be called with the error
#' object as its first argument. If the error is fatal and will result in the
#' session closing, the error condition will have the `shiny.error.fatal` class.
#'
#' Note that the `onUnhandledError` callbacks don't allow you to prevent the app
#' from closing or to modify the error condition. Instead, they are intended to
#' give you an opportunity to log the error or perform other cleanup operations.
#'
#' @param fun A callback function.
#' @param once Should the function be run once, and then cleared, or should it
#'   re-run each time the event occurs. (Only for `onFlush` and
#'   `onFlushed`.)
#' @param session A shiny session object.
#'
#' @examplesIf interactive()
#' library(shiny)
#'
#' ui <- fixedPage(
#'   markdown(c(
#'     "Set the number to 8 or higher to cause an error",
#'     "in the `renderText()` output."
#'   )),
#'   sliderInput("number", "Number", 0, 10, 4),
#'   textOutput("text"),
#'   hr(),
#'   markdown(c(
#'     "Click the button below to crash the app with an unhandled error",
#'     "in an `observe()` block."
#'   )),
#'   actionButton("crash", "Crash the app!")
#' )
#'
#' log_event <- function(level, ...) {
#'   ts <- strftime(Sys.time(), " [%F %T] ")
#'   message(level, ts, ...)
#' }
#'
#' server <- function(input, output, session) {
#'   log_event("INFO", "Session started")
#'
#'   onUnhandledError(function(err) {
#'     # log the unhandled error
#'     level <- if (inherits(err, "shiny.error.fatal")) "FATAL" else "ERROR"
#'     log_event(level, conditionMessage(err))
#'   })
#'
#'   onStop(function() {
#'     log_event("INFO", "Session ended")
#'   })
#'
#'   observeEvent(input$crash, stop("Oops, an unhandled error happened!"))
#'
#'   output$text <- renderText({
#'     if (input$number > 7) {
#'       stop("that's too high!")
#'     }
#'     sprintf("You picked number %d.", input$number)
#'   })
#' }
#'
#' shinyApp(ui, server)
#'
#' @export
onFlush <- function(fun, once = TRUE, session = getDefaultReactiveDomain()) {
  session$onFlush(fun, once = once)
}

#' @rdname onFlush
#' @export
onFlushed <- function(fun, once = TRUE, session = getDefaultReactiveDomain()) {
  session$onFlushed(fun, once = once)
}

#' @rdname onFlush
#'
#' @seealso [onStop()] for registering callbacks that will be
#'   invoked when the application exits, or when a session ends.
#' @export
onSessionEnded <- function(fun, session = getDefaultReactiveDomain()) {
  session$onSessionEnded(fun)
}

.globals$onUnhandledErrorCallbacks <- NULL
on_load({
  .globals$onUnhandledErrorCallbacks <- Callbacks$new()
})

#' @rdname onFlush
#' @export
onUnhandledError <- function(fun, session = getDefaultReactiveDomain()) {
  if (!is.function(fun) || length(formals(fun)) == 0) {
    rlang::abort(
      "The unhandled error callback must be a function that takes an error object as its first argument."
    )
  }

  if (is.null(session)) {
    .globals$onUnhandledErrorCallbacks$register(fun)
  } else {
    session$onUnhandledError(fun)
  }
}


flushPendingSessions <- function() {
  lapply(appsNeedingFlush$values(), function(shinysession) {
    tryCatch(
      shinysession$flushOutput(),

      stop = function(e) {
        # If there are any uncaught errors that bubbled up to here, close the
        # session.
        shinysession$close()
      }
    )
    NULL
  })
}

.globals$onStopCallbacks <- Callbacks$new()

#' Run code after an application or session ends
#'
#' This function registers callback functions that are invoked when the
#' application exits (when [runApp()] exits), or after each user
#' session ends (when a client disconnects).
#'
#' @param fun A function that will be called after the app has finished running.
#' @param session A scope for when the callback will run. If `onStop` is
#'   called from within the server function, this will default to the current
#'   session, and the callback will be invoked when the current session ends. If
#'   `onStop` is called outside a server function, then the callback will
#'   be invoked with the application exits. If `NULL`, it is the same as
#'   calling `onStop` outside of the server function, and the callback will
#'   be invoked when the application exits.
#'
#'
#' @seealso [onSessionEnded()] for the same functionality, but at
#'   the session level only.
#'
#' @return A function which, if invoked, will cancel the callback.
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'   # Open this application in multiple browsers, then close the browsers.
#'   shinyApp(
#'     ui = basicPage("onStop demo"),
#'
#'     server = function(input, output, session) {
#'       onStop(function() cat("Session stopped\n"))
#'     },
#'
#'     onStart = function() {
#'       cat("Doing application setup\n")
#'
#'       onStop(function() {
#'         cat("Doing application cleanup\n")
#'       })
#'     }
#'   )
#' }
#' # In the example above, onStop() is called inside of onStart(). This is
#' # the pattern that should be used when creating a shinyApp() object from
#' # a function, or at the console. If instead you are writing an app.R which
#' # will be invoked with runApp(), you can do it that way, or put the onStop()
#' # before the shinyApp() call, as shown below.
#'
#' \dontrun{
#' # ==== app.R ====
#' cat("Doing application setup\n")
#' onStop(function() {
#'   cat("Doing application cleanup\n")
#' })
#'
#' shinyApp(
#'   ui = basicPage("onStop demo"),
#'
#'   server = function(input, output, session) {
#'     onStop(function() cat("Session stopped\n"))
#'   }
#' )
#' # ==== end app.R ====
#'
#'
#' # Similarly, if you have a global.R, you can call onStop() from there.
#' # ==== global.R ====
#' cat("Doing application setup\n")
#' onStop(function() {
#'   cat("Doing application cleanup\n")
#' })
#' # ==== end global.R ====
#' }
#' @export
onStop <- function(fun, session = getDefaultReactiveDomain()) {
  if (is.null(session)) {
    return(.globals$onStopCallbacks$register(fun))
  } else {
    # Note: In the future if we allow scoping the onStop() callback to modules
    # and allow modules to be stopped, then session_proxy objects will need
    # its own implementation of $onSessionEnded.
    return(session$onSessionEnded(fun))
  }
}

# Helper class for emitting log messages to stdout that will be interpreted by
# a Shiny Server parent process. The duration it's trying to record is the time
# between a websocket message being received, and the next flush to the client.
ShinyServerTimingRecorder <- R6Class("ShinyServerTimingRecorder",
  cloneable = FALSE,
  public = list(
    initialize = function() {
      private$shiny_stdout <- if (exists(".shiny__stdout", globalenv()))
        get(".shiny__stdout", globalenv())
      else
        NULL
      private$guid <- NULL
    },
    start = function(guid) {
      if (is.null(private$shiny_stdout)) return()

      private$guid <- guid
      if (!is.null(guid)) {
        private$write("n")
      }
    },
    stop = function() {
      if (is.null(private$shiny_stdout)) return()

      if (!is.null(private$guid)) {
        private$write("x")
        private$guid <- NULL
      }
    }
  ),
  private = list(
    shiny_stdout = NULL,
    guid = character(),
    write = function(code) {
      # eNter or eXit a flushReact
      writeLines(paste("_", code, "_flushReact ", private$guid,
        " @ ", sprintf("%.3f", as.numeric(Sys.time())),
        sep=""), con=private$shiny_stdout)
      flush(private$shiny_stdout)
    }
  )
)

missingOutput <- function(...) req(FALSE)

#' Insert inline Markdown
#'
#' This function accepts
#' [Markdown](https://en.wikipedia.org/wiki/Markdown)-syntax text and returns
#' HTML that may be included in Shiny UIs.
#'
#' Leading whitespace is trimmed from Markdown text with [glue::trim()].
#' Whitespace trimming ensures Markdown is processed correctly even when the
#' call to `markdown()` is indented within surrounding R code.
#'
#' By default, [Github extensions][commonmark::extensions] are enabled, but this
#' can be disabled by passing `extensions = FALSE`.
#'
#' Markdown rendering is performed by [commonmark::markdown_html()]. Additional
#' arguments to `markdown()` are passed as arguments to `markdown_html()`
#'
#' @param mds A character vector of Markdown source to convert to HTML. If the
#'   vector has more than one element, a single-element character vector of
#'   concatenated HTML is returned.
#' @param extensions Enable Github syntax extensions; defaults to `TRUE`.
#' @param .noWS Character vector used to omit some of the whitespace that would
#'   normally be written around generated HTML. Valid options include `before`,
#'   `after`, and `outside` (equivalent to `before` and `end`).
#' @param ... Additional arguments to pass to [commonmark::markdown_html()].
#'   These arguments are _[dynamic][rlang::dyn-dots]_.
#'
#' @return a character vector marked as HTML.
#' @export
#' @examples
#' ui <- fluidPage(
#'   markdown("
#'     # Markdown Example
#'
#'     This is a markdown paragraph, and will be contained within a `<p>` tag
#'     in the UI.
#'
#'     The following is an unordered list, which will be represented in the UI as
#'     a `<ul>` with `<li>` children:
#'
#'     * a bullet
#'     * another
#'
#'     [Links](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a) work;
#'     so does *emphasis*.
#'
#'     To see more of what's possible, check out [commonmark.org/help](https://commonmark.org/help).
#'     ")
#' )
markdown <- function(mds, extensions = TRUE, .noWS = NULL, ...) {
  html <- rlang::exec(commonmark::markdown_html, glue::trim(mds), extensions = extensions, ...)
  htmltools::HTML(html, .noWS = .noWS)
}


# Check that an object is a ShinySession object, and give an informative error.
# The default label is the caller function's name.
validate_session_object <- function(session, label = as.character(sys.call(sys.parent())[[1]])) {
  if (missing(session) ||
      !inherits(session, c("ShinySession", "MockShinySession", "session_proxy")))
  {
    stop(call. = FALSE,
      sprintf(
        "`session` must be a 'ShinySession' object. Did you forget to pass `session` to `%s()`?",
        label
      )
    )
  }
}
