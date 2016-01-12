#' @include utils.R stack.R
NULL

#' Web Application Framework for R
#'
#' Shiny makes it incredibly easy to build interactive web applications with R.
#' Automatic "reactive" binding between inputs and outputs and extensive
#' pre-built widgets make it possible to build beautiful, responsive, and
#' powerful applications with minimal effort.
#'
#' The Shiny tutorial at \url{http://shiny.rstudio.com/tutorial/} explains
#' the framework in depth, walks you through building a simple application, and
#' includes extensive annotated examples.
#'
#' @seealso \link{shiny-options} for documentation about global options.
#'
#' @name shiny-package
#' @aliases shiny
#' @docType package
#' @import htmltools httpuv xtable digest R6 mime
NULL

# It's necessary to Depend on methods so Rscript doesn't fail. It's necessary
# to import(methods) in NAMESPACE so R CMD check doesn't complain. This
# approach isn't foolproof because Rscript -e pkgname::func() doesn't actually
# cause methods to be attached, but it's not a problem for shiny::runApp()
# since we call require(shiny) as part of loading the app.
#' @import methods
NULL


#' Global options for Shiny
#'
#' There are a number of global options that affect Shiny's behavior. These can
#' be set with (for example) \code{options(shiny.trace=TRUE)}.
#'
#' \describe{
#'   \item{shiny.launch.browser}{A boolean which controls the default behavior
#'     when an app is run. See \code{\link{runApp}} for more information.}
#'   \item{shiny.port}{A port number that Shiny will listen on. See
#'     \code{\link{runApp}} for more information.}
#'   \item{shiny.trace}{If \code{TRUE}, all of the messages sent between the R
#'     server and the web browser client will be printed on the console. This
#'     is useful for debugging.}
#'   \item{shiny.autoreload}{If \code{TRUE} when a Shiny app is launched, the
#'     app directory will be continually monitored for changes to files that
#'     have the extensions: r, htm, html, js, css, png, jpg, jpeg, gif. If any
#'     changes are detected, all connected Shiny sessions are reloaded. This
#'     allows for fast feedback loops when tweaking Shiny UI.
#'
#'     Since monitoring for changes is expensive (we simply poll for last
#'     modified times), this feature is intended only for development.
#'
#'     You can customize the file patterns Shiny will monitor by setting the
#'     shiny.autoreload.pattern option. For example, to monitor only ui.R:
#'     \code{option(shiny.autoreload.pattern = glob2rx("ui.R"))}
#'
#'     The default polling interval is 500 milliseconds. You can change this
#'     by setting e.g. \code{option(shiny.autoreload.interval = 2000)} (every
#'     two seconds).}
#'   \item{shiny.reactlog}{If \code{TRUE}, enable logging of reactive events,
#'     which can be viewed later with the \code{\link{showReactLog}} function.
#'     This incurs a substantial performance penalty and should not be used in
#'     production.}
#'   \item{shiny.usecairo}{This is used to disable graphical rendering by the
#'     Cairo package, if it is installed. See \code{\link{plotPNG}} for more
#'     information.}
#'   \item{shiny.maxRequestSize}{This is a number which specifies the maximum
#'     web request size, which serves as a size limit for file uploads. If
#'     unset, the maximum request size defaults to 5MB.}
#'   \item{shiny.suppressMissingContextError}{Normally, invoking a reactive
#'     outside of a reactive context (or \code{\link{isolate}()}) results in
#'     an error. If this is \code{TRUE}, don't error in these cases. This
#'     should only be used for debugging or demonstrations of reactivity at the
#'     console.}
#'   \item{shiny.host}{The IP address that Shiny should listen on. See
#'     \code{\link{runApp}} for more information.}
#'   \item{shiny.json.digits}{The number of digits to use when converting
#'     numbers to JSON format to send to the client web browser.}
#'   \item{shiny.minified}{If this is \code{TRUE} or unset (the default), then
#'     Shiny will use minified JavaScript (\code{shiny.min.js}). If
#'     \code{FALSE}, then Shiny will use the un-minified JavaScript
#'     (\code{shiny.js}); this can be useful during development.}
#'   \item{shiny.error}{This can be a function which is called when an error
#'     occurs. For example, \code{options(shiny.error=recover)} will result a
#'     the debugger prompt when an error occurs.}
#'   \item{shiny.table.class}{CSS class names to use for tables.}
#'   \item{shiny.deprecation.messages}{This controls whether messages for
#'     deprecated functions in Shiny will be printed. See
#'     \code{\link{shinyDeprecated}} for more information.}
#'   \item{shiny.fullstacktrace}{Controls whether "pretty" or full stack traces
#'     are dumped to the console when errors occur during Shiny app execution.
#'     The default is \code{FALSE} (pretty stack traces).}
#'   \item{shiny.stacktraceoffset}{If \code{TRUE}, then Shiny's printed stack
#'     traces will display srcrefs one line above their usual location. This is
#'     an arguably more intuitive arrangement for casual R users, as the name
#'     of a function appears next to the srcref where it is defined, rather than
#'     where it is currently being called from.}
#' }
#' @name shiny-options
NULL
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
  auto_unbox = TRUE, digits = getOption("shiny.json.digits", 16),
  use_signif = TRUE, force = TRUE, POSIXt = "ISO8601", UTC = TRUE,
  rownames = FALSE, keep_vec_names = TRUE) {

  # I(x) is so that length-1 atomic vectors get put in [].
  jsonlite::toJSON(I(x), dataframe = dataframe, null = null, na = na,
   auto_unbox = auto_unbox, digits = digits, use_signif = use_signif,
   force = force, POSIXt = POSIXt, UTC = UTC, rownames = rownames,
   keep_vec_names = keep_vec_names, json_verbatim = TRUE, ...)
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
#' Shiny server functions can optionally include \code{session} as a parameter
#' (e.g. \code{function(input, output, session)}). The session object is an
#' environment that can be used to access information and functionality
#' relating to the session. The following list describes the items available
#' in the environment; they can be accessed using the \code{$} operator (for
#' example, \code{session$clientData$url_search}).
#'
#' @return
#' \item{clientData}{
#'   A \code{\link{reactiveValues}} object that contains information about the client.
#'   \itemize{
#'     \item{\code{allowDataUriScheme} is a logical value that indicates whether
#'       the browser is able to handle URIs that use the \code{data:} scheme.
#'     }
#'     \item{\code{pixelratio} reports the "device pixel ratio" from the web browser,
#'       or 1 if none is reported. The value is 2 for Apple Retina displays.
#'     }
#'     \item{\code{singletons} - for internal use}
#'     \item{\code{url_protocol}, \code{url_hostname}, \code{url_port},
#'       \code{url_pathname}, \code{url_search}, and \code{url_hash_initial}
#'       can be used to get the components of the URL that was requested by the
#'       browser to load the Shiny app page. These values are from the
#'       browser's perspective, so neither HTTP proxies nor Shiny Server will
#'       affect these values. The \code{url_search} value may be used with
#'       \code{\link{parseQueryString}} to access query string parameters.
#'     }
#'   }
#'   \code{clientData} also contains information about each output.
#'   \code{output_\var{outputId}_width} and \code{output_\var{outputId}_height}
#'   give the dimensions (using \code{offsetWidth} and \code{offsetHeight}) of
#'   the DOM element that is bound to \code{\var{outputId}}, and
#'   \code{output_\var{outputId}_hidden} is a logical that indicates whether
#'   the element is hidden. These values may be \code{NULL} if the output is
#'   not bound.
#' }
#' \item{input}{
#'   The session's \code{input} object (the same as is passed into the Shiny
#'   server function as an argument).
#' }
#' \item{isClosed()}{A function that returns \code{TRUE} if the client has
#'   disconnected.
#' }
#' \item{onEnded(callback)}{
#'   Synonym for \code{onSessionEnded}.
#' }
#' \item{onFlush(func, once=TRUE)}{
#'   Registers a function to be called before the next time (if \code{once=TRUE})
#'   or every time (if \code{once=FALSE}) Shiny flushes the reactive system.
#'   Returns a function that can be called with no arguments to cancel the
#'   registration.
#' }
#' \item{onFlushed(func, once=TRUE)}{
#'   Registers a function to be called after the next time (if \code{once=TRUE})
#'   or every time (if \code{once=FALSE}) Shiny flushes the reactive system.
#'   Returns a function that can be called with no arguments to cancel the
#'   registration.
#' }
#' \item{onSessionEnded(callback)}{
#'   Registers a function to be called after the client has disconnected.
#'   Returns a function that can be called with no arguments to cancel the
#'   registration.
#' }
#' \item{output}{
#'   The session's \code{output} object (the same as is passed into the Shiny
#'   server function as an argument).
#' }
#' \item{reactlog}{
#'   For internal use.
#' }
#' \item{registerDataObj(name, data, filterFunc)}{
#'   Publishes any R object as a URL endpoint that is unique to this session.
#'   \code{name} must be a single element character vector; it will be used
#'   to form part of the URL. \code{filterFunc} must be a function that takes
#'   two arguments: \code{data} (the value that was passed into
#'   \code{registerDataObj}) and \code{req} (an environment that implements
#'   the Rook specification for HTTP requests). \code{filterFunc} will be
#'   called with these values whenever an HTTP request is made to the URL
#'   endpoint. The return value of \code{filterFunc} should be a Rook-style
#'   response.
#' }
#' \item{reload()}{
#'   The equivalent of hitting the browser's Reload button. Only works if the
#'   session is actually connected.
#' }
#' \item{request}{
#'   An environment that implements the Rook specification for HTTP requests.
#'   This is the request that was used to initiate the websocket connection
#'   (as opposed to the request that downloaded the web page for the app).
#' }
#' \item{sendCustomMessage(type, message)}{
#'   Sends a custom message to the web page. \code{type} must be a
#'   single-element character vector giving the type of message, while
#'   \code{message} can be any jsonlite-encodable value. Custom messages
#'   have no meaning to Shiny itself; they are used soley to convey information
#'   to custom JavaScript logic in the browser. You can do this by adding
#'   JavaScript code to the browser that calls
#'   \code{Shiny.addCustomMessageHandler(type, function(message){...})}
#'   as the page loads; the function you provide to
#'   \code{addCustomMessageHandler} will be invoked each time
#'   \code{sendCustomMessage} is called on the server.
#' }
#' \item{sendInputMessage(inputId, message)}{
#'   Sends a message to an input on the session's client web page; if the input
#'   is present and bound on the page at the time the message is received, then
#'   the input binding object's \code{receiveMessage(el, message)} method will
#'   be called. \code{sendInputMessage} should generally not be called directly
#'   from Shiny apps, but through friendlier wrapper functions like
#'   \code{\link{updateTextInput}}.
#' }
#' \item{ns(id)}{
#'   Server-side version of \code{ns <- \link{NS}(id)}. If bare IDs need to be
#'   explicitly namespaced for the current module, \code{session$ns("name")}
#'   will return the fully-qualified ID.
#' }
#'
#' @name session
NULL

#' Namespaced IDs for inputs/outputs
#'
#' The \code{NS} function creates namespaced IDs out of bare IDs, by joining
#' them using \code{ns.sep} as the delimiter. It is intended for use in Shiny
#' modules. See \url{http://shiny.rstudio.com/articles/modules.html}.
#'
#' Shiny applications use IDs to identify inputs and outputs. These IDs must be
#' unique within an application, as accidentally using the same input/output ID
#' more than once will result in unexpected behavior. The traditional solution
#' for preventing name collisions is \emph{namespaces}; a namespace is to an ID
#' as a directory is to a file. Use the \code{NS} function to turn a bare ID
#' into a namespaced one, by combining them with \code{ns.sep} in between.
#'
#' @param namespace The character vector to use for the namespace. This can have
#'   any length, though a single element is most common. Length 0 will cause the
#'   \code{id} to be returned without a namespace, and length 2 will be
#'   interpreted as multiple namespaces, in increasing order of specificity
#'   (i.e. starting with the top-level namespace).
#' @param id The id string to be namespaced (optional).
#' @return If \code{id} is missing, returns a function that expects an id string
#'   as its only argument and returns that id with the namespace prepended.
#' @seealso \url{http://shiny.rstudio.com/articles/modules.html}
#'
#' @export
NS <- function(namespace, id = NULL) {
  if (missing(id)) {
    function(id) {
      paste(c(namespace, id), collapse = ns.sep)
    }
  } else {
    paste(c(namespace, id), collapse = ns.sep)
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
    inputMessageQueue = list(), # A list of inputMessages to send when flushed
    .outputs = list(),          # Keeps track of all the output observer objects
    .outputOptions = list(),     # Options for each of the output observer objects
    progressKeys = 'character',
    showcase   = 'ANY',
    fileUploadContext = 'FileUploadContext',
    .input      = 'ANY', # Internal ReactiveValues object for normal input sent from client
    .clientData = 'ANY', # Internal ReactiveValues object for other data sent from the client
    busyCount = 0L, # Number of observer callbacks that are pending. When 0, we are idle
    closedCallbacks = 'Callbacks',
    flushCallbacks = 'Callbacks',
    flushedCallbacks = 'Callbacks',
    inputReceivedCallbacks = 'Callbacks',
    sendResponse = function(requestMsg, value) {
      if (is.null(requestMsg$tag)) {
        warning("Tried to send response for untagged message; method: ",
                requestMsg$method)
        return()
      }
      private$write(toJSON(list(response=list(tag=requestMsg$tag, value=value))))
    },
    sendErrorResponse = function(requestMsg, error) {
      if (is.null(requestMsg$tag))
        return()
      private$write(toJSON(list(response=list(tag=requestMsg$tag, error=error))))
    },
    write = function(json) {
      if (self$closed){
        return()
      }
      if (isTRUE(getOption('shiny.trace')))
        message('SEND ',
           gsub('(?m)base64,[a-zA-Z0-9+/=]+','[base64 data]',json,perl=TRUE))
      private$websocket$send(json)
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
      hidden <- private$.clientData$.values[[paste("output_", name, "_hidden",
                                           sep="")]]
      if (is.null(hidden)) hidden <- TRUE

      return(hidden && private$getOutputOption(name, 'suspendWhenHidden', TRUE))
    },

    registerSessionEndCallbacks = function() {
      # This is to be called from the initialization. It registers functions
      # that are called when a session ends.

      # Clear file upload directories, if present
      self$onSessionEnded(private$fileUploadContext$rmUploadDirs)
    }
  ),
  public = list(
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
    user = NULL,
    groups = NULL,

    initialize = function(websocket) {
      private$websocket <- websocket
      self$closed <- FALSE
      # TODO: Put file upload context in user/app-specific dir if possible

      private$invalidatedOutputValues <- Map$new()
      private$invalidatedOutputErrors <- Map$new()
      private$fileUploadContext <- FileUploadContext$new()
      private$closedCallbacks <- Callbacks$new()
      private$flushCallbacks <- Callbacks$new()
      private$flushedCallbacks <- Callbacks$new()
      private$inputReceivedCallbacks <- Callbacks$new()
      private$.input      <- ReactiveValues$new()
      private$.clientData <- ReactiveValues$new()
      self$progressStack <- Stack$new()
      self$files <- Map$new()
      self$downloads <- Map$new()

      self$input <- .createReactiveValues(private$.input, readonly=TRUE)
      .setLabel(self$input, 'input')
      self$clientData <- .createReactiveValues(private$.clientData, readonly=TRUE)
      .setLabel(self$clientData, 'clientData')

      self$output <- .createOutputWriter(self)

      self$token <- createUniqueId(16)
      private$.outputs <- list()
      private$.outputOptions <- list()

      private$registerSessionEndCallbacks()

      if (!is.null(websocket$request$HTTP_SHINY_SERVER_CREDENTIALS)) {
        try({
          creds <- jsonlite::fromJSON(websocket$request$HTTP_SHINY_SERVER_CREDENTIALS)
          self$user <- creds$user
          self$groups <- creds$groups
        }, silent=FALSE)
      }

      # session$request should throw an error if httpuv doesn't have
      # websocket$request, but don't throw it until a caller actually
      # tries to access session$request
      delayedAssign('request', websocket$request, assign.env = self)

      private$write(toJSON(list(config = list(
        workerId = workerId(),
        sessionId = self$token
      ))))
    },
    makeScope = function(namespace) {
      ns <- NS(namespace)

      createSessionProxy(self,
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
        }
      )
    },
    ns = function(id) {
      NS(NULL, id)
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
    unhandledError = function(e) {
      self$close()
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
      private$closedCallbacks$invoke(onError = printError, ..stacktraceon = TRUE)
      flushReact()
      lapply(appsByToken$values(), function(shinysession) {
        shinysession$flushOutput()
        NULL
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
    defineOutput = function(name, func, label) {
      "Binds an output generating function to this name. The function can either
      take no parameters, or have named parameters for \\code{name} and
      \\code{shinysession} (in the future this list may expand, so it is a good idea
      to also include \\code{...} in your function signature)."

      # jcheng 08/31/2012: User submitted an example of a dynamically calculated
      # name not working unless name was eagerly evaluated. Yikes!
      force(name)

      # If overwriting an output object, suspend the previous copy of it
      if (!is.null(private$.outputs[[name]])) {
        private$.outputs[[name]]$suspend()
      }

      if (is.function(func)) {
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

          self$sendCustomMessage('recalculating', list(
            name = name, status = 'recalculating'
          ))

          value <- tryCatch(
            shinyCallingHandlers(func()),
            shiny.silent.error = function(cond) {
              # Don't let shiny.silent.error go through the normal stop
              # path of try, because we don't want it to print. But we
              # do want to try to return the same looking result so that
              # the code below can send the error to the browser.
              structure(
                NULL,
                class = "try-error",
                condition = cond
              )
            },
            error = function(cond) {
              msg <- paste0("Error in output$", name, ": ", conditionMessage(cond), "\n")
              if (isTRUE(getOption("show.error.messages"))) {
                printError(cond)
              }
              invisible(structure(msg, class = "try-error", condition = cond))
            }
          )

          self$sendCustomMessage('recalculating', list(
            name = name, status = 'recalculated'
          ))

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
        }, suspended=private$shouldSuspend(name), label=label)

        obs$onInvalidate(function() {
          self$showProgress(name)
        })

        private$.outputs[[name]] <- obs
        if (is.null(private$.outputOptions[[name]]))
          private$.outputOptions[[name]] <- list()
      }
      else {
        stop(paste("Unexpected", class(func), "output for", name))
      }
    },
    flushOutput = function() {

      # ..stacktraceon matches with the top-level ..stacktraceoff..
      private$flushCallbacks$invoke(..stacktraceon = TRUE)
      # ..stacktraceon matches with the top-level ..stacktraceoff..
      on.exit(private$flushedCallbacks$invoke(..stacktraceon = TRUE))

      if (length(private$progressKeys) == 0
          && length(private$invalidatedOutputValues) == 0
          && length(private$invalidatedOutputErrors) == 0
          && length(private$inputMessageQueue) == 0) {
        return(invisible())
      }

      private$progressKeys <- character(0)

      values <- private$invalidatedOutputValues
      private$invalidatedOutputValues <- Map$new()
      errors <- private$invalidatedOutputErrors
      private$invalidatedOutputErrors <- Map$new()
      inputMessages <- private$inputMessageQueue
      private$inputMessageQueue <- list()

      json <- toJSON(list(errors=as.list(errors),
                          values=as.list(values),
                          inputMessages=inputMessages))

      private$write(json)
    },
    showProgress = function(id) {
      'Send a message to the client that recalculation of the output identified
      by \\code{id} is in progress. There is currently no mechanism for
      explicitly turning off progress for an output component; instead, all
      progress is implicitly turned off when flushOutput is next called.'

      # If app is already closed, be sure not to show progress, otherwise we
      # will get an error because of the closed websocket
      if (self$closed)
        return()

      if (id %in% private$progressKeys)
        return()

      private$progressKeys <- c(private$progressKeys, id)

      self$sendProgress('binding', list(id = id))
    },
    sendProgress = function(type, message) {
      json <- toJSON(list(
        progress = list(type = type, message = message)
      ))
      private$write(json)
    },
    dispatch = function(msg) {
      method <- paste('@', msg$method, sep='')
      # we must use $ instead of [[ here at the moment; see
      # https://github.com/rstudio/shiny/issues/274
      func <- try(do.call(`$`, list(self, method)), silent=TRUE)
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
    sendCustomMessage = function(type, message) {
      data <- list()
      data[[type]] <- message
      private$write(toJSON(list(custom=data)))
    },
    sendInputMessage = function(inputId, message) {
      data <- list(id = inputId, message = message)

      # Add to input message queue
      private$inputMessageQueue[[length(private$inputMessageQueue) + 1]] <- data
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
    reactlog = function(logEntry) {
      if (private$showcase)
        self$sendCustomMessage("reactlog", logEntry)
    },
    reload = function() {
      self$sendCustomMessage("reload", TRUE)
    },

    # Public RPC methods
    `@uploadieFinish` = function() {
      # Do nothing; just want the side effect of flushReact, output flush, etc.
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

      if (matches[2] == 'uploadie' && identical(req$REQUEST_METHOD, "POST")) {
        id <- URLdecode(matches[3])
        res <- mime::parse_multipart(req)
        private$.input$set(id, res[[id]])
        return(httpResponse(200, 'text/plain', 'OK'))
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
        # ..stacktraceon matches with the top-level ..stacktraceoff..
        result <- try(shinyCallingHandlers(Context$new(getDefaultReactiveDomain(), '[download]')$run(
          function() { ..stacktraceon..(download$func(tmpdata)) }
        )))
        if (inherits(result, 'try-error')) {
          cond <- attr(result, 'condition', exact = TRUE)
          printError(cond)
          unlink(tmpdata)
          return(httpResponse(500, 'text/plain; charset=UTF-8',
                              enc2utf8(conditionMessage(cond))))
        }
        return(httpResponse(
          200,
          download$contentType %OR% getContentType(filename),
          # owned=TRUE means tmpdata will be deleted after response completes
          list(file=tmpdata, owned=TRUE),
          c(
            'Content-Disposition' = ifelse(
              dlmatches[3] == '',
              'attachment; filename="' %.%
                gsub('(["\\\\])', '\\\\\\1', filename) %.%  # yes, that many \'s
                '"',
              'attachment'
            ),
            'Cache-Control'='no-cache')))
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
    saveFileUrl = function(name, data, contentType, extra=list()) {
      "Creates an entry in the file map for the data, and returns a URL pointing
      to the file."
      self$files$set(name, list(data=data, contentType=contentType))
      return(sprintf('session/%s/file/%s?w=%s&r=%s',
                     URLencode(self$token, TRUE),
                     URLencode(name, TRUE),
                     workerId(),
                     createUniqueId(8)))
    },
    # Send a file to the client
    fileUrl = function(name, file, contentType='application/octet-stream') {
      "Return a URL for a file to be sent to the client. If allowDataUriScheme
      is TRUE, then the file will be base64 encoded and embedded in the URL.
      Otherwise, a URL pointing to the file will be returned."
      bytes <- file.info(file)$size
      if (is.na(bytes))
        return(NULL)

      fileData <- readBin(file, 'raw', n=bytes)

      if (isTRUE(private$.clientData$.values$allowDataUriScheme)) {
        b64 <- rawToBase64(fileData)
        return(paste('data:', contentType, ';base64,', b64, sep=''))
      } else {
        return(self$saveFileUrl(name, fileData, contentType))
      }
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
      return(sprintf('session/%s/dataobj/%s?w=%s',
                     URLencode(self$token, TRUE),
                     URLencode(name, TRUE),
                     workerId()))
    },
    # This function suspends observers for hidden outputs and resumes observers
    # for un-hidden outputs.
    manageHiddenOutputs = function() {
      # Find hidden state for each output, and suspend/resume accordingly
      for (outputName in names(private$.outputs)) {
        if (private$shouldSuspend(outputName)) {
          private$.outputs[[outputName]]$suspend()
        } else {
          private$.outputs[[outputName]]$resume()
        }
      }
    },
    # Set the normal and client data input variables
    manageInputs = function(data) {

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
        self$manageHiddenOutputs()
      }

      if ("priority" %in% names(opts)) {
        private$.outputs[[name]]$setPriority(opts[['priority']])
      }

      invisible()
    },
    incrementBusyCount = function() {
      if (private$busyCount == 0L) {
        self$sendCustomMessage("busy", "busy")
      }
      private$busyCount <- private$busyCount + 1L
    },
    decrementBusyCount = function() {
      private$busyCount <- private$busyCount - 1L
      if (private$busyCount == 0L) {
        self$sendCustomMessage("busy", "idle")
      }
    }
  ),
  active = list(
    session = function() {
      shinyDeprecated(
        msg = paste("Attempted to access deprecated shinysession$session object.",
                    "Please just access the shinysession object directly."),
        version = "0.11.1"
      )
      self
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
  stop("Reading objects from shinyoutput object not allowed.")
}

#' @export
`[[.shinyoutput` <- `$.shinyoutput`

#' @export
`[.shinyoutput` <- function(values, name) {
  stop("Single-bracket indexing of shinyoutput object is not allowed.")
}

#' @export
`[<-.shinyoutput` <- function(values, name, value) {
  stop("Single-bracket indexing of shinyoutput object is not allowed.")
}

#' Set options for an output object.
#'
#' These are the available options for an output object:
#' \itemize{
#'   \item suspendWhenHidden. When \code{TRUE} (the default), the output object
#'     will be suspended (not execute) when it is hidden on the web page. When
#'     \code{FALSE}, the output object will not suspend when hidden, and if it
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
#' @param x A shinyoutput object (typically \code{output}).
#' @param name The name of an output observer in the shinyoutput object.
#' @param ... Options to set for the output observer.
#' @export
outputOptions <- function(x, name, ...) {
  if (!inherits(x, "shinyoutput"))
    stop("x must be a shinyoutput object.")

  name <- .subset2(x, 'ns')(name)

  .subset2(x, 'impl')$outputOptions(name, ...)
}
