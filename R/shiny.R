#' @include utils.R
NULL

#' Web Application Framework for R
#'
#' Shiny makes it incredibly easy to build interactive web applications with R.
#' Automatic "reactive" binding between inputs and outputs and extensive
#' pre-built widgets make it possible to build beautiful, responsive, and
#' powerful applications with minimal effort.
#'
#' The Shiny tutorial at \url{http://rstudio.github.com/shiny/tutorial} explains
#' the framework in depth, walks you through building a simple application, and
#' includes extensive annotated examples.
#'
#' @name shiny-package
#' @aliases shiny
#' @docType package
#' @import htmltools httpuv caTools RJSONIO xtable digest methods
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
#' \item{request}{
#'   An environment that implements the Rook specification for HTTP requests.
#'   This is the request that was used to initiate the websocket connection
#'   (as opposed to the request that downloaded the web page for the app).
#' }
#' \item{sendCustomMessage(type, message)}{
#'   Sends a custom message to the web page. \code{type} must be a
#'   single-element character vector giving the type of message, while
#'   \code{message} can be any RJSONIO-encodable value. Custom messages
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
#'
#' @name session
NULL


#' @include utils.R
ShinySession <- setRefClass(
  'ShinySession',
  fields = list(
    .websocket = 'ANY',
    .invalidatedOutputValues = 'Map',
    .invalidatedOutputErrors = 'Map',
    .inputMessageQueue = 'list',    # A list of inputMessages to send when flushed
    .outputs = 'list',       # Keeps track of all the output observer objects
    .outputOptions = 'list', # Options for each of the output observer objects
    .progressKeys = 'character',
    .showcase   = 'ANY',
    .fileUploadContext = 'FileUploadContext',
    .input      = 'ANY', # Internal ReactiveValues object for normal input sent from client
    .clientData = 'ANY', # Internal ReactiveValues object for other data sent from the client
    .closedCallbacks = 'Callbacks',
    .flushCallbacks = 'Callbacks',
    .flushedCallbacks = 'Callbacks',
    input       = 'reactivevalues', # Externally-usable S3 wrapper object for .input
    output      = 'ANY',    # Externally-usable S3 wrapper object for .outputs
    clientData  = 'reactivevalues', # Externally-usable S3 wrapper object for .clientData
    token = 'character',  # Used to identify this instance in URLs
    files = 'Map',        # For keeping track of files sent to client
    downloads = 'Map',
    closed = 'logical',
    session = 'environment',      # Object for the server app to access session stuff
    singletons = 'character'  # Tracks singleton HTML fragments sent to the page
  ),
  methods = list(
    initialize = function(websocket) {
      .websocket <<- websocket
      closed <<- FALSE
      # TODO: Put file upload context in user/app-specific dir if possible

      .input      <<- ReactiveValues$new()
      .clientData <<- ReactiveValues$new()

      input      <<- .createReactiveValues(.input,      readonly=TRUE)
      .setLabel(input, 'input')
      clientData <<- .createReactiveValues(.clientData, readonly=TRUE)
      .setLabel(clientData, 'clientData')

      output     <<- .createOutputWriter(.self)

      token <<- createUniqueId(16)
      .outputs <<- list()
      .outputOptions <<- list()

      session <<- new.env(parent=emptyenv())
      session$clientData        <<- clientData
      session$sendCustomMessage <<- .self$.sendCustomMessage
      session$sendInputMessage  <<- .self$.sendInputMessage
      session$onSessionEnded    <<- .self$onSessionEnded
      session$onEnded           <<- .self$onEnded
      session$onFlush           <<- .self$onFlush
      session$onFlushed         <<- .self$onFlushed
      session$isClosed          <<- .self$isClosed
      session$input             <<- .self$input
      session$output            <<- .self$output
      session$reactlog          <<- .self$reactlog
      session$registerDataObj   <<- .self$registerDataObj
      session$.impl             <<- .self

      if (!is.null(websocket$request$HTTP_SHINY_SERVER_CREDENTIALS)) {
        try({
          creds <- fromJSON(websocket$request$HTTP_SHINY_SERVER_CREDENTIALS)
          session$user <<- creds$user
          session$groups <<- creds$groups
        }, silent=FALSE)
      }

      # session$request should throw an error if httpuv doesn't have
      # websocket$request, but don't throw it until a caller actually
      # tries to access session$request
      delayedAssign('request', websocket$request, assign.env = session)

      .write(toJSON(list(config = list(
        workerId = workerId(),
        sessionId = token
      ))))
    },
    onSessionEnded = function(callback) {
      "Registers the given callback to be invoked when the session is closed
      (i.e. the connection to the client has been severed). The return value
      is a function which unregisters the callback. If multiple callbacks are
      registered, the order in which they are invoked is not guaranteed."
      return(.closedCallbacks$register(callback))
    },
    onEnded = function(callback) {
      "Synonym for onSessionEnded"
      return(onSessionEnded(callback))
    },
    close = function() {
      closed <<- TRUE
      for (output in .outputs) {
        output$suspend()
      }
      .closedCallbacks$invoke(onError=function(e) {
        warning(simpleWarning(
          paste("An error occurred in an onSessionEnded handler:",
                e$message),
          e$call
        ))
      })
      flushReact()
      lapply(appsByToken$values(), function(shinysession) {
        shinysession$flushOutput()
        NULL
      })
    },
    isClosed = function() {
      return(closed)
    },
    isEnded = function() {
      return(isClosed())
    },
    setShowcase = function(value) {
      .showcase <<- !is.null(value) && as.logical(value)
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
      if (!is.null(.outputs[[name]])) {
        .outputs[[name]]$suspend()
      }

      if (is.function(func)) {
        if (length(formals(func)) != 0) {
          orig <- func
          func <- function() {
            orig(name=name, shinysession=.self)
          }
        }

        # Preserve source reference and file information when formatting the
        # label for display in the reactive graph
        srcref <- attr(label, "srcref")
        srcfile <- attr(label, "srcfile")
        label <- sprintf('output$%s <- %s', name, paste(label, collapse='\n'))
        attr(label, "srcref") <- srcref
        attr(label, "srcfile") <- srcfile

        obs <- observe({

          value <- try(
            {
              tryCatch(
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
                }
              )
            },
            silent=FALSE
          )

          .invalidatedOutputErrors$remove(name)
          .invalidatedOutputValues$remove(name)

          if (inherits(value, 'try-error')) {
            cond <- attr(value, 'condition')
            type <- setdiff(class(cond), c('simpleError', 'error', 'condition'))
            .invalidatedOutputErrors$set(
              name,
              list(message = cond$message,
                   call = capture.output(print(cond$call)),
                   type = if (length(type)) type))
          }
          else
            .invalidatedOutputValues$set(name, value)
        }, suspended=.shouldSuspend(name), label=label)

        obs$onInvalidate(function() {
          showProgress(name)
        })

        .outputs[[name]] <<- obs
        if (is.null(.outputOptions[[name]]))
          .outputOptions[[name]] <<- list()
      }
      else {
        stop(paste("Unexpected", class(func), "output for", name))
      }
    },
    flushOutput = function() {

      .flushCallbacks$invoke()
      on.exit(.flushedCallbacks$invoke())

      if (length(.progressKeys) == 0
          && length(.invalidatedOutputValues) == 0
          && length(.invalidatedOutputErrors) == 0
          && length(.inputMessageQueue) == 0) {
        return(invisible())
      }

      .progressKeys <<- character(0)

      values <- .invalidatedOutputValues
      .invalidatedOutputValues <<- Map$new()
      errors <- .invalidatedOutputErrors
      .invalidatedOutputErrors <<- Map$new()
      inputMessages <- .inputMessageQueue
      .inputMessageQueue <<- list()

      json <- toJSON(list(errors=as.list(errors),
                          values=as.list(values),
                          inputMessages=inputMessages))

      .write(json)
    },
    showProgress = function(id) {
      'Send a message to the client that recalculation of the output identified
      by \\code{id} is in progress. There is currently no mechanism for
      explicitly turning off progress for an output component; instead, all
      progress is implicitly turned off when flushOutput is next called.'

      # If app is already closed, be sure not to show progress, otherwise we
      # will get an error because of the closed websocket
      if (closed)
        return()

      if (id %in% .progressKeys)
        return()

      .progressKeys <<- c(.progressKeys, id)

      json <- toJSON(list(progress=list(id)))

      .write(json)
    },
    dispatch = function(msg) {
      method <- paste('@', msg$method, sep='')
      # we must use $ instead of [[ here at the moment; see
      # https://github.com/rstudio/shiny/issues/274
      func <- try(do.call(`$`, list(.self, method)), silent=TRUE)
      if (inherits(func, 'try-error')) {
        .sendErrorResponse(msg, paste('Unknown method', msg$method))
      }

      value <- try(do.call(func, as.list(append(msg$args, msg$blobs))),
                   silent=TRUE)
      if (inherits(value, 'try-error')) {
        .sendErrorResponse(msg, conditionMessage(attr(value, 'condition')))
      }
      else {
        .sendResponse(msg, value)
      }
    },
    .sendResponse = function(requestMsg, value) {
      if (is.null(requestMsg$tag)) {
        warning("Tried to send response for untagged message; method: ",
                requestMsg$method)
        return()
      }
      .write(toJSON(list(response=list(tag=requestMsg$tag, value=value))))
    },
    .sendErrorResponse = function(requestMsg, error) {
      if (is.null(requestMsg$tag))
        return()
      .write(toJSON(list(response=list(tag=requestMsg$tag, error=error))))
    },
    .sendCustomMessage = function(type, message) {
      data <- list()
      data[[type]] <- message
      .write(toJSON(list(custom=data)))
    },
    .sendInputMessage = function(inputId, message) {
      data <- list(id = inputId, message = message)

      # Add to input message queue
      .inputMessageQueue[[length(.inputMessageQueue) + 1]] <<- data
    },
    onFlush = function(func, once = TRUE) {
      if (!isTRUE(once)) {
        return(.flushCallbacks$register(func))
      } else {
        dereg <- .flushCallbacks$register(function() {
          dereg()
          func()
        })
        return(dereg)
      }
    },
    onFlushed = function(func, once = TRUE) {
      if (!isTRUE(once)) {
        return(.flushedCallbacks$register(func))
      } else {
        dereg <- .flushedCallbacks$register(function() {
          dereg()
          func()
        })
        return(dereg)
      }
    },
    reactlog = function(logEntry) {
      if (.showcase)
        .sendCustomMessage("reactlog", logEntry)
    },
    .write = function(json) {
      if (closed){
        return()
      }
      if (getOption('shiny.trace', FALSE))
        message('SEND ',
           gsub('(?m)base64,[a-zA-Z0-9+/=]+','[base64 data]',json,perl=TRUE))
      if (getOption('shiny.transcode.json', TRUE))
        json <- iconv(json, to='UTF-8')
      .websocket$send(json)
    },

    # Public RPC methods
    `@uploadInit` = function(fileInfos) {
      maxSize <- getOption('shiny.maxRequestSize', 5 * 1024 * 1024)
      fileInfos <- lapply(fileInfos, function(fi) {
        if (is.null(fi$type))
          fi$type <- getContentType(tools::file_ext(fi$name))
        fi
      })
      sizes <- sapply(fileInfos, function(fi){ fi$size })
      if (maxSize > 0 && any(sizes > maxSize)) {
        stop("Maximum upload size exceeded")
      }

      jobId <- .fileUploadContext$createUploadOperation(fileInfos)
      return(list(jobId=jobId,
                  uploadUrl=paste('session', token, 'upload',
                                  paste(jobId, "?w=", workerId(), sep=""),
                                  sep='/')))
    },
    `@uploadEnd` = function(jobId, inputId) {
      fileData <- .fileUploadContext$getUploadOperation(jobId)$finish()
      .input$set(inputId, fileData)
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
        savedFile <- files$get(utils::URLdecode(matches[3]))
        if (is.null(savedFile))
          return(httpResponse(404, 'text/html', '<h1>Not Found</h1>'))

        return(httpResponse(200, savedFile$contentType, savedFile$data))
      }

      if (matches[2] == 'upload' && identical(req$REQUEST_METHOD, "POST")) {
        job <- .fileUploadContext$getUploadOperation(matches[3])
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
        dlname <- utils::URLdecode(dlmatches[2])
        download <- downloads$get(dlname)
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
                                 utils::URLencode(dlname, TRUE),
                                 utils::URLencode(filename, TRUE)),
            'Cache-Control' = 'no-cache')))
        }

        tmpdata <- tempfile()
        result <- try(Context$new(getDefaultReactiveDomain(), '[download]')$run(
          function() { download$func(tmpdata) }
        ))
        if (inherits(result, 'try-error')) {
          unlink(tmpdata)
          return(httpResponse(500, 'text/plain',
                              attr(result, 'condition')$message))
        }
        return(httpResponse(
          200,
          download$contentType %OR% getContentType(tools::file_ext(filename)),
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
        dlname <- utils::URLdecode(dlmatches[2])
        download <- downloads$get(dlname)
        return(download$filter(download$data, req))
      }

      return(httpResponse(404, 'text/html', '<h1>Not Found</h1>'))
    },
    saveFileUrl = function(name, data, contentType, extra=list()) {
      "Creates an entry in the file map for the data, and returns a URL pointing
      to the file."
      files$set(name, list(data=data, contentType=contentType))
      return(sprintf('session/%s/file/%s?w=%s&r=%s',
                     URLencode(token, TRUE),
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

      if (isTRUE(.clientData$.values$allowDataUriScheme)) {
        b64 <- base64encode(fileData)
        return(paste('data:', contentType, ';base64,', b64, sep=''))
      } else {
        return(saveFileUrl(name, fileData, contentType))
      }
    },
    registerDownload = function(name, filename, contentType, func) {

      downloads$set(name, list(filename = filename,
                               contentType = contentType,
                               func = func))
      return(sprintf('session/%s/download/%s?w=%s',
                     URLencode(token, TRUE),
                     URLencode(name, TRUE),
                     workerId()))
    },
    # register a data object on the server side (for datatable or selectize, etc)
    registerDataObj = function(name, data, filterFunc) {
      # abusing downloads at the moment
      downloads$set(name, list(data = data, filter = filterFunc))
      return(sprintf('session/%s/dataobj/%s?w=%s',
                     URLencode(token, TRUE),
                     URLencode(name, TRUE),
                     workerId()))
    },
    .getOutputOption = function(outputName, propertyName, defaultValue) {
      opts <- .outputOptions[[outputName]]
      if (is.null(opts))
        return(defaultValue)
      result <- opts[[propertyName]]
      if (is.null(result))
        return(defaultValue)
      return(result)
    },
    .shouldSuspend = function(name) {
      # Find corresponding hidden state clientData variable, with the format
      # "output_foo_hidden". (It comes from .clientdata_output_foo_hidden
      # on the JS side)
      # Some tricky stuff: instead of accessing names using input$names(),
      # get the names directly via input$.values, to avoid triggering reactivity.
      # Need to handle cases where the output object isn't actually used
      # in the web page; in these cases, there's no output_foo_hidden flag,
      # and hidden should be TRUE. In other words, NULL and TRUE should map to
      # TRUE, FALSE should map to FALSE.
      hidden <- .clientData$.values[[paste("output_", name, "_hidden",
                                           sep="")]]
      if (is.null(hidden)) hidden <- TRUE

      return(hidden && .getOutputOption(name, 'suspendWhenHidden', TRUE))
    },
    # This function suspends observers for hidden outputs and resumes observers
    # for un-hidden outputs.
    manageHiddenOutputs = function() {
      # Find hidden state for each output, and suspend/resume accordingly
      for (outputName in names(.outputs)) {
        if (.shouldSuspend(outputName)) {
          .outputs[[outputName]]$suspend()
        } else {
          .outputs[[outputName]]$resume()
        }
      }
    },
    # Set the normal and client data input variables
    manageInputs = function(data) {
      data_names <- names(data)

      # Separate normal input variables from client data input variables
      clientdata_idx <- grepl("^.clientdata_", data_names)

      # Set normal (non-clientData) input values
      .input$mset(data[data_names[!clientdata_idx]])

      # Strip off .clientdata_ from clientdata input names, and set values
      input_clientdata <- data[data_names[clientdata_idx]]
      names(input_clientdata) <- sub("^.clientdata_", "",
                                     names(input_clientdata))
      .clientData$mset(input_clientdata)
    },
    outputOptions = function(name, ...) {
      # If no name supplied, return the list of options for all outputs
      if (is.null(name))
        return(.outputOptions)
      if (! name %in% names(.outputs))
        stop(name, " is not in list of output objects")

      opts <- list(...)
      # If no options are set, return the options for the specified output
      if (length(opts) == 0)
        return(.outputOptions[[name]])

      # Set the appropriate option
      validOpts <- c("suspendWhenHidden", "priority")
      for (optname in names(opts)) {
        if (! optname %in% validOpts)
          stop(optname, " is not a valid option")

        .outputOptions[[name]][[optname]] <<- opts[[optname]]
      }

      # If any changes to suspendWhenHidden, need to re-run manageHiddenOutputs
      if ("suspendWhenHidden" %in% names(opts)) {
        manageHiddenOutputs()
      }

      if ("priority" %in% names(opts)) {
        .outputs[[name]]$setPriority(opts[['priority']])
      }

      invisible()
    }
  )
)

.createOutputWriter <- function(shinysession) {
  structure(list(impl=shinysession), class='shinyoutput')
}

#' @export
`$<-.shinyoutput` <- function(x, name, value) {
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

  .subset2(x, 'impl')$outputOptions(name, ...)
}
