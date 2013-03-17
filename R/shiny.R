#' @docType package
#' @import httpuv caTools RJSONIO xtable digest
NULL

suppressPackageStartupMessages({
  library(httpuv)
  library(RJSONIO)
})

createUniqueId <- function(bytes) {
  # TODO: Use a method that isn't affected by the R seed
  paste(as.character(as.raw(floor(runif(bytes, min=1, max=255)))), collapse='')
}

ShinySession <- setRefClass(
  'ShinySession',
  fields = list(
    .websocket = 'ANY',
    .invalidatedOutputValues = 'Map',
    .invalidatedOutputErrors = 'Map',
    .outputs = 'list',       # Keeps track of all the output observer objects
    .outputOptions = 'list', # Options for each of the output observer objects
    .progressKeys = 'character',
    .fileUploadContext = 'FileUploadContext',
    .input      = 'ReactiveValues', # Internal object for normal input sent from client
    .clientData = 'ReactiveValues', # Internal object for other data sent from the client
    input       = 'reactivevalues', # Externally-usable S3 wrapper object for .input
    clientData  = 'reactivevalues', # Externally-usable S3 wrapper object for .clientData
    token = 'character',  # Used to identify this instance in URLs
    files = 'Map',        # For keeping track of files sent to client
    downloads = 'Map',
    closed = 'logical'
  ),
  methods = list(
    initialize = function(websocket) {
      .websocket <<- websocket
      .invalidatedOutputValues <<- Map$new()
      .invalidatedOutputErrors <<- Map$new()
      .progressKeys <<- character(0)
      closed <<- FALSE
      # TODO: Put file upload context in user/app-specific dir if possible
      .fileUploadContext <<- FileUploadContext$new()

      .input      <<- ReactiveValues$new()
      .clientData <<- ReactiveValues$new()

      input      <<- .createReactiveValues(.input,      readonly=TRUE)
      clientData <<- .createReactiveValues(.clientData, readonly=TRUE)
      
      token <<- createUniqueId(16)
      .outputs <<- list()
      .outputOptions <<- list()
    },
    close = function() {
      closed <<- TRUE
      for (output in .outputs) {
        output$suspend()
      }
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

        obs <- observe({
          
          value <- try(func(), silent=FALSE)
          
          .invalidatedOutputErrors$remove(name)
          .invalidatedOutputValues$remove(name)
          
          if (inherits(value, 'try-error')) {
            cond <- attr(value, 'condition')
            .invalidatedOutputErrors$set(
              name, 
              list(message=cond$message,
                   call=capture.output(print(cond$call))))
          }
          else
            .invalidatedOutputValues$set(name, value)
        }, label=label, suspended=TRUE)
        
        obs$onInvalidate(function() {
          showProgress(name)
        })

        .outputs[[name]] <<- obs
        # Default is to suspend when hidden
        .outputOptions[[name]][['suspendWhenHidden']] <<- TRUE
      }
      else {
        stop(paste("Unexpected", class(func), "output for", name))
      }
    },
    flushOutput = function() {
      if (length(.progressKeys) == 0
          && length(.invalidatedOutputValues) == 0
          && length(.invalidatedOutputErrors) == 0) {
        return(invisible())
      }
      
      .progressKeys <<- character(0)
      
      values <- .invalidatedOutputValues
      .invalidatedOutputValues <<- Map$new()
      errors <- .invalidatedOutputErrors
      .invalidatedOutputErrors <<- Map$new()
      
      json <- toJSON(list(errors=as.list(errors),
                          values=as.list(values)))

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
    .write = function(json) {
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
                  uploadUrl=paste('session', token, 'upload', jobId, sep='/')))
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
                           Context$new('[download]')$run(download$filename),
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
        result <- try(Context$new('[download]')$run(function() { download$func(tmpdata) }))
        if (is(result, 'try-error')) {
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
      
      return(httpResponse(404, 'text/html', '<h1>Not Found</h1>'))
    },
    saveFileUrl = function(name, data, contentType, extra=list()) {
      "Creates an entry in the file map for the data, and returns a URL pointing
      to the file."
      files$set(name, list(data=data, contentType=contentType))
      return(sprintf('session/%s/file/%s?%s',
                     URLencode(token, TRUE),
                     URLencode(name, TRUE),
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
      return(sprintf('session/%s/download/%s',
                     URLencode(token, TRUE),
                     URLencode(name, TRUE)))
    },
    # This function suspends observers for hidden outputs and resumes observers
    # for un-hidden outputs.
    manageHiddenOutputs = function() {
      # Find hidden state for each output, and suspend/resume accordingly
      for (outputName in names(.outputs)) {
        # Find corresponding hidden state clientData variable, with the format
        # "output_foo_hidden". (It comes from .clientdata_output_foo_hidden
        # on the JS side)
        # Some tricky stuff: instead of accessing names using input$names(),
        # get the names directly via input$.values, to avoid triggering reactivity.
        # Need to handle cases where the output object isn't actually used
        # in the web page; in these cases, there's no output_foo_hidden flag,
        # and hidden should be TRUE. In other words, NULL and TRUE should map to
        # TRUE, FALSE should map to FALSE.
        hidden <- .clientData$.values[[paste("output_", outputName, "_hidden",
                                             sep="")]]
        if (is.null(hidden)) hidden <- TRUE

        if (hidden && .outputOptions[[outputName]][['suspendWhenHidden']]) {
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
      validOpts <- "suspendWhenHidden"
      for (optname in names(opts)) {
        if (! optname %in% validOpts)
          stop(optname, " is not a valid option")

        .outputOptions[[name]][[optname]] <<- opts[[optname]]
      }

      # If any changes to suspendWhenHidden, need to re-run manageHiddenOutputs
      if ("suspendWhenHidden" %in% names(opts)) {
        manageHiddenOutputs()
      }

      invisible()
    }
  )
)

.createOutputWriter <- function(shinysession) {
  structure(list(impl=shinysession), class='shinyoutput')
}

#' @S3method $<- shinyoutput
`$<-.shinyoutput` <- function(x, name, value) {
  .subset2(x, 'impl')$defineOutput(name, value, deparse(substitute(value)))
  return(invisible(x))
}

#' @S3method [[<- shinyoutput
`[[<-.shinyoutput` <- `$<-.shinyoutput`

#' @S3method $ shinyoutput
`$.shinyoutput` <- function(x, name) {
  stop("Reading objects from shinyoutput object not allowed.")
}

#' @S3method [[ shinyoutput
`[[.shinyoutput` <- `$.shinyoutput`

#' @S3method [ shinyoutput
`[.shinyoutput` <- function(values, name) {
  stop("Single-bracket indexing of shinyoutput object is not allowed.")
}

#' @S3method [<- shinyoutput
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


resolve <- function(dir, relpath) {
  abs.path <- file.path(dir, relpath)
  if (!file.exists(abs.path))
    return(NULL)
  abs.path <- normalizePath(abs.path, winslash='/', mustWork=TRUE)
  dir <- normalizePath(dir, winslash='/', mustWork=TRUE)
  if (nchar(abs.path) <= nchar(dir) + 1)
    return(NULL)
  if (substr(abs.path, 1, nchar(dir)) != dir ||
    !(substr(abs.path, nchar(dir)+1, nchar(dir)+1) %in% c('/', '\\'))) {
    return(NULL)
  }
  return(abs.path)
}

httpResponse <- function(status = 200,
                         content_type = "text/html; charset=UTF-8", 
                         content = "",
                         headers = list()) {
  # Make sure it's a list, not a vector
  headers <- as.list(headers)
  if (is.null(headers$`X-UA-Compatible`))
    headers$`X-UA-Compatible` <- "chrome=1"
  resp <- list(status = status, content_type = content_type, content = content,
               headers = headers)
  class(resp) <- 'httpResponse'
  return(resp)
}

httpServer <- function(handlers) {
  handler <- joinHandlers(handlers)

  # TODO: Figure out what this means after httpuv migration
  filter <- getOption('shiny.http.response.filter', NULL)
  if (is.null(filter))
    filter <- function(req, response) response
  
  function(req) {
    response <- handler(req)
    if (is.null(response))
      response <- httpResponse(404, content="<h1>Not Found</h1>")
    
    headers <- as.list(response$headers)
    headers$'Content-Type' <- response$content_type
    
    response <- filter(req, response)
    return(list(status=response$status,
                body=response$content,
                headers=headers))
  }
}

joinHandlers <- function(handlers) {
  handlers <- lapply(handlers, function(h) {
    if (is.character(h))
      return(staticHandler(h))
    else
      return(h)
  })
  
  # Filter out NULL
  handlers <- handlers[!sapply(handlers, is.null)]
  
  if (length(handlers) == 0)
    return(function(req) NULL)
  if (length(handlers) == 1)
    return(handlers[[1]])
  
  function(req) {
    for (handler in handlers) {
      response <- handler(req)
      if (!is.null(response))
        return(response)
    }
    return(NULL)
  }
}

sessionHandler <- function(req) {
  path <- req$PATH_INFO
  if (is.null(path))
    return(NULL)
  
  matches <- regmatches(path, regexec('^(/session/([0-9a-f]+))(/.*)$', path))
  if (length(matches[[1]]) == 0)
    return(NULL)
  
  session <- matches[[1]][3]
  subpath <- matches[[1]][4]
  
  shinysession <- appsByToken$get(session)
  if (is.null(shinysession))
    return(NULL)
  
  subreq <- as.environment(as.list(req, all.names=TRUE))
  subreq$PATH_INFO <- subpath
  subreq$SCRIPT_NAME <- paste(subreq$SCRIPT_NAME, matches[[1]][2], sep='')
  
  return(shinysession$handleRequest(subreq))
}

dynamicHandler <- function(filePath, dependencyFiles=filePath) {
  lastKnownTimestamps <- NA
  metaHandler <- function(req) NULL
  
  if (!file.exists(filePath))
    return(metaHandler)
  
  cacheContext <- CacheContext$new()
  
  return (function(req) {
    # Check if we need to rebuild
    if (cacheContext$isDirty()) {
      cacheContext$reset()
      for (dep in dependencyFiles)
        cacheContext$addDependencyFile(dep)

      clearClients()
      if (file.exists(filePath)) {
        local({
          cacheContext$with(function() {
            source(filePath, local=new.env(parent=.GlobalEnv))
          })
        })
      }
      metaHandler <<- joinHandlers(.globals$clients)
      clearClients()
    }
    
    return(metaHandler(req))
  })
}

staticHandler <- function(root) {
  return(function(req) {
    if (!identical(req$REQUEST_METHOD, 'GET'))
      return(NULL)

    path <- req$PATH_INFO
    
    if (is.null(path))
      return(httpResponse(400, content="<h1>Bad Request</h1>"))
    
    if (path == '/')
      path <- '/index.html'
    
    abs.path <- resolve(root, path)
    if (is.null(abs.path))
      return(NULL)
    
    ext <- tools::file_ext(abs.path)
    content.type <- getContentType(ext)
    response.content <- readBin(abs.path, 'raw', n=file.info(abs.path)$size)
    return(httpResponse(200, content.type, response.content))
  })
}

appsByToken <- Map$new()

# Provide a character representation of the WS that can be used
# as a key in a Map.
wsToKey <- function(WS) {
  as.character(WS$socket)
}

.globals <- new.env()

.globals$clients <- function(req) NULL


clearClients <- function() {
  .globals$clients <- function(req) NULL
}


registerClient <- function(client) {
  .globals$clients <- append(.globals$clients, client)
}


.globals$resources <- list()

#' Resource Publishing
#' 
#' Adds a directory of static resources to Shiny's web server, with the given 
#' path prefix. Primarily intended for package authors to make supporting 
#' JavaScript/CSS files available to their components.
#' 
#' @param prefix The URL prefix (without slashes). Valid characters are a-z, 
#'   A-Z, 0-9, hyphen, and underscore; and must begin with a-z or A-Z. For 
#'   example, a value of 'foo' means that any request paths that begin with 
#'   '/foo' will be mapped to the given directory.
#' @param directoryPath The directory that contains the static resources to be 
#'   served.
#'   
#' @details You can call \code{addResourcePath} multiple times for a given 
#'   \code{prefix}; only the most recent value will be retained. If the 
#'   normalized \code{directoryPath} is different than the directory that's 
#'   currently mapped to the \code{prefix}, a warning will be issued.
#'   
#' @seealso \code{\link{singleton}}
#' 
#' @examples
#' addResourcePath('datasets', system.file('data', package='datasets'))
#'   
#' @export
addResourcePath <- function(prefix, directoryPath) {
  prefix <- prefix[1]
  if (!grepl('^[a-z][a-z0-9\\-_]*$', prefix, ignore.case=TRUE, perl=TRUE)) {
    stop("addResourcePath called with invalid prefix; please see documentation")
  }
  
  if (prefix %in% c('shared')) {
    stop("addResourcePath called with the reserved prefix '", prefix, "'; ",
         "please use a different prefix")
  }
  
  directoryPath <- normalizePath(directoryPath, mustWork=TRUE)
  
  existing <- .globals$resources[[prefix]]
  
  if (!is.null(existing)) {
    if (existing$directoryPath != directoryPath) {
      warning("Overriding existing prefix ", prefix, " => ",
              existing$directoryPath)
    }
  }
  
  message('Shiny URLs starting with /', prefix, ' will mapped to ', directoryPath)
  
  .globals$resources[[prefix]] <- list(directoryPath=directoryPath,
                                       func=staticHandler(directoryPath))
}

resourcePathHandler <- function(req) {
  if (!identical(req$REQUEST_METHOD, 'GET'))
    return(NULL)

  path <- req$PATH_INFO
  
  match <- regexpr('^/([^/]+)/', path, perl=TRUE)
  if (match == -1)
    return(NULL)
  len <- attr(match, 'capture.length')
  prefix <- substr(path, 2, 2 + len - 1)
  
  resInfo <- .globals$resources[[prefix]]
  if (is.null(resInfo))
    return(NULL)
  
  suffix <- substr(path, 2 + len, nchar(path))
  
  subreq <- as.environment(as.list(req, all.names=TRUE))
  subreq$PATH_INFO <- suffix
  subreq$SCRIPT_NAME <- paste(subreq$SCRIPT_NAME, substr(path, 1, 2 + len), sep='')
  
  return(resInfo$func(subreq))
}

.globals$server <- NULL
#' Define Server Functionality
#' 
#' Defines the server-side logic of the Shiny application. This generally 
#' involves creating functions that map user inputs to various kinds of output.
#' 
#' @param func The server function for this application. See the details section
#'   for more information.
#'   
#' @details
#' Call \code{shinyServer} from your application's \code{server.R} file, passing
#' in a "server function" that provides the server-side logic of your 
#' application.
#' 
#' The server function will be called when each client (web browser) first loads
#' the Shiny application's page. It must take an \code{input} and an
#' \code{output} parameter. Any return value will be ignored.
#' 
#' See the \href{http://rstudio.github.com/shiny/tutorial/}{tutorial} for more 
#' on how to write a server function.
#' 
#' @examples
#' \dontrun{
#' # A very simple Shiny app that takes a message from the user
#' # and outputs an uppercase version of it.
#' shinyServer(function(input, output) {
#'   output$uppercase <- renderText({
#'     toupper(input$message)
#'   })
#' })
#' }
#' 
#' @export
shinyServer <- function(func) {
  .globals$server <- func
  invisible()
}

decodeMessage <- function(data) {
  readInt <- function(pos) {
    packBits(rawToBits(data[pos:(pos+3)]), type='integer')
  }
  
  if (readInt(1) != 0x01020202L)
    return(fromJSON(rawToChar(data), asText=TRUE, simplify=FALSE))
  
  i <- 5
  parts <- list()
  while (i <= length(data)) {
    length <- readInt(i)
    i <- i + 4
    if (length != 0)
      parts <- append(parts, list(data[i:(i+length-1)]))
    else
      parts <- append(parts, list(raw(0)))
    i <- i + length
  }
  
  mainMessage <- decodeMessage(parts[[1]])
  mainMessage$blobs <- parts[2:length(parts)]
  return(mainMessage)
}

# Takes a list-of-lists and returns a matrix. The lists
# must all be the same length. NULL is replaced by NA.
unpackMatrix <- function(data) {
  if (length(data) == 0)
    return(matrix(nrow=0, ncol=0))
  
  m <- matrix(unlist(lapply(data, function(x) {
    sapply(x, function(y) {
      ifelse(is.null(y), NA, y)
    })
  })), nrow = length(data[[1]]), ncol = length(data))
  return(m)
}

# Combine dir and (file)name into a file path. If a file already exists with a
# name differing only by case, then use it instead.
file.path.ci <- function(dir, name) {
  default <- file.path(dir, name)
  if (file.exists(default))
    return(default)
  if (!file.exists(dir))
    return(default)
  
  matches <- list.files(dir, name, ignore.case=TRUE, full.names=TRUE,
                        include.dirs=TRUE)
  if (length(matches) == 0)
    return(default)
  return(matches[[1]])
}

# Instantiates the app in the current working directory.
# port - The TCP port that the application should listen on.
startApp <- function(port=8101L) {

  sys.www.root <- system.file('www', package='shiny')
  
  globalR <- file.path.ci(getwd(), 'global.R')
  uiR <- file.path.ci(getwd(), 'ui.R')
  serverR <- file.path.ci(getwd(), 'server.R')
  wwwDir <- file.path.ci(getwd(), 'www')
  
  if (!file.exists(uiR) && !file.exists(wwwDir))
    stop(paste("Neither ui.R nor a www subdirectory was found in", getwd()))
  if (!file.exists(serverR))
    stop(paste("server.R file was not found in", getwd()))
  
  if (file.exists(globalR))
    source(globalR, local=FALSE)
  
  shinyServer(NULL)
  serverFileTimestamp <- NULL
  local({
    serverFileTimestamp <<- file.info(serverR)$mtime
    source(serverR, local=new.env(parent=.GlobalEnv))
    if (is.null(.globals$server))
      stop("No server was defined in server.R")
  })
  serverFunc <- .globals$server
  
  httpuvCallbacks <- list(
    onHeaders = function(req) {
      maxSize <- getOption('shiny.maxRequestSize', 5 * 1024 * 1024)
      if (maxSize <= 0)
        return(NULL)

      reqSize <- 0
      if (length(req$CONTENT_LENGTH) > 0)
        reqSize <- as.numeric(req$CONTENT_LENGTH)
      else if (length(req$HTTP_TRANSFER_ENCODING) > 0)
        reqSize <- Inf

      if (reqSize > maxSize) {
        return(list(status = 413L,
                    headers = list(
                      'Content-Type' = 'text/plain'
                    ),
                    body = 'Maximum upload size exceeded'))
      }
      else {
        return(NULL)
      }
    },
    call = httpServer(c(sessionHandler,
                        dynamicHandler(uiR),
                        wwwDir,
                        sys.www.root,
                        resourcePathHandler)),
    onWSOpen = function(ws) {
      shinysession <- ShinySession$new(ws)
      appsByToken$set(shinysession$token, shinysession)
      
      ws$onMessage(function(binary, msg) {
        
        # To ease transition from websockets-based code. Should remove once we're stable.
        if (is.character(msg))
          msg <- charToRaw(msg)
        
        if (getOption('shiny.trace', FALSE)) {
          if (binary)
            message("RECV ", '$$binary data$$')
          else
            message("RECV ", rawToChar(msg))
        }
        
        if (identical(charToRaw("\003\xe9"), msg))
          return()
        
        msg <- decodeMessage(msg)
        
        # Do our own list simplifying here. sapply/simplify2array give names to
        # character vectors, which is rarely what we want.
        if (!is.null(msg$data)) {
          for (name in names(msg$data)) {
            val <- msg$data[[name]]
            
            splitName <- strsplit(name, ':')[[1]]
            if (length(splitName) > 1) {
              msg$data[[name]] <- NULL
              
              # TODO: Make the below a user-extensible registry of deserializers
              msg$data[[ splitName[[1]] ]] <- switch(
                splitName[[2]],
                matrix = unpackMatrix(val),
                number = ifelse(is.null(val), NA, val),
                stop('Unknown type specified for ', name)
              )
            }
            else if (is.list(val) && is.null(names(val))) {
              val_flat <- unlist(val, recursive = TRUE)
              
              if (is.null(val_flat)) {
                # This is to assign NULL instead of deleting the item
                msg$data[name] <- list(NULL)
              } else {
                msg$data[[name]] <- val_flat
              }
            }
          }
        }
        
        switch(
          msg$method,
          init = {
            
            # Check if server.R has changed, and if so, reload
            mtime <- file.info(serverR)$mtime
            if (!identical(mtime, serverFileTimestamp)) {
              shinyServer(NULL)
              local({
                serverFileTimestamp <<- mtime
                source(serverR, local=new.env(parent=.GlobalEnv))
                if (is.null(.globals$server))
                  stop("No server was defined in server.R")
              })
              serverFunc <<- .globals$server
            }
            
            shinysession$manageInputs(msg$data)
            local({
              args <- list(
                input=shinysession$input,
                output=.createOutputWriter(shinysession))

              # The clientData argument is optional; check if it exists
              if ('clientData' %in% names(formals(serverFunc)))
                args$clientData <- shinysession$clientData

              do.call(serverFunc, args)
            })
          },
          update = {
            shinysession$manageInputs(msg$data)
          },
          shinysession$dispatch(msg)
        )
        shinysession$manageHiddenOutputs()
        flushReact()
        lapply(appsByToken$values(), function(shinysession) {
          shinysession$flushOutput()
          NULL
        })
      })
      
      ws$onClose(function() {
        shinysession$close()
        appsByToken$remove(shinysession$token)
      })
    }
  )
  
  message('\n', 'Listening on port ', port)

  return(startServer("0.0.0.0", port, httpuvCallbacks))
}

# NOTE: we de-roxygenized this comment because the function isn't exported
# Run an application that was created by \code{\link{startApp}}. This
# function should normally be called in a \code{while(TRUE)} loop.
# 
# @param ws_env The return value from \code{\link{startApp}}.
serviceApp <- function(ws_env) {
  if (timerCallbacks$executeElapsed()) {
    for (shinysession in appsByToken$values()) {
      shinysession$manageHiddenOutputs()
    }

    flushReact()

    for (shinysession in appsByToken$values()) {
      shinysession$flushOutput()
    }
  }

  # If this R session is interactive, then call service() with a short timeout
  # to keep the session responsive to user input
  maxTimeout <- ifelse(interactive(), 100, 1000)
  
  timeout <- max(1, min(maxTimeout, timerCallbacks$timeToNextEvent()))
  service(timeout)
}

#' Run Shiny Application
#' 
#' Runs a Shiny application. This function normally does not return; interrupt
#' R to stop the application (usually by pressing Ctrl+C or Esc).
#' 
#' @param appDir The directory of the application. Should contain
#'   \code{server.R}, plus, either \code{ui.R} or a \code{www} directory that
#'   contains the file \code{index.html}. Defaults to the working directory.
#' @param port The TCP port that the application should listen on. Defaults to 
#'   port 8100.
#' @param launch.browser If true, the system's default web browser will be 
#'   launched automatically after the app is started. Defaults to true in 
#'   interactive sessions only.
#'   
#' @export
runApp <- function(appDir=getwd(),
                   port=8100L,
                   launch.browser=getOption('shiny.launch.browser',
                                            interactive())) {

  # Make warnings print immediately
  ops <- options(warn = 1)
  on.exit(options(ops))
  
  orig.wd <- getwd()
  setwd(appDir)
  on.exit(setwd(orig.wd), add = TRUE)
  
  require(shiny)
  
  server <- startApp(port=port)
  on.exit({
    stopServer(server)
  }, add = TRUE)
  
  if (launch.browser) {
    appUrl <- paste("http://localhost:", port, sep="")
    utils::browseURL(appUrl)
  }
  
  tryCatch(
    while (TRUE) {
      serviceApp()
      Sys.sleep(0.001)
    },
    finally = {
      timerCallbacks$clear()
    }
  )
}

#' Run Shiny Example Applications
#' 
#' Launch Shiny example applications, and optionally, your system's web browser.
#' 
#' @param example The name of the example to run, or \code{NA} (the default) to
#'   list the available examples.
#' @param port The TCP port that the application should listen on. Defaults to 
#'   port 8100.
#' @param launch.browser If true, the system's default web browser will be 
#'   launched automatically after the app is started. Defaults to true in 
#'   interactive sessions only.
#'   
#' @export
runExample <- function(example=NA,
                       port=8100L,
                       launch.browser=getOption('shiny.launch.browser',
                                                interactive())) {
  examplesDir <- system.file('examples', package='shiny')
  dir <- resolve(examplesDir, example)
  if (is.null(dir)) {
    if (is.na(example)) {
      errFun <- message
      errMsg <- ''
    }
    else {
      errFun <- stop
      errMsg <- paste('Example', example, 'does not exist. ')
    }
    
    errFun(errMsg,
           'Valid examples are "',
           paste(list.files(examplesDir), collapse='", "'),
           '"')
  }
  else {
    runApp(dir, port = port, launch.browser = launch.browser)
  }
}

# This is a wrapper for download.file and has the same interface.
# The only difference is that, if the protocol is https, it changes the
# download settings, depending on platform.
download <- function(url, ...) {
  # First, check protocol. If http or https, check platform:
  if (grepl('^https?://', url)) {
    
    # If Windows, call setInternet2, then use download.file with defaults.
    if (.Platform$OS.type == "windows") {
      # If we directly use setInternet2, R CMD CHECK gives a Note on Mac/Linux
      mySI2 <- `::`(utils, 'setInternet2')
      # Store initial settings
      internet2_start <- mySI2(NA)
      on.exit(mySI2(internet2_start))
      
      # Needed for https
      mySI2(TRUE)
      download.file(url, ...)
      
    } else {
      # If non-Windows, check for curl/wget/lynx, then call download.file with
      # appropriate method.
      
      if (nzchar(Sys.which("wget")[1])) {
        method <- "wget"
      } else if (nzchar(Sys.which("curl")[1])) {
        method <- "curl"
        
        # curl needs to add a -L option to follow redirects.
        # Save the original options and restore when we exit.
        orig_extra_options <- getOption("download.file.extra")
        on.exit(options(download.file.extra = orig_extra_options))
        
        options(download.file.extra = paste("-L", orig_extra_options))
        
      } else if (nzchar(Sys.which("lynx")[1])) {
        method <- "lynx"
      } else {
        stop("no download method found")
      }
      
      download.file(url, method = method, ...)
    }
    
  } else {
    download.file(url, ...)
  }
}
