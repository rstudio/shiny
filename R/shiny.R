#' @docType package
#' @import websockets caTools RJSONIO xtable digest
NULL

suppressPackageStartupMessages({
  library(websockets)
  library(RJSONIO)
})

createUniqueId <- function(bytes) {
  # TODO: Use a method that isn't affected by the R seed
  paste(as.character(as.raw(floor(runif(bytes, min=1, max=255)))), collapse='')
}

ShinyApp <- setRefClass(
  'ShinyApp',
  fields = list(
    .websocket = 'list',
    .invalidatedOutputValues = 'Map',
    .invalidatedOutputErrors = 'Map',
    .progressKeys = 'character',
    .fileUploadContext = 'FileUploadContext',
    session = 'Values',
    token = 'character',  # Used to identify this instance in URLs
    plots = 'Map',
    downloads = 'Map',
    allowDataUriScheme = 'logical'
  ),
  methods = list(
    initialize = function(ws) {
      .websocket <<- ws
      .invalidatedOutputValues <<- Map$new()
      .invalidatedOutputErrors <<- Map$new()
      .progressKeys <<- character(0)
      # TODO: Put file upload context in user/app-specific dir if possible
      .fileUploadContext <<- FileUploadContext$new()
      session <<- Values$new()
      
      token <<- createUniqueId(16)
      
      allowDataUriScheme <<- TRUE
    },
    defineOutput = function(name, func) {
      "Binds an output generating function to this name. The function can either
      take no parameters, or have named parameters for \\code{name} and
      \\code{shinyapp} (in the future this list may expand, so it is a good idea
      to also include \\code{...} in your function signature)."
      
      # jcheng 08/31/2012: User submitted an example of a dynamically calculated
      # name not working unless name was eagerly evaluated. Yikes!
      force(name)
      
      if (is.function(func)) {
        if (length(formals(func)) != 0) {
          orig <- func
          func <- function() {
            orig(name=name, shinyapp=.self)
          }
        }

        obs <- Observer$new(function() {
          
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
        })
        
        obs$onInvalidateHint(function() {
          showProgress(name)
        })
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
      
      value <- try(do.call(func, as.list(append(msg$args, msg$blobs))))
      if (inherits(value, 'try-error')) {
        .sendErrorResponse(msg, paste('Error:', as.character(value)))
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
        message('SEND ', json)
      if (getOption('shiny.transcode.json', TRUE))
        json <- iconv(json, to='UTF-8')
      websocket_write(json, .websocket)
    },
    
    # Public RPC methods
    `@uploadInit` = function() {
      return(list(jobId=.fileUploadContext$createUploadOperation()))
    },
    `@uploadFileBegin` = function(jobId, fileName, fileType, fileSize) {
      .fileUploadContext$getUploadOperation(jobId)$fileBegin(list(
        name=fileName, type=fileType, size=fileSize
      ))
      invisible()
    },
    `@uploadFileChunk` = function(jobId, ...) {
      args <- list(...)
      if (length(args) != 1)
        stop("Bad file chunk request")
      .fileUploadContext$getUploadOperation(jobId)$fileChunk(args[[1]])
      invisible()
    },
    `@uploadFileEnd` = function(jobId) {
      .fileUploadContext$getUploadOperation(jobId)$fileEnd()
      invisible()
    },
    `@uploadEnd` = function(jobId, inputId) {
      fileData <- .fileUploadContext$getUploadOperation(jobId)$finish()
      session$set(inputId, fileData)
      invisible()
    },
    # Provides a mechanism for handling direct HTTP requests that are posted
    # to the session (rather than going through the websocket)
    handleRequest = function(ws, header, subpath) {
      # TODO: Turn off caching for the response
      
      matches <- regmatches(subpath,
                            regexec("^/([a-z]+)/([^?]*)", 
                                    subpath, 
                                    ignore.case=TRUE))[[1]]
      if (length(matches) == 0)
        return(httpResponse(400, 'text/html', '<h1>Bad Request</h1>'))
      
      if (matches[2] == 'plot') {
        savedPlot <- plots$get(utils::URLdecode(matches[3]))
        if (is.null(savedPlot))
          return(httpResponse(404, 'text/html', '<h1>Not Found</h1>'))
        
        return(httpResponse(200, savedPlot$contentType, savedPlot$data))
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
                           Context$new()$run(download$filename),
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
        on.exit(unlink(tmpdata))
        conn <- file(tmpdata, open = 'wb')
        result <- try(Context$new()$run(function() { download$func(conn) }))
        if (is(result, 'try-error')) {
          return(httpResponse(500, 'text/plain', 
                              attr(result, 'condition')$message))
        }
        close(conn)
        return(httpResponse(
          200,
          download$contentType %OR% getContentType(tools::file_ext(filename)),
          readBin(tmpdata, 'raw', n=file.info(tmpdata)$size),
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
    savePlot = function(name, data, contentType) {
      plots$set(name, list(data=data, contentType=contentType))
      return(sprintf('session/%s/plot/%s?%s',
                     URLencode(token, TRUE),
                     URLencode(name, TRUE),
                     createUniqueId(8)))
    },
    registerDownload = function(name, filename, contentType, func) {
      
      downloads$set(name, list(filename = filename,
                               contentType = contentType,
                               func = func))
      return(sprintf('session/%s/download/%s',
                     URLencode(token, TRUE),
                     URLencode(name, TRUE)))
    }
  )
)

.createOutputWriter <- function(shinyapp) {
  ow <- list(impl=shinyapp)
  class(ow) <- 'shinyoutput'
  return(ow)
}

#' @S3method $<- shinyoutput
`$<-.shinyoutput` <- function(x, name, value) {
  x[['impl']]$defineOutput(name, value)
  return(invisible(x))
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
                         headers = c()) {
  resp <- list(status = status, content_type = content_type, content = content,
               headers = headers)
  class(resp) <- 'httpResponse'
  return(resp)
}

httpServer <- function(handlers) {
  handler <- joinHandlers(handlers)

  filter <- getOption('shiny.http.response.filter', NULL)
  if (is.null(filter))
    filter <- function(ws, header, response) response
  
  function(ws, header) {
    response <- handler(ws, header)
    if (is.null(response))
      response <- httpResponse(404, content="<h1>Not Found</h1>")
    
    response <- filter(ws, header, response)
    
    return(http_response(ws,
                         status=response$status,
                         content_type=response$content_type,
                         content=response$content,
                         headers=response$headers))
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
    return(function(ws, header) NULL)
  if (length(handlers) == 1)
    return(handlers[[1]])
  
  function(ws, header) {
    for (handler in handlers) {
      response <- handler(ws, header)
      if (!is.null(response))
        return(response)
    }
    return(NULL)
  }
}

sessionHandler <- function(ws, header) {
  path <- header$RESOURCE
  if (is.null(path))
    return(NULL)
  
  matches <- regmatches(path, regexec('^/session/([0-9a-f]+)(/.*)$', path))
  if (length(matches[[1]]) == 0)
    return(NULL)
  
  session <- matches[[1]][2]
  subpath <- matches[[1]][3]
  
  shinyapp <- appsByToken$get(session)
  if (is.null(shinyapp))
    return(NULL)
  
  return(shinyapp$handleRequest(ws, header, subpath))
}

dynamicHandler <- function(filePath, dependencyFiles=filePath) {
  lastKnownTimestamps <- NA
  metaHandler <- function(ws, header) NULL
  
  if (!file.exists(filePath))
    return(metaHandler)
  
  cacheContext <- CacheContext$new()
  
  return (function(ws, header) {
    # Check if we need to rebuild
    if (cacheContext$isDirty()) {
      cacheContext$reset()
      for (dep in dependencyFiles)
        cacheContext$addDependencyFile(dep)

      clearClients()
      if (file.exists(filePath)) {
        local({
          cacheContext$with(function() {
            source(filePath, local=TRUE)
          })
        })
      }
      metaHandler <<- joinHandlers(.globals$clients)
      clearClients()
    }
    
    return(metaHandler(ws, header))
  })
}

staticHandler <- function(root) {
  return(function(ws, header) {
    path <- header$RESOURCE
    
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

apps <- Map$new()
appsByToken <- Map$new()

# Provide a character representation of the WS that can be used
# as a key in a Map.
wsToKey <- function(WS) {
  as.character(WS$socket)
}

.globals <- new.env()

.globals$clients <- function(ws, header) NULL


clearClients <- function() {
  .globals$clients <- function(ws, header) NULL
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

resourcePathHandler <- function(ws, header) {
  path <- header$RESOURCE
  
  match <- regexpr('^/([^/]+)/', path, perl=TRUE)
  if (match == -1)
    return(NULL)
  len <- attr(match, 'capture.length')
  prefix <- substr(path, 2, 2 + len - 1)
  
  resInfo <- .globals$resources[[prefix]]
  if (is.null(resInfo))
    return(NULL)
  
  suffix <- substr(path, 2 + len, nchar(path))
  
  header$RESOURCE <- suffix
  
  return(resInfo$func(ws, header))
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
#'   output$uppercase <- reactiveText(function() {
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
    source(serverR, local=TRUE)
    if (is.null(.globals$server))
      stop("No server was defined in server.R")
  })
  serverFunc <- .globals$server
  
  ws_env <- create_server(
    port=port,
    webpage=httpServer(c(sessionHandler,
                         dynamicHandler(uiR),
                         wwwDir,
                         sys.www.root,
                         resourcePathHandler)))
  
  set_callback('established', function(WS, ...) {
    shinyapp <- ShinyApp$new(WS)
    apps$set(wsToKey(WS), shinyapp)
    appsByToken$set(shinyapp$token, shinyapp)
  }, ws_env)
  
  set_callback('closed', function(WS, ...) {
    shinyapp <- apps$get(wsToKey(WS))
    if (!is.null(shinyapp))
      appsByToken$remove(shinyapp$token)
    apps$remove(wsToKey(WS))
  }, ws_env)
  
  set_callback('receive', function(DATA, WS, ...) {
    if (getOption('shiny.trace', FALSE)) {
      if (as.raw(0) %in% DATA)
        message("RECV ", '$$binary data$$')
      else
        message("RECV ", rawToChar(DATA))
    }
    
    if (identical(charToRaw("\003\xe9"), DATA))
      return()
    
    shinyapp <- apps$get(wsToKey(WS))
    
    msg <- decodeMessage(DATA)

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
            stop('Unknown type specified for ', name)
          )
        }
        else if (is.list(val) && is.null(names(val)))
          msg$data[[name]] <- unlist(val, recursive=FALSE)
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
            source(serverR, local=TRUE)
            if (is.null(.globals$server))
              stop("No server was defined in server.R")
          })
          serverFunc <<- .globals$server
        }
        
        shinyapp$allowDataUriScheme <- msg$data[['__allowDataUriScheme']]
        msg$data[['__allowDataUriScheme']] <- NULL
        shinyapp$session$mset(msg$data)
        flushReact()
        local({
          serverFunc(input=.createValuesReader(shinyapp$session),
                     output=.createOutputWriter(shinyapp))
        })
      },
      update = {
        shinyapp$session$mset(msg$data)
      },
      shinyapp$dispatch(msg)
    )
    flushReact()
    shinyapp$flushOutput()
  }, ws_env)
  
  message('\n', 'Listening on port ', port)
  
  return(ws_env)
}

# NOTE: we de-roxygenized this comment because the function isn't exported
# Run an application that was created by \code{\link{startApp}}. This
# function should normally be called in a \code{while(TRUE)} loop.
# 
# @param ws_env The return value from \code{\link{startApp}}.
serviceApp <- function(ws_env) {
  if (timerCallbacks$executeElapsed()) {
    flushReact()
     lapply(apps$values(), function(shinyapp) {
       shinyapp$flushOutput()
       NULL
     })
  }

  # If this R session is interactive, then call service() with a short timeout
  # to keep the session responsive to user input
  maxTimeout <- ifelse(interactive(), 100, 5000)
  
  timeout <- max(1, min(maxTimeout, timerCallbacks$timeToNextEvent()))
  service(server=ws_env, timeout=timeout)
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
  on.exit(setwd(orig.wd))
  
  ws_env <- startApp(port=port)
  
  if (launch.browser) {
    appUrl <- paste("http://localhost:", port, sep="")
    utils::browseURL(appUrl)
  }
  
  tryCatch(
    while (TRUE) {
      serviceApp(ws_env)
    },
    finally = {
      timerCallbacks$clear()
      websocket_close(ws_env)
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
  # First, check protocol. If https, check platform:
  if (grepl('^https://', url)) {
    
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

#' Run a Shiny application from https://gist.github.com
#' 
#' Download and launch a Shiny application that is hosted on GitHub as a gist.
#' 
#' @param gist The identifier of the gist. For example, if the gist is
#'   https://gist.github.com/3239667, then \code{3239667}, \code{'3239667'}, and
#'   \code{'https://gist.github.com/3239667'} are all valid values.
#' @param port The TCP port that the application should listen on. Defaults to 
#'   port 8100.
#' @param launch.browser If true, the system's default web browser will be 
#'   launched automatically after the app is started. Defaults to true in 
#'   interactive sessions only.
#'   
#' @export
runGist <- function(gist,
                    port=8100L,
                    launch.browser=getOption('shiny.launch.browser',
                                             interactive())) {

  gistUrl <- if (is.numeric(gist) || grepl('^[0-9a-f]+$', gist)) {
    sprintf('https://gist.github.com/gists/%s/download', gist)
  } else if(grepl('^https://gist.github.com/([0-9a-f]+)$', gist)) {
    paste(sub('https://gist.github.com/',
              'https://gist.github.com/gists/',
              gist),
          '/download',
          sep='')
  } else {
    stop('Unrecognized gist identifier format')
  }
  filePath <- tempfile('shinygist', fileext='.tar.gz')
  if (download(gistUrl, filePath, mode = "wb", quiet = TRUE) != 0)
    stop("Failed to download URL ", gistUrl)
  on.exit(unlink(filePath))
  
  argsFilter <- getOption('shiny.untar.args.filter', identity)
  dirname <- do.call(untar, argsFilter(list(filePath, list=TRUE)))[1]
  do.call(untar, argsFilter(list(filePath, exdir=dirname(filePath))))
  
  appdir <- file.path(dirname(filePath), dirname)
  on.exit(unlink(appdir, recursive = TRUE))
  
  runApp(appdir, port=port, launch.browser=launch.browser)
}
