#' @useDynLib shiny
NULL

suppressPackageStartupMessages({
  library(websockets)
  library(RJSONIO)
})

ShinyApp <- setRefClass(
  'ShinyApp',
  fields = list(
    .websocket = 'list',
    .outputs = 'Map',
    .invalidatedOutputValues = 'Map',
    .invalidatedOutputErrors = 'Map',
    .progressKeys = 'character',
    session = 'Values'
  ),
  methods = list(
    initialize = function(ws) {
      .websocket <<- ws
      .outputs <<- Map$new()
      .invalidatedOutputValues <<- Map$new()
      .invalidatedOutputErrors <<- Map$new()
      .progressKeys <<- character(0)
      session <<- Values$new()
    },
    defineOutput = function(name, func) {
      "Binds an output generating function to this name. The function can either
      take no parameters, or have named parameters for \\code{name} and
      \\code{shinyapp} (in the future this list may expand, so it is a good idea
      to also include \\code{...} in your function signature)."
      if (is.function(func)) {
        if (length(formals(func)) != 0) {
          orig <- func
          func <- function() {
            orig(name=name, shinyapp=.self)
          }
        }
        .outputs$set(name, func)
      }
      else {
        stop(paste("Unexpected", class(func), "output for", name))
      }
    },
    instantiateOutputs = function() {
      lapply(.outputs$keys(),
             function(key) {
               func <- .outputs$remove(key)
               obs <- Observer$new(function() {
                 
                 value <- try(func(), silent=F)

                 .invalidatedOutputErrors$remove(key)
                 .invalidatedOutputValues$remove(key)
                 
                 if (identical(class(value), 'try-error')) {
                   cond <- attr(value, 'condition')
                   .invalidatedOutputErrors$set(
                     key, 
                     list(message=cond$message,
                          call=capture.output(print(cond$call))))
                 }
                 else
                   .invalidatedOutputValues$set(key, value)
               })
               
               obs$onInvalidateHint(function() {
                 showProgress(key)
               })
             })
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

      if (getOption('shiny.trace', F))
        message("SEND ", json)

      websocket_write(json, .websocket)
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
      
      if (getOption('shiny.trace', F))
        message("SEND ", json)
      
      websocket_write(json, .websocket)
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
  abs.path <- normalizePath(abs.path, mustWork=T)
  if (nchar(abs.path) <= nchar(dir) + 1)
    return(NULL)
  if (substr(abs.path, 1, nchar(dir)) != dir ||
    !(substr(abs.path, nchar(dir)+1, nchar(dir)+1) %in% c('/', '\\'))) {
    return(NULL)
  }
  return(abs.path)
}

httpServer <- function(handlers) {
  handler <- joinHandlers(handlers)
  function(ws, header) {
    response <- handler(ws, header)
    if (!is.null(response))
      return(response)
    else
      return(http_response(ws, 404, content="<h1>Not Found</h1>"))
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

dynamicHandler <- function(filePath, dependencyFiles=filePath) {
  lastKnownTimestamps <- NA
  metaHandler <- function(ws, header) NULL
  
  return (function(ws, header) {
    # Check if we need to rebuild
    mtime <- file.info(dependencyFiles)$mtime
    if (!identical(lastKnownTimestamps, mtime)) {
      lastKnownTimestamps <<- mtime
      clearClients()
      if (file.exists(filePath)) {
        local({
          source(filePath, local=T)
        })
      }
      metaHandler <<- joinHandlers(.clients)
      clearClients()
    }
    
    return(metaHandler(ws, header))
  })
}

staticHandler <- function(root) {
  return(function(ws, header) {
    path <- header$RESOURCE
    
    if (is.null(path))
      return(http_response(ws, 400, content="<h1>Bad Request</h1>"))
    
    if (path == '/')
      path <- '/index.html'
    
    abs.path <- resolve(root, path)
    if (is.null(abs.path))
      return(NULL)
    
    ext <- tools::file_ext(abs.path)
    content.type <- switch(ext,
                           html='text/html; charset=UTF-8',
                           htm='text/html; charset=UTF-8',
                           js='text/javascript',
                           css='text/css',
                           png='image/png',
                           jpg='image/jpeg',
                           jpeg='image/jpeg',
                           gif='image/gif',
                           'application/octet-stream')
    response.content <- readBin(abs.path, 'raw', n=file.info(abs.path)$size)
    return(http_response(ws, 200, content.type, response.content))
  })
}

apps <- Map$new()

# Provide a character representation of the WS that can be used
# as a key in a Map.
wsToKey <- function(WS) {
  as.character(WS$socket)
}

.clients <- function(ws, header) NULL
#' @export
clearClients <- function() {
  unlockBinding('.clients', environment(clearClients))
  .clients <<- NULL
}
#' @export
registerClient <- function(client) {
  unlockBinding('.clients', environment(registerClient))
  .clients <<- append(.clients, client)
}

.server <- NULL
#' @export
shinyServer <- function(func) {
  unlockBinding('.server', environment(shinyServer))
  .server <<- func
}

#' Instantiates the app in the current working directory.
#' 
#' @param port The TCP port that the application should listen on.
startApp <- function(port=8101L) {

  sys.www.root <- system.file('www', package='shiny')
  
  globalR <- file.path(getwd(), 'global.R')
  uiR <- file.path(getwd(), 'ui.R')
  serverR <- file.path(getwd(), 'server.R')
  wwwDir <- file.path(getwd(), 'www')
  
  if (!file.exists(uiR) && !file.exists(wwwDir))
    stop(paste("Neither ui.R nor a www subdirectory was found in", getwd()))
  if (!file.exists(serverR))
    stop(paste("server.R file was not found in", getwd()))
  
  if (file.exists(globalR))
    source(globalR, local=F)
  
  shinyServer(NULL)
  serverFileTimestamp <- NULL
  local({
    serverFileTimestamp <<- file.info(serverR)$mtime
    source(serverR, local=T)
    if (is.null(.server))
      stop("No server was defined in server.R")
  })
  serverFunc <- .server
  
  ws_env <- create_server(
    port=port,
    webpage=httpServer(c(dynamicHandler(uiR), wwwDir, sys.www.root)))
  
  set_callback('established', function(WS, ...) {
    shinyapp <- ShinyApp$new(WS)
    apps$set(wsToKey(WS), shinyapp)
  }, ws_env)
  
  set_callback('closed', function(WS, ...) {
    apps$remove(wsToKey(WS))
  }, ws_env)
  
  set_callback('receive', function(DATA, WS, ...) {
    if (getOption('shiny.trace', F))
      message("RECV ", rawToChar(DATA))
    
    if (identical(charToRaw("\003\xe9"), DATA))
      return()
    
    shinyapp <- apps$get(wsToKey(WS))
    
    msg <- fromJSON(rawToChar(DATA), asText=T, simplify=F)
    switch(
      msg$method,
      init = {
        
        # Check if server.R has changed, and if so, reload
        mtime <- file.info(serverR)$mtime
        if (!identical(mtime, serverFileTimestamp)) {
          shinyServer(NULL)
          local({
            serverFileTimestamp <<- mtime
            source(serverR, local=T)
            if (is.null(.server))
              stop("No server was defined in server.R")
          })
          serverFunc <<- .server
        }
        
        shinyapp$session$mset(msg$data)
        flushReact()
        local({
          serverFunc(input=.createValuesReader(shinyapp$session),
                     output=.createOutputWriter(shinyapp))
        })
        shinyapp$instantiateOutputs()
      },
      update = {
        shinyapp$session$mset(msg$data)
      })
    flushReact()
    shinyapp$flushOutput()
  }, ws_env)
  
  message('\n', 'Listening on port ', port)
  
  return(ws_env)
}

#' Run an application that was created by \code{\link{startApp}}. This
#' function should normally be called in a \code{while(T)} loop.
#' 
#' @param ws_env The return value from \code{\link{startApp}}.
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

#' Run an application. This function normally does not return.
#' 
#' @param client Path to the root of the application-specific www files (which
#'   should include index.html).
#' @param server If a character string, a path to the R file that contains the 
#'   server application logic. If a function, the actual server application 
#'   logic (should take \code{input} and \code{output} parameters).
#' @param sys.www.root Path to the system www root, that is, the assets that are
#'   shared by all Shiny applications (shiny.css, shiny.js, etc.).
#' @param port The TCP port that the application should listen on.
#' @param launch.browser If true, the system's default web browser will be 
#'   launched automatically after the app is started. Defaults to true in 
#'   interactive sessions only.
#'   
#' @export
runApp <- function(appDir=getwd(),
                   port=8100L,
                   launch.browser=interactive()) {

  orig.wd <- getwd()
  setwd(appDir)
  on.exit(setwd(orig.wd))
  
  ws_env <- startApp(port=port)
  
  if (launch.browser) {
    appUrl <- paste("http://localhost:", port, sep="")
    utils::browseURL(appUrl)
  }
  
  tryCatch(
    while (T) {
      serviceApp(ws_env)
    },
    finally = {
      websocket_close(ws_env)
    }
  )
}

#' @export
runExample <- function(example=NA, port=8100L, launch.browser=interactive()) {
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
