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
    session = 'Values'
  ),
  methods = list(
    initialize = function(ws) {
      .websocket <<- ws
      .outputs <<- Map$new()
      .invalidatedOutputValues <<- Map$new()
      session <<- Values$new()
    },
    defineOutput = function(name, func) {
      .outputs$set(name, func)
    },
    instantiateOutputs = function() {
      lapply(.outputs$keys(),
             function(key) {
               func <- .outputs$remove(key)
               Observer$new(function() {
                 value <- func()
                 .invalidatedOutputValues$set(key, value)
               })
             })
    },
    flushOutput = function() {
      if (length(.invalidatedOutputValues) == 0)
        return(invisible())
      
      data <- .invalidatedOutputValues
      .invalidatedOutputValues <<- Map$new()
      # cat(c("SEND", toJSON(as.list(data)), "\n"))
      websocket_write(toJSON(as.list(data)), .websocket)
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

statics <- function(root, sys.root=NULL) {
  root <- normalizePath(root, mustWork=T)
  if (!is.null(sys.root))
    sys.root <- normalizePath(sys.root, mustWork=T)
  
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
  
  return(function(ws, header) {
    path <- header$RESOURCE
    
    if (is.null(path))
      return(http_response(ws, 400, content="<h1>Bad Request</h1>"))
    
    if (path == '/')
      path <- '/index.html'
    
    abs.path <- resolve(root, path)
    if (is.null(abs.path) && !is.null(sys.root))
      abs.path <- resolve(sys.root, path)
    if (is.null(abs.path))
      return(http_response(ws, 404, content="<h1>Not Found</h1>"))
    
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

#' Creates a new app with the given properties.
#' 
#' @param app If a character string, a path to the R file that contains the 
#'   server application logic. If a function, the actual server application 
#'   logic (should take \code{input} and \code{output} parameters).
#' @param www.root Path to the root of the application-specific www files (which
#'   should include index.html).
#' @param sys.www.root Path to the system www root, that is, the assets that are
#'   shared by all Shiny applications (shiny.css, shiny.js, etc.).
#' @param port The TCP port that the application should listen on.
startApp <- function(app = './app.R',
                     www.root = './www',
                     sys.www.root = system.file('www',
                                                package='shiny'),
                     port=8101L) {
  
  ws_env <- create_server(port=port, webpage=statics(www.root, sys.www.root))
  
  set_callback('established', function(WS, ...) {
    shinyapp <- ShinyApp$new(WS)
    apps$set(wsToKey(WS), shinyapp)
  }, ws_env)
  
  set_callback('closed', function(WS, ...) {
    apps$remove(wsToKey(WS))
  }, ws_env)
  
  set_callback('receive', function(DATA, WS, ...) {
    # cat(c("RECV", rawToChar(DATA), "\n"))
    
    if (identical(charToRaw("\003\xe9"), DATA))
      return()
    
    shinyapp <- apps$get(wsToKey(WS))
    
    msg <- fromJSON(rawToChar(DATA), asText=T, simplify=F)
    switch(
      msg$method,
      init = {
        shinyapp$session$mset(msg$data)
        flushReact()
        local({
          input <- .createValuesReader(shinyapp$session)
          output <- .createOutputWriter(shinyapp)
          
          if (is.function(app))
            app(input=input, output=output)
          else if (is.character(app))
            source(app, local=T)
          else
            warning("app must be a function or filename")
        })
        shinyapp$instantiateOutputs()
      },
      update = {
        shinyapp$session$mset(msg$data)
      })
    flushReact()
    shinyapp$flushOutput()
  }, ws_env)
  
  cat(paste('Listening on http://0.0.0.0:', port, "\n", sep=''))
  
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
#' @param app If a character string, a path to the R file that contains the 
#'   server application logic. If a function, the actual server application 
#'   logic (should take \code{input} and \code{output} parameters).
#' @param www.root Path to the root of the application-specific www files (which
#'   should include index.html).
#' @param sys.www.root Path to the system www root, that is, the assets that are
#'   shared by all Shiny applications (shiny.css, shiny.js, etc.).
#' @param port The TCP port that the application should listen on.
#' 
#' @export
runApp <- function(app = './app.R',
                   www.root = './www',
                   sys.www.root = system.file('www',
                                              package='shiny'),
                   port=8101L) {
  ws_env <- startApp(app=app, 
                     www.root=www.root, 
                     sys.www.root=sys.www.root,
                     port=port)
  
  while (T) {
    serviceApp(ws_env)
  }
}



#' Run an application in a new process and open it in a web browser
#' @param appDir Application directory
#' @param port The TCP port that the application should listen on
#' @return The URL used to access the application.
#' @export
launchApp <- function(appDir, port=8101L) {
  # manage working directory
  currentWd <- getwd()
  on.exit(setwd(currentWd))
  setwd(appDir)
  
  # launch an instance of R to run the app
  code <- paste("library(shiny); runApp(port=", port, ");", sep = "")
  runCmd <- paste("R --slave --vanilla -e", shQuote(code))
  system(runCmd, wait=FALSE)
  
  # wait to ensure the port is bound, then launch a browser
  Sys.sleep(0.5)
  appUrl <- paste("http://localhost:", port, sep="")
  utils::browseURL(appUrl)
  
  # return the url of the app (invisibliy)
  invisible(appUrl)
}

