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

shinyapp <- NULL

startApp <- function(app, www.root, sys.www.root=NULL, port=8101L) {
  
  ws_env <- create_server(port=port, webpage=statics(www.root, sys.www.root))
  
  set_callback('established', function(WS, ...) {
    shinyapp <<- ShinyApp$new(WS)
  }, ws_env)
  
   set_callback('closed', function(WS, ...) {
   }, ws_env)
  
  set_callback('receive', function(DATA, WS, ...) {
    # cat(c("RECV", rawToChar(DATA), "\n"))
    
    if (identical(charToRaw("\003\xe9"), DATA))
      return()
    
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
            app()
          else if (is.character(app))
            source(app, local=T)
          else
            warning("Don't know how to configure app; it's neither a function or filename!")
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

runApp <- function(ws_env) {
  while (T)
    service(server=ws_env)
}
