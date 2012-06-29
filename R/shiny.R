suppressPackageStartupMessages({
  library(websockets)
  library(RJSONIO)
  library(caTools)
  library(xtable)
})

ShinyApp <- setRefClass(
  'ShinyApp',
  fields = list(
    .websocket = 'list',
    .outputs = 'Map',
    .invalidated.output.values = 'Map',
    session = 'Values'
  ),
  methods = list(
    initialize = function(ws) {
      .websocket <<- ws
      .outputs <<- Map$new()
      .invalidated.output.values <<- Map$new()
      session <<- Values$new()
    },
    define.output = function(name, func) {
      .outputs$set(name, func)
    },
    define.plot.output = function(name, func, ...) {
      .outputs$set(name, function() {
        png.file <- tempfile(fileext='.png')
        png(filename=png.file, ...)
        func()
        dev.off()

        bytes <- file.info(png.file)$size
        if (is.na(bytes))
          return(NULL)
        
        b64 <- base64encode(readBin(png.file, 'raw', n=bytes))
        return(paste("data:image/png;base64,", b64, sep=''))
      })
    },
    define.table.output = function(name, func, ...) {
      .outputs$set(name, function() {
        data <- func()
        return(paste(
          capture.output(
            print(xtable(data, ...), 
                  type='html', 
                  html.table.attributes='class="data"')),
          collapse="\n"))
      })
    },
    instantiate.outputs = function() {
      lapply(.outputs$keys(),
             function(key) {
               func <- .outputs$remove(key)
               Observer$new(function() {
                 value <- func()
                 .invalidated.output.values$set(key, value)
               })
             })
    },
    flush.output = function() {
      if (length(.invalidated.output.values) == 0)
        return(invisible())
      
      data <- .invalidated.output.values
      .invalidated.output.values <<- Map$new()
      # cat(c("SEND", toJSON(as.list(data)), "\n"))
      websocket_write(toJSON(as.list(data)), .websocket)
    }
  )
)

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

start.app <- function(app, www.root, sys.www.root=NULL, port=8101L) {
  
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
        flush.react()
        local({
          define.output <- function(name, func) {
            shinyapp$define.output(name, func)
          }
          define.plot <- function(name, func, ...) {
            shinyapp$define.plot.output(name, func, ...)
          }
          define.table <- function(name, func, ...) {
            shinyapp$define.table.output(name, func, ...)
          }
          get.input <- function(name) {
            shinyapp$session$get(name)
          }
          
          if (is.function(app))
            app()
          else if (is.character(app))
            source(app, local=T)
          else
            warning("Don't know how to configure app; it's neither a function or filename!")
        })
        shinyapp$instantiate.outputs()
      },
      update = {
        shinyapp$session$mset(msg$data)
      })
    flush.react()
    shinyapp$flush.output()
  }, ws_env)
  
  cat(paste('Listening on http://0.0.0.0:', port, "\n", sep=''))
  
  return(ws_env)
}

run.app <- function(ws_env) {
  while (T)
    service(server=ws_env)
}
