library(websockets)
library(RJSONIO)
library(caTools)
library(xtable)

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
      cat(c("SEND", toJSON(as.list(data)), "\n"))
      websocket_write(toJSON(as.list(data)), .websocket)
    }
  )
)

statics <- function(root) {
  root <- normalizePath(root, mustWork=T)
  
  return(function(ws, header) {
    # TODO: Stop using websockets' internal methods
    path <- header$RESOURCE
    
    if (is.null(path))
      return(websockets:::.http_400(ws))
    
    if (path == '/')
      path <- '/index.html'
    
    abs.path <- file.path(root, path)
    
    if (!file.exists(abs.path)) {
      # TODO: This should be 404, not 400
      return(websockets:::.http_400(ws))
    }
    
    abs.path <- normalizePath(abs.path, mustWork=T)
    
    if (nchar(abs.path) <= nchar(root) + 1) {
      return(websockets:::.http_400(ws))
    }
    
    if (substr(abs.path, 1, nchar(root)) != root ||
        !(substr(abs.path, nchar(root)+1, nchar(root)+1) %in% c('/', '\\'))) {
      return(websockets:::.http_400(ws))
    }
    
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
    return(websockets:::.http_200(ws, content.type, response.content))
  })
}

start.app <- function(port = 8101L) {
  
  ws_env <- create_server(port=port, webpage=statics('./www'))
  
  set_callback('established', function(WS, ...) {
    shinyapp <<- ShinyApp$new(WS)
    
    input <- Observable$new(function() {
      str <- shinyapp$session$get('input1')
      if (shinyapp$session$get('addnewline'))
        str <- paste(str, "\n", sep='')
      return(str)
    })
    input.df <- Observable$new(function() {
      varname <- shinyapp$session$get('input1')
      if (nchar(varname) > 0 && exists(varname, where=.GlobalEnv)) {
        df <- get(varname, pos=.GlobalEnv)
        if (is.data.frame(df)) {
          return(df)
        }
      }
      return(NULL)
    })
    shinyapp$define.output('md5_hash', function() {
      digest(input$get.value(), algo='md5', serialize=F)
    })
    shinyapp$define.output('sha1_hash', function() {
      digest(input$get.value(), algo='sha1', serialize=F)
    })
    shinyapp$define.output('table1', function() {
      if (!is.null(input.df$get.value()))
        print(xtable(input.df$get.value()), type='html')
    })
    shinyapp$define.plot.output('plot1', function() {
      if (!is.null(input.df$get.value()))
        plot(input.df$get.value())
    }, width=800, height=600)
    
  }, ws_env)
  
   set_callback('closed', function(WS, ...) {
   }, ws_env)
  
  set_callback('receive', function(DATA, WS, ...) {
    cat(c("RECV", rawToChar(DATA), "\n"))
    
    if (identical(charToRaw("\003\xe9"), DATA))
      return()
    
    msg <- fromJSON(rawToChar(DATA), asText=T, simplify=F)
    switch(
      msg$method,
      init = {
        shinyapp$session$mset(msg$data)
        flush.react()
        shinyapp$instantiate.outputs()
      },
      update = {
        shinyapp$session$mset(msg$data)
      })
    flush.react()
    shinyapp$flush.output()
  }, ws_env)
  
  return(ws_env)
}

run.app <- function(ws_env) {
  while (T)
    service(server=ws_env)
}
