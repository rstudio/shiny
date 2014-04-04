
## ------------------------------------------------------------------------
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


## ------------------------------------------------------------------------
joinHandlers <- function(handlers) {
  # Zero handlers; return a null handler
  if (length(handlers) == 0)
    return(function(req) NULL)

  # Just one handler (function)? Return it.
  if (is.function(handlers))
    return(handlers)

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


## ------------------------------------------------------------------------
routeHandler <- function(prefix, handler) {
  force(prefix)
  force(handler)

  if (identical("", prefix))
    return(handler)

  if (length(prefix) != 1 || !isTRUE(grepl("^/[^\\]+$", prefix))) {
    stop("Invalid URL prefix \"", prefix, "\"")
  }

  pathPattern <- paste("^\\Q", prefix, "\\E/", sep = "")
  function(req) {
    if (isTRUE(grepl(pathPattern, req$PATH_INFO))) {
      origScript <- req$SCRIPT_NAME
      origPath <- req$PATH_INFO
      on.exit({
        req$SCRIPT_NAME <- origScript
        req$PATH_INFO <- origPath
      }, add = TRUE)
      pathInfo <- substr(req$PATH_INFO, nchar(prefix)+1, nchar(req$PATH_INFO))
      req$SCRIPT_NAME <- paste(req$SCRIPT_NAME, prefix, sep = "")
      req$PATH_INFO <- pathInfo
      return(handler(req))
    } else {
      return(NULL)
    }
  }
}


## ------------------------------------------------------------------------
routeWSHandler <- function(prefix, wshandler) {
  force(prefix)
  force(wshandler)

  if (identical("", prefix))
    return(wshandler)

  if (length(prefix) != 1 || !isTRUE(grepl("^/[^\\]+$", prefix))) {
    stop("Invalid URL prefix \"", prefix, "\"")
  }

  pathPattern <- paste("^\\Q", prefix, "\\E/", sep = "")
  function(ws) {
    req <- ws$request
    if (isTRUE(grepl(pathPattern, req$PATH_INFO))) {
      origScript <- req$SCRIPT_NAME
      origPath <- req$PATH_INFO
      on.exit({
        req$SCRIPT_NAME <- origScript
        req$PATH_INFO <- origPath
      }, add = TRUE)
      pathInfo <- substr(req$PATH_INFO, nchar(prefix)+1, nchar(req$PATH_INFO))
      req$SCRIPT_NAME <- paste(req$SCRIPT_NAME, prefix, sep = "")
      req$PATH_INFO <- pathInfo
      return(wshandler(ws))
    } else {
      return(NULL)
    }
  }
}


## ------------------------------------------------------------------------
staticHandler <- function(root) {
  force(root)
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


## ------------------------------------------------------------------------
HandlerManager <- setRefClass("HandlerManager",
  fields = list(
    handlers = "list",
    wsHandlers = "list"
  ),
  methods = list(
    addHandler = function(handler) {
      if (length(handlers) == 0)
        handlers <<- list(handler)
      else
        handlers <<- c(handlers, list(handler))
    },
    addWSHandler = function(wsHandler) {
      if (length(wsHandlers) == 0)
        wsHandlers <<- list(wsHandler)
      else
        wsHandlers <<- c(wsHandlers, list(wsHandler))
    },
    clear = function() {
      handlers <<- list()
      wsHandlers <<- list()
    },
    createHttpuvApp = function() {
      list(
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
        call = .httpServer(
          function (req) {
            for (handler in handlers) {
              result <- handler(req)
              if (!is.null(result))
                return(result)
            }
            return(NULL)
          },
          getOption('shiny.sharedSecret', NULL)
        ),
        onWSOpen = function(ws) {
          for (wsHandler in wsHandlers) {
            result <- wsHandler(ws)
            if (!is.null(result))
              return(result)
          }
          return(NULL)
        }
      )
    },
    .httpServer = function(handler, sharedSecret) {
      filter <- getOption('shiny.http.response.filter', NULL)
      if (is.null(filter))
        filter <- function(req, response) response

      function(req) {
        if (!is.null(sharedSecret)
          && !identical(sharedSecret, req$HTTP_SHINY_SHARED_SECRET)) {
          return(list(status=403,
            body='<h1>403 Forbidden</h1><p>Shared secret mismatch</p>',
            headers=list('Content-Type' = 'text/html')))
        }

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
  )
)


