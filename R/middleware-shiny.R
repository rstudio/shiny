#' @include globals.R
NULL

reactLogHandler <- function(req) {
  if (!identical(req$PATH_INFO, '/reactlog'))
    return(NULL)

  if (!isTRUE(getOption('shiny.reactlog'))) {
    return(NULL)
  }

  sessionToken <- parseQueryString(req$QUERY_STRING)$s

  return(httpResponse(
    status=200,
    content=list(file=renderReactLog(sessionToken), owned=TRUE)
  ))
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

  withReactiveDomain(shinysession, {
    shinysession$handleRequest(subreq)
  })
}

apiHandler <- function(serverFuncSource) {
  function(req) {
    path <- req$PATH_INFO
    if (is.null(path))
      return(NULL)

    matches <- regmatches(path, regexec('^/api/(.*)$', path))
    if (length(matches[[1]]) == 0)
      return(NULL)

    apiName <- matches[[1]][2]

    sharedSecret <- getOption('shiny.sharedSecret')
    if (!is.null(sharedSecret)
      && !identical(sharedSecret, req$HTTP_SHINY_SHARED_SECRET)) {
      stop("Incorrect shared secret")
    }

    if (!is.null(getOption("shiny.observer.error", NULL))) {
      warning(
        call. = FALSE,
        "options(shiny.observer.error) is no longer supported; please unset it!"
      )
      stopApp()
    }

    # need to give a fake websocket to the session
    ws <- list(
      request = req,
      sendMessage = function(...) {
        #print(list(...))
      }
    )

    inputVals <- parseQueryString(req$QUERY_STRING)

    shinysession <- ShinySession$new(ws)
    appsByToken$set(shinysession$token, shinysession)
    shinysession$setShowcase(.globals$showcaseDefault)

    serverFunc <- withReactiveDomain(NULL, serverFuncSource())

    tryCatch({
      withReactiveDomain(shinysession, {
        shinysession$manageInputs(inputVals)
        do.call(serverFunc, argsForServerFunc(serverFunc, shinysession))
        result <- NULL
        shinysession$enableApi(apiName, function(value) {
          result <<- try(value, silent = TRUE)
        })
        flushReact()
        if (inherits(result, "try-error")) {
          return(httpResponse(
            status=500,
            content_type="text/plain",
            content=conditionMessage(attr(result, "condition"))
          ))
        } else {
          if (!is.null(attr(result, "content.type"))) {
            return(httpResponse(
              status=200L,
              content_type=attr(result, "content.type"),
              content=result
            ))
          }
          return(httpResponse(
            status=200,
            content_type="application/json",
            content=toJSON(result, pretty=TRUE)
          ))
        }
      })
    }, error = function(e) {
      return(httpResponse(
        status=500,
        content=htmlEscape(conditionMessage(e))
      ))
    })
  }
}

apiWsHandler <- function(serverFuncSource) {
  function(ws) {
    path <- ws$request$PATH_INFO
    if (is.null(path))
      return(NULL)

    matches <- regmatches(path, regexec('^/api/(.*)$', path))
    if (length(matches[[1]]) == 0)
      return(NULL)

    apiName <- matches[[1]][2]

    sharedSecret <- getOption('shiny.sharedSecret')
    if (!is.null(sharedSecret)
      && !identical(sharedSecret, ws$request$HTTP_SHINY_SHARED_SECRET)) {
      ws$close()
      return(TRUE)
    }

    if (!is.null(getOption("shiny.observer.error", NULL))) {
      warning(
        call. = FALSE,
        "options(shiny.observer.error) is no longer supported; please unset it!"
      )
      stopApp()
    }

    inputVals <- parseQueryString(ws$request$QUERY_STRING)

    # Give a fake websocket to suppress messages from session
    shinysession <- ShinySession$new(list(
      request = ws$request,
      sendMessage = function(...) {
        #print(list(...))
      }
    ))
    appsByToken$set(shinysession$token, shinysession)
    shinysession$setShowcase(.globals$showcaseDefault)

    serverFunc <- withReactiveDomain(NULL, serverFuncSource())

    tryCatch({
      withReactiveDomain(shinysession, {
        shinysession$manageInputs(inputVals)
        do.call(serverFunc, argsForServerFunc(serverFunc, shinysession))
        result <- NULL
        shinysession$enableApi(apiName, function(value) {
          try(ws$send(toJSON(value, pretty=TRUE)), silent=TRUE)
        })
        flushReact()
      })
    }, error = function(e) {
      ws$close()
    })

    # TODO: Handle ws$onClose()
    # TODO: What to do on ws$onMessage?
  }
}
