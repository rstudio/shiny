#' @include globals.R
NULL

reactLogHandler <- function(req) {
  if (! rLog$isLogging()) {
    if (
      identical(req$PATH_INFO, "/reactlog/mark") ||
      identical(req$PATH_INFO, "/reactlog")
    ) {
      # is not logging, but is a reactlog path...

      return(
        httpResponse(
          # Not Implemented
          # - The server either does not recognize the request method, or it lacks the ability to fulfil the request.
          status = 501,
          content_type = "text/plain; charset=utf-8",
          content = "To enable reactlog, set the following option before running the application: \n\noptions(shiny.reactlog = TRUE)"
        )
      )

    } else {
      # continue on like normal
      return(NULL)
    }

  }

  if (identical(req$PATH_INFO, "/reactlog/mark")) {
    sessionToken <- parseQueryString(req$QUERY_STRING)$s
    shinysession <- appsByToken$get(sessionToken)

    # log time
    withReactiveDomain(shinysession, {
      rLog$userMark(getDefaultReactiveDomain())
    })

    return(httpResponse(
      status = 200,
      content = "marked",
      content_type = "text/plain"
    ))

  } else if (identical(req$PATH_INFO, "/reactlog")){

    sessionToken <- parseQueryString(req$QUERY_STRING)$s

    # `renderReactLog` will check/throw if reactlog doesn't exist
    reactlogFile <- renderReactlog(sessionToken)

    return(httpResponse(
      status = 200,
      content = list(
        file = reactlogFile,
        owned = TRUE
      )
    ))

  } else {
    # continue on like normal
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

  withReactiveDomain(shinysession, {
    shinysession$handleRequest(subreq)
  })
}
