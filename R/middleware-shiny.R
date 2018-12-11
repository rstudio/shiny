#' @include globals.R
NULL

reactLogHandler <- function(req) {
  if (! rLog$isLogging()) {
    return(NULL)
  }

  if (identical(req$PATH_INFO, "/reactlog/mark")) {
    sessionToken <- parseQueryString(req$QUERY_STRING)$s
    shinysession <- appsByToken$get(sessionToken)

    # log time
    withReactiveDomain(shinysession, {
      rLog$markTime(getDefaultReactiveDomain())
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

    # add asset path after reactlog has been calculated (makes sure package exists)
    reactlog::reactlog_add_shiny_resource_paths()

    return(httpResponse(
      status = 200,
      content = list(
        file = reactlogFile,
        owned = TRUE
      )
    ))

  } else {
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
