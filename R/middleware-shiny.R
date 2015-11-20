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
