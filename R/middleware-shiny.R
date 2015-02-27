#' @include globals.R
NULL

reactLogHandler <- function(req) {
  if (!identical(req$PATH_INFO, '/reactlog'))
    return(NULL)

  if (!isTRUE(getOption('shiny.reactlog'))) {
    return(NULL)
  }

  return(httpResponse(
    status=200,
    content=list(file=renderReactLog(), owned=TRUE)
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

dynamicHandler <- function(filePath, dependencyFiles=filePath) {
  lastKnownTimestamps <- NA
  metaHandler <- function(req) NULL

  if (!file.exists(filePath))
    return(metaHandler)

  cacheContext <- CacheContext$new()

  return (function(req) {
    # Check if we need to rebuild
    if (cacheContext$isDirty()) {
      cacheContext$reset()
      for (dep in dependencyFiles)
        cacheContext$addDependencyFile(dep)

      clearClients()
      if (file.exists(filePath)) {
        local({
          cacheContext$with(function() {
            sys.source(filePath, envir=new.env(parent=globalenv()), keep.source=TRUE)
          })
        })
      }
      metaHandler <<- joinHandlers(.globals$clients)
      clearClients()
    }

    return(metaHandler(req))
  })
}
