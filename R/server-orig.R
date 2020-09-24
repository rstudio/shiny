#' @include server-input-handlers.R

appsByToken <- Map$new()
appsNeedingFlush <- Map$new()

# Provide a character representation of the WS that can be used
# as a key in a Map.
wsToKey <- function(WS) {
  as.character(WS$socket)
}

.globals$clients <- function(req) NULL


clearClients <- function() {
  .globals$clients <- function(req) NULL
}


registerClient <- function(client) {
  .globals$clients <- append(.globals$clients, client)
}


.globals$resourcePaths <- list()
.globals$resources <- list()

.globals$showcaseDefault <- 0

.globals$showcaseOverride <- FALSE

#' Resource Publishing
#'
#' Add, remove, or list directory of static resources to Shiny's web server,
#' with the given path prefix. Primarily intended for package authors to make
#' supporting JavaScript/CSS files available to their components.
#'
#' Shiny provides two ways of serving static files (i.e., resources):
#'
#' 1. Static files under the `www/` directory are automatically made available
#' under a request path that begins with `/`.
#' 2. `addResourcePath()` makes static files in a `directoryPath` available
#' under a request path that begins with `prefix`.
#'
#' The second approach is primarily intended for package authors to make
#' supporting JavaScript/CSS files available to their components.
#'
#' Tools for managing static resources published by Shiny's web server:
#'  * `addResourcePath()` adds a directory of static resources.
#'  * `resourcePaths()` lists the currently active resource mappings.
#'  * `removeResourcePath()` removes a directory of static resources.
#'
#' @param prefix The URL prefix (without slashes). Valid characters are a-z,
#'   A-Z, 0-9, hyphen, period, and underscore. For example, a value of 'foo'
#'   means that any request paths that begin with '/foo' will be mapped to the
#'   given directory.
#' @param directoryPath The directory that contains the static resources to be
#'   served.
#'
#' @rdname resourcePaths
#' @seealso [singleton()]
#'
#' @examples
#' addResourcePath('datasets', system.file('data', package='datasets'))
#' resourcePaths()
#' removeResourcePath('datasets')
#' resourcePaths()
#'
#' # make sure all resources are removed
#' lapply(names(resourcePaths()), removeResourcePath)
#' @export
addResourcePath <- function(prefix, directoryPath) {
  if (length(prefix) != 1) stop("prefix must be of length 1")
  if (grepl("^\\.+$", prefix)) stop("prefix can't be composed of dots only")
  if (!grepl('[a-z0-9\\-_.]+$', prefix, ignore.case = TRUE, perl = TRUE)) {
    stop("addResourcePath called with invalid prefix; please see documentation")
  }
  if (prefix %in% c('shared')) {
    stop("addResourcePath called with the reserved prefix '", prefix, "'; ",
         "please use a different prefix")
  }
  normalizedPath <- tryCatch(normalizePath(directoryPath, mustWork = TRUE),
    error = function(e) {
      stop("Couldn't normalize path in `addResourcePath`, with arguments: ",
        "`prefix` = '", prefix, "'; `directoryPath` = '" , directoryPath, "'")
    }
  )

  # # Often times overwriting a resource path is "what you want",
  # # but sometimes it can lead to difficult to diagnose issues
  # # (e.g. an implict dependency might set a resource path that
  # # conflicts with what you, the app author, are trying to register)
  # # Note that previous versions of shiny used to warn about this case,
  # # but it was eventually removed since it caused confusion (#567).
  # # It seems a good compromise is to throw a more information message.
  # if (getOption("shiny.resourcePathChanges", FALSE) &&
  #     prefix %in% names(.globals$resourcePaths)) {
  #   existingPath <- .globals$resourcePaths[[prefix]]$path
  #   if (normalizedPath != existingPath) {
  #     message(
  #       "The resource path '", prefix, "' used to point to ",
  #       existingPath, ", but it now points to ", normalizedPath, ". ",
  #       "If your app doesn't work as expected, you may want to ",
  #       "choose a different prefix name."
  #     )
  #   }
  # }

  # If a shiny app is currently running, dynamically register this path with
  # the corresponding httpuv server object.
  if (!is.null(getShinyOption("server")))
  {
    getShinyOption("server")$setStaticPath(.list = stats::setNames(normalizedPath, prefix))
  }

  # .globals$resourcePaths and .globals$resources persist across runs of applications.
  .globals$resourcePaths[[prefix]] <- staticPath(normalizedPath)
  # This is necessary because resourcePaths is only for serving assets out of C++;
  # to support subapps, we also need assets to be served out of R, because those
  # URLs are rewritten by R code (i.e. routeHandler) before they can be matched to
  # a resource path.
  .globals$resources[[prefix]] <- list(
    directoryPath = normalizedPath,
    func = staticHandler(normalizedPath)
  )
}

#' @rdname resourcePaths
#' @export
resourcePaths <- function() {
  urls <- names(.globals$resourcePaths)
  paths <- vapply(.globals$resourcePaths, function(x) x$path, character(1))
  stats::setNames(paths, urls)
}

hasResourcePath <- function(prefix) {
  prefix %in% names(resourcePaths())
}

#' @rdname resourcePaths
#' @export
removeResourcePath <- function(prefix) {
  if (length(prefix) > 1) stop("`prefix` must be of length 1.")
  if (!hasResourcePath(prefix)) {
    warning("Resource ", prefix, " not found.")
    return(invisible(FALSE))
  }
  .globals$resourcePaths[[prefix]] <- NULL
  .globals$resources[[prefix]] <- NULL
  invisible(TRUE)
}



# This function handles any GET request with two or more path elements where the
# first path element matches a prefix that was previously added using
# addResourcePath().
#
# For example, if `addResourcePath("foo", "~/bar")` was called, then a GET
# request for /foo/one/two.html would rewrite the PATH_INFO as /one/two.html and
# send it to the resource path function for "foo". As of this writing, that
# function will always be a staticHandler, which serves up a file if it exists
# and NULL if it does not.
#
# Since Shiny 1.3.x, assets registered via addResourcePath should mostly be
# served out of httpuv's native static file serving features. However, in the
# specific case of subapps, the R code path must be used, because subapps insert
# a giant random ID into the beginning of the URL that must be stripped off by
# an R route handler (see addSubApp()).
resourcePathHandler <- function(req) {
  if (!identical(req$REQUEST_METHOD, 'GET'))
    return(NULL)

  # e.g. "/foo/one/two.html"
  path <- req$PATH_INFO

  match <- regexpr('^/([^/]+)/', path, perl=TRUE)
  if (match == -1)
    return(NULL)
  len <- attr(match, 'capture.length')
  # e.g. "foo"
  prefix <- substr(path, 2, 2 + len - 1)

  resInfo <- .globals$resources[[prefix]]
  if (is.null(resInfo))
    return(NULL)

  # e.g. "/one/two.html"
  suffix <- substr(path, 2 + len, nchar(path))

  # Create a new request that's a clone of the current request, but adjust
  # PATH_INFO and SCRIPT_NAME to reflect that we have already matched the first
  # path element (e.g. "/foo"). See routeHandler() for more info.
  subreq <- as.environment(as.list(req, all.names=TRUE))
  subreq$PATH_INFO <- suffix
  subreq$SCRIPT_NAME <- paste(subreq$SCRIPT_NAME, substr(path, 1, 2 + len), sep='')

  return(resInfo$func(subreq))
}

#' Define Server Functionality
#'
#' Defines the server-side logic of the Shiny application. This generally
#' involves creating functions that map user inputs to various kinds of output.
#' In older versions of Shiny, it was necessary to call `shinyServer()` in
#' the `server.R` file, but this is no longer required as of Shiny 0.10.
#' Now the `server.R` file may simply return the appropriate server
#' function (as the last expression in the code), without calling
#' `shinyServer()`.
#'
#' Call `shinyServer` from your application's `server.R`
#' file, passing in a "server function" that provides the server-side logic of
#' your application.
#'
#' The server function will be called when each client (web browser) first loads
#' the Shiny application's page. It must take an `input` and an
#' `output` parameter. Any return value will be ignored. It also takes an
#' optional `session` parameter, which is used when greater control is
#' needed.
#'
#' See the [tutorial](http://rstudio.github.com/shiny/tutorial/) for more
#' on how to write a server function.
#'
#' @param func The server function for this application. See the details section
#'   for more information.
#'
#' @examples
#' \dontrun{
#' # A very simple Shiny app that takes a message from the user
#' # and outputs an uppercase version of it.
#' shinyServer(function(input, output, session) {
#'   output$uppercase <- renderText({
#'     toupper(input$message)
#'   })
#' })
#'
#'
#' # It is also possible for a server.R file to simply return the function,
#' # without calling shinyServer().
#' # For example, the server.R file could contain just the following:
#' function(input, output, session) {
#'   output$uppercase <- renderText({
#'     toupper(input$message)
#'   })
#' }
#' }
#' @export
#' @keywords internal
shinyServer <- function(func) {
  .globals$server <- list(func)
  invisible(func)
}

decodeMessage <- function(data) {
  readInt <- function(pos) {
    packBits(rawToBits(data[pos:(pos+3)]), type='integer')
  }

  if (readInt(1) != 0x01020202L) {
    # Treat message as UTF-8
    charData <- rawToChar(data)
    Encoding(charData) <- 'UTF-8'
    return(safeFromJSON(charData, simplifyVector=FALSE))
  }

  i <- 5
  parts <- list()
  while (i <= length(data)) {
    length <- readInt(i)
    i <- i + 4
    if (length != 0)
      parts <- append(parts, list(data[i:(i+length-1)]))
    else
      parts <- append(parts, list(raw(0)))
    i <- i + length
  }

  mainMessage <- decodeMessage(parts[[1]])
  mainMessage$blobs <- parts[2:length(parts)]
  return(mainMessage)
}

autoReloadCallbacks <- Callbacks$new()

createAppHandlers <- function(httpHandlers, serverFuncSource) {
  appvars <- new.env()
  appvars$server <- NULL

  sys.www.root <- system.file('www', package='shiny')

  # This value, if non-NULL, must be present on all HTTP and WebSocket
  # requests as the Shiny-Shared-Secret header or else access will be
  # denied (403 response for HTTP, and instant close for websocket).
  checkSharedSecret <- loadSharedSecret()

  appHandlers <- list(
    http = joinHandlers(c(
      sessionHandler,
      httpHandlers,
      sys.www.root,
      resourcePathHandler,
      reactLogHandler
    )),
    ws = function(ws) {
      if (!checkSharedSecret(ws$request$HTTP_SHINY_SHARED_SECRET)) {
        ws$close()
        return(TRUE)
      }

      if (identical(ws$request$PATH_INFO, "/autoreload/")) {
        if (!getOption("shiny.autoreload", FALSE)) {
          ws$close()
          return(TRUE)
        }

        callbackHandle <- autoReloadCallbacks$register(function() {
          ws$send("autoreload")
          ws$close()
        })
        ws$onClose(function() {
          callbackHandle()
        })
        return(TRUE)
      }

      if (!is.null(getOption("shiny.observer.error", NULL))) {
        warning(
          call. = FALSE,
          "options(shiny.observer.error) is no longer supported; please unset it!"
        )
        stopApp()
      }

      shinysession <- ShinySession$new(ws)
      appsByToken$set(shinysession$token, shinysession)
      shinysession$setShowcase(.globals$showcaseDefault)

      messageHandler <- function(binary, msg) {
        withReactiveDomain(shinysession, {
          # To ease transition from websockets-based code. Should remove once we're stable.
          if (is.character(msg))
            msg <- charToRaw(msg)

          traceOption <- getOption('shiny.trace', FALSE)
          if (isTRUE(traceOption) || traceOption == "recv") {
            if (binary)
              message("RECV ", '$$binary data$$')
            else
              message("RECV ", rawToChar(msg))
          }

          if (isEmptyMessage(msg))
            return()

          msg <- decodeMessage(msg)

          # Set up a restore context from .clientdata_url_search before
          # handling all the input values, because the restore context may be
          # used by an input handler (like the one for "shiny.file"). This
          # should only happen once, when the app starts.
          if (is.null(shinysession$restoreContext)) {
            bookmarkStore <- getShinyOption("bookmarkStore", default = "disable")
            if (bookmarkStore == "disable") {
              # If bookmarking is disabled, use empty context
              shinysession$restoreContext <- RestoreContext$new()
            } else {
              # If there's bookmarked state, save it on the session object
              shinysession$restoreContext <- RestoreContext$new(msg$data$.clientdata_url_search)
              shinysession$createBookmarkObservers()
            }
          }


          msg$data <- applyInputHandlers(msg$data)

          switch(
            msg$method,
            init = {

              serverFunc <- withReactiveDomain(NULL, serverFuncSource())
              if (!identicalFunctionBodies(serverFunc, appvars$server)) {
                appvars$server <- serverFunc
                if (!is.null(appvars$server))
                {
                  # Tag this function as the Shiny server function. A debugger may use this
                  # tag to give this function special treatment.
                  # It's very important that it's appvars$server itself and NOT a copy that
                  # is invoked, otherwise new breakpoints won't be picked up.
                  attr(appvars$server, "shinyServerFunction") <- TRUE
                  registerDebugHook("server", appvars, "Server Function")
                }
              }

              # Check for switching into/out of showcase mode
              if (.globals$showcaseOverride &&
                  exists(".clientdata_url_search", where = msg$data)) {
                mode <- showcaseModeOfQuerystring(msg$data$.clientdata_url_search)
                if (!is.null(mode))
                  shinysession$setShowcase(mode)
              }

              # In shinysession$createBookmarkObservers() above, observers may be
              # created, which puts the shiny session in busyCount > 0 state. That
              # prevents the manageInputs here from taking immediate effect, by
              # default. The manageInputs here needs to take effect though, because
              # otherwise the bookmark observers won't find the clientData they are
              # looking for. So use `now = TRUE` to force the changes to be
              # immediate.
              #
              # FIXME: break createBookmarkObservers into two separate steps, one
              # before and one after manageInputs, and put the observer creation
              # in the latter. Then add an assertion that busyCount == 0L when
              # this manageInputs is called.
              shinysession$manageInputs(msg$data, now = TRUE)

              # The client tells us what singletons were rendered into
              # the initial page
              if (!is.null(msg$data$.clientdata_singletons)) {
                shinysession$singletons <- strsplit(
                  msg$data$.clientdata_singletons, ',')[[1]]
              }

              local({
                args <- argsForServerFunc(serverFunc, shinysession)

                withReactiveDomain(shinysession, {
                  do.call(
                    # No corresponding ..stacktraceoff; the server func is pure
                    # user code
                    wrapFunctionLabel(appvars$server, "server",
                      ..stacktraceon = TRUE
                    ),
                    args
                  )
                })
              })
            },
            update = {
              shinysession$manageInputs(msg$data)
            },
            shinysession$dispatch(msg)
          )
          # The HTTP_GUID, if it exists, is for Shiny Server reporting purposes
          shinysession$startTiming(ws$request$HTTP_GUID)
          shinysession$requestFlush()

          # Make httpuv return control to Shiny quickly, instead of waiting
          # for the usual timeout
          httpuv::interrupt()
        })
      }
      ws$onMessage(function(binary, msg) {
        # If unhandled errors occur, make sure they get properly logged
        withLogErrors(messageHandler(binary, msg))
      })

      ws$onClose(function() {
        shinysession$wsClosed()
        appsByToken$remove(shinysession$token)
        appsNeedingFlush$remove(shinysession$token)
      })

      return(TRUE)
    }
  )
  return(appHandlers)
}

# Determine what arguments should be passed to this serverFunc. All server funcs
# must take input and output, but clientData (obsolete) and session are
# optional.
argsForServerFunc <- function(serverFunc, session) {
  args <- list(input = session$input, output = .createOutputWriter(session))

  paramNames <- names(formals(serverFunc))

  # The clientData and session arguments are optional; check if
  # each exists

  if ("clientData" %in% paramNames)
    args$clientData <- session$clientData

  if ("session" %in% paramNames)
    args$session <- session

  args
}

getEffectiveBody <- function(func) {
  if (is.null(func))
    NULL
  else if (isS4(func) && class(func) == "functionWithTrace")
    body(func@original)
  else
    body(func)
}

identicalFunctionBodies <- function(a, b) {
  identical(getEffectiveBody(a), getEffectiveBody(b))
}

handlerManager <- HandlerManager$new()

addSubApp <- function(appObj, autoRemove = TRUE) {
  path <- createUniqueId(16, "/app")
  appHandlers <- createAppHandlers(appObj$httpHandler, appObj$serverFuncSource)

  # remove the leading / from the path so a relative path is returned
  # (needed for the case where the root URL for the Shiny app isn't /, such
  # as portmapped URLs)
  finalPath <- paste(
    substr(path, 2, nchar(path)),
    "/?w=", workerId(),
    "&__subapp__=1",
    sep="")
  handlerManager$addHandler(routeHandler(path, appHandlers$http), finalPath)
  handlerManager$addWSHandler(routeWSHandler(path, appHandlers$ws), finalPath)

  if (autoRemove) {
    # If a session is currently active, remove this subapp automatically when
    # the current session ends
    onReactiveDomainEnded(getDefaultReactiveDomain(), function() {
      removeSubApp(finalPath)
    })
  }

  return(finalPath)
}

removeSubApp <- function(path) {
  handlerManager$removeHandler(path)
  handlerManager$removeWSHandler(path)
}

startApp <- function(appObj, port, host, quiet) {
  appHandlers <- createAppHandlers(appObj$httpHandler, appObj$serverFuncSource)
  handlerManager$addHandler(appHandlers$http, "/", tail = TRUE)
  handlerManager$addWSHandler(appHandlers$ws, "/", tail = TRUE)

  httpuvApp <- handlerManager$createHttpuvApp()
  httpuvApp$staticPaths <- c(
    appObj$staticPaths,
    list(
      # Always handle /session URLs dynamically, even if / is a static path.
      "session" = excludeStaticPath(),
      "shared" = system.file(package = "shiny", "www", "shared")
    ),
    .globals$resourcePaths
  )

  # throw an informative warning if a subdirectory of the
  # app's www dir conflicts with another resource prefix
  wwwDir <- httpuvApp$staticPaths[["/"]]$path
  if (length(wwwDir)) {
    # although httpuv allows for resource prefixes like 'foo/bar',
    # we won't worry about conflicts in sub-sub directories since
    # addResourcePath() currently doesn't allow it
    wwwSubDirs <- list.dirs(wwwDir, recursive = FALSE, full.names = FALSE)
    resourceConflicts <- intersect(wwwSubDirs, names(httpuvApp$staticPaths))
    if (length(resourceConflicts)) {
      warning(
        "Found subdirectories of your app's www/ directory that ",
        "conflict with other resource URL prefixes. ",
        "Consider renaming these directories: '",
        paste0("www/", resourceConflicts, collapse = "', '"), "'",
        call. = FALSE
      )
    }
  }

  # check for conflicts in each pairwise combinations of resource mappings
  checkResourceConflict <- function(paths) {
    if (length(paths) < 2) return(NULL)
    # ensure paths is a named character vector: c(resource_path = local_path)
    paths <- vapply(paths, function(x) if (inherits(x, "staticPath")) x$path else x, character(1))
    # get all possible pairwise combinations of paths
    pair_indices <- utils::combn(length(paths), 2, simplify = FALSE)
    lapply(pair_indices, function(x) {
      p1 <- paths[x[1]]
      p2 <- paths[x[2]]
      if (identical(names(p1), names(p2)) && (p1 != p2)) {
        warning(
          "Found multiple local file paths pointing the same resource prefix: ", names(p1), ". ",
          "If you run into resource-related issues (e.g. 404 requests), consider ",
          "using `addResourcePath()` and/or `removeResourcePath()` to manage resource mappings.",
          call. = FALSE
        )
      }
    })
  }
  checkResourceConflict(httpuvApp$staticPaths)

  httpuvApp$staticPathOptions <- httpuv::staticPathOptions(
    html_charset = "utf-8",
    headers = list("X-UA-Compatible" = "IE=edge,chrome=1"),
    validation =
      if (!is.null(getOption("shiny.sharedSecret"))) {
        sprintf('"Shiny-Shared-Secret" == "%s"', getOption("shiny.sharedSecret"))
      } else {
        character(0)
      }
  )

  if (is.numeric(port) || is.integer(port)) {
    if (!quiet) {
      hostString <- host
      if (httpuv::ipFamily(host) == 6L)
        hostString <- paste0("[", hostString, "]")
      message('\n', 'Listening on http://', hostString, ':', port)
    }
    return(startServer(host, port, httpuvApp))
  } else if (is.character(port)) {
    if (!quiet) {
      message('\n', 'Listening on domain socket ', port)
    }
    mask <- attr(port, 'mask')
    if (is.null(mask)) {
      stop("`port` is not a valid domain socket (missing `mask` attribute). ",
           "Note that if you're using the default `host` + `port` ",
           "configuration (and not domain sockets), then `port` must ",
           "be numeric, not a string.")
    }
    return(startPipeServer(port, mask, httpuvApp))
  }
}

# Run an application that was created by \code{\link{startApp}}. This
# function should normally be called in a \code{while(TRUE)} loop.
serviceApp <- function() {
  timerCallbacks$executeElapsed()

  flushReact()
  flushPendingSessions()

  # If this R session is interactive, then call service() with a short timeout
  # to keep the session responsive to user input
  maxTimeout <- ifelse(interactive(), 100, 1000)

  timeout <- max(1, min(maxTimeout, timerCallbacks$timeToNextEvent(), later::next_op_secs()))
  service(timeout)

  flushReact()
  flushPendingSessions()
}

.shinyServerMinVersion <- '0.3.4'

# Global flag that's TRUE whenever we're inside of the scope of a call to runApp
.globals$running <- FALSE

#' Check whether a Shiny application is running
#'
#' This function tests whether a Shiny application is currently running.
#'
#' @return `TRUE` if a Shiny application is currently running. Otherwise,
#'   `FALSE`.
#' @export
isRunning <- function() {
  .globals$running
}


# Returns TRUE if we're running in Shiny Server or other hosting environment,
# otherwise returns FALSE.
inShinyServer <- function() {
  nzchar(Sys.getenv('SHINY_PORT'))
}

# This check was moved out of the main function body because of an issue with
# the RStudio debugger. (#1474)
isEmptyMessage <- function(msg) {
  identical(as.raw(c(0x03, 0xe9)), msg)
}
