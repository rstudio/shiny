#' @include server-input-handlers.R

appsByToken <- NULL
appsNeedingFlush <- NULL
on_load({
  appsByToken <- Map$new()
  appsNeedingFlush <- Map$new()
})


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


.globals$showcaseDefault <- 0

.globals$showcaseOverride <- FALSE


#' Define Server Functionality
#'
#' @description `r lifecycle::badge("superseded")`
#'
#' @description Defines the server-side logic of the Shiny application. This generally
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
#' See the [tutorial](https://shiny.rstudio.com/tutorial/) for more
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
  if (in_devmode()) {
    shinyDeprecated(
      "0.10.0", "shinyServer()",
      details = paste0(
        "When removing `shinyServer()`, ",
        "ensure that the last expression returned from server.R ",
        "is the function normally supplied to `shinyServer(func)`."
      )
    )
  }

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

autoReloadCallbacks <- NULL
on_load({
  autoReloadCallbacks <- Callbacks$new()
})

createAppHandlers <- function(httpHandlers, serverFuncSource) {
  appvars <- new.env()
  appvars$server <- NULL

  sys.www.root <- system_file('www', package='shiny')

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
        if (!get_devmode_option("shiny.autoreload", FALSE)) {
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
                  otel_span_session_start(domain = shinysession, {

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
  else if (isS4(func) && inherits(func, "functionWithTrace"))
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
      "shared" = system_file(package = "shiny", "www", "shared")
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

# Non-blocking service loop using later callbacks.
# Uses 10ms delay between iterations to yield CPU for console interaction.
startServiceLoop <- function(captureResult, cleanup) {
  serviceLoop <- function() {
    if (!.globals$stopped) {
      ..stacktraceoff..(
        captureStackTraces(
          tryCatch(
            ..stacktracefloor..(serviceApp()),
            error = function(e) {
              .globals$stopped <- TRUE
              .globals$retval <- e
              .globals$reterror <- TRUE
            }
          )
        )
      )
      if (!.globals$stopped) {
        later::later(serviceLoop, delay = 0.01)
      } else {
        captureResult()
        cleanup()
      }
    } else {
      # App was stopped externally (e.g., via stopApp())
      captureResult()
      cleanup()
    }
  }
  later::later(serviceLoop, delay = 0.01)
}

.shinyServerMinVersion <- '0.3.4'

#' Check whether a Shiny application is running
#'
#' This function tests whether a Shiny application is currently running.
#'
#' @return `TRUE` if a Shiny application is currently running. Otherwise,
#'   `FALSE`.
#' @export
isRunning <- function() {
  !is.null(getCurrentAppState())
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
