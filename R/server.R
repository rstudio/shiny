#' @include server-input-handlers.R

appsByToken <- Map$new()

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


.globals$resources <- list()

.globals$showcaseDefault <- 0

.globals$showcaseOverride <- FALSE

#' Resource Publishing
#'
#' Adds a directory of static resources to Shiny's web server, with the given
#' path prefix. Primarily intended for package authors to make supporting
#' JavaScript/CSS files available to their components.
#'
#' @param prefix The URL prefix (without slashes). Valid characters are a-z,
#'   A-Z, 0-9, hyphen, period, and underscore; and must begin with a-z or A-Z.
#'   For example, a value of 'foo' means that any request paths that begin with
#'   '/foo' will be mapped to the given directory.
#' @param directoryPath The directory that contains the static resources to be
#'   served.
#'
#' @details You can call \code{addResourcePath} multiple times for a given
#'   \code{prefix}; only the most recent value will be retained. If the
#'   normalized \code{directoryPath} is different than the directory that's
#'   currently mapped to the \code{prefix}, a warning will be issued.
#'
#' @seealso \code{\link{singleton}}
#'
#' @examples
#' addResourcePath('datasets', system.file('data', package='datasets'))
#' @export
addResourcePath <- function(prefix, directoryPath) {
  prefix <- prefix[1]
  if (!grepl('^[a-z][a-z0-9\\-_.]*$', prefix, ignore.case=TRUE, perl=TRUE)) {
    stop("addResourcePath called with invalid prefix; please see documentation")
  }

  if (prefix %in% c('shared')) {
    stop("addResourcePath called with the reserved prefix '", prefix, "'; ",
         "please use a different prefix")
  }

  directoryPath <- normalizePath(directoryPath, mustWork=TRUE)

  existing <- .globals$resources[[prefix]]

  .globals$resources[[prefix]] <- list(directoryPath=directoryPath,
                                       func=staticHandler(directoryPath))
}

resourcePathHandler <- function(req) {
  if (!identical(req$REQUEST_METHOD, 'GET'))
    return(NULL)

  path <- req$PATH_INFO

  match <- regexpr('^/([^/]+)/', path, perl=TRUE)
  if (match == -1)
    return(NULL)
  len <- attr(match, 'capture.length')
  prefix <- substr(path, 2, 2 + len - 1)

  resInfo <- .globals$resources[[prefix]]
  if (is.null(resInfo))
    return(NULL)

  suffix <- substr(path, 2 + len, nchar(path))

  subreq <- as.environment(as.list(req, all.names=TRUE))
  subreq$PATH_INFO <- suffix
  subreq$SCRIPT_NAME <- paste(subreq$SCRIPT_NAME, substr(path, 1, 2 + len), sep='')

  return(resInfo$func(subreq))
}

#' Define Server Functionality
#'
#' Defines the server-side logic of the Shiny application. This generally
#' involves creating functions that map user inputs to various kinds of output.
#' In older versions of Shiny, it was necessary to call \code{shinyServer()} in
#' the \code{server.R} file, but this is no longer required as of Shiny 0.10.
#' Now the \code{server.R} file may simply return the appropriate server
#' function (as the last expression in the code), without calling
#' \code{shinyServer()}.
#'
#' Call \code{shinyServer} from your application's \code{server.R}
#' file, passing in a "server function" that provides the server-side logic of
#' your application.
#'
#' The server function will be called when each client (web browser) first loads
#' the Shiny application's page. It must take an \code{input} and an
#' \code{output} parameter. Any return value will be ignored. It also takes an
#' optional \code{session} parameter, which is used when greater control is
#' needed.
#'
#' See the \href{http://rstudio.github.com/shiny/tutorial/}{tutorial} for more
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
    return(jsonlite::fromJSON(charData, simplifyVector=FALSE))
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

createAppHandlers <- function(httpHandlers, serverFuncSource) {
  appvars <- new.env()
  appvars$server <- NULL

  sys.www.root <- system.file('www', package='shiny')

  # This value, if non-NULL, must be present on all HTTP and WebSocket
  # requests as the Shiny-Shared-Secret header or else access will be
  # denied (403 response for HTTP, and instant close for websocket).
  sharedSecret <- getOption('shiny.sharedSecret')

  appHandlers <- list(
    http = joinHandlers(c(
      sessionHandler,
      httpHandlers,
      sys.www.root,
      resourcePathHandler,
      reactLogHandler)),
    ws = function(ws) {
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

      shinysession <- ShinySession$new(ws)
      appsByToken$set(shinysession$token, shinysession)
      shinysession$setShowcase(.globals$showcaseDefault)

      messageHandler <- function(binary, msg) {
        withReactiveDomain(shinysession, {
          # To ease transition from websockets-based code. Should remove once we're stable.
          if (is.character(msg))
            msg <- charToRaw(msg)

          if (isTRUE(getOption('shiny.trace'))) {
            if (binary)
              message("RECV ", '$$binary data$$')
            else
              message("RECV ", rawToChar(msg))
          }

          if (identical(charToRaw("\003\xe9"), msg))
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
            }
          }

          withRestoreContext(shinysession$restoreContext, {

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

                shinysession$manageInputs(msg$data)

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
            shinysession$manageHiddenOutputs()

            if (exists(".shiny__stdout", globalenv()) &&
                exists("HTTP_GUID", ws$request)) {
              # safe to assume we're in shiny-server
              shiny_stdout <- get(".shiny__stdout", globalenv())

              # eNter a flushReact
              writeLines(paste("_n_flushReact ", get("HTTP_GUID", ws$request),
                " @ ", sprintf("%.3f", as.numeric(Sys.time())),
                sep=""), con=shiny_stdout)
              flush(shiny_stdout)

              flushReact()

              # eXit a flushReact
              writeLines(paste("_x_flushReact ", get("HTTP_GUID", ws$request),
                " @ ", sprintf("%.3f", as.numeric(Sys.time())),
                sep=""), con=shiny_stdout)
              flush(shiny_stdout)
            } else {
              flushReact()
            }

            flushAllSessions()
          })
        })
      }
      ws$onMessage(function(binary, msg) {
        # If unhandled errors occur, make sure they get properly logged
        withLogErrors(messageHandler(binary, msg))
      })

      ws$onClose(function() {
        shinysession$wsClosed()
        appsByToken$remove(shinysession$token)
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
  # Note: NULL values are OK. isS4(NULL) returns FALSE, body(NULL)
  # returns NULL.
  if (isS4(func) && class(func) == "functionWithTrace")
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

  if (is.numeric(port) || is.integer(port)) {
    if (!quiet) {
      message('\n', 'Listening on http://', host, ':', port)
    }
    return(startServer(host, port, handlerManager$createHttpuvApp()))
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
    return(startPipeServer(port, mask, handlerManager$createHttpuvApp()))
  }
}

# Run an application that was created by \code{\link{startApp}}. This
# function should normally be called in a \code{while(TRUE)} loop.
serviceApp <- function() {
  if (timerCallbacks$executeElapsed()) {
    for (shinysession in appsByToken$values()) {
      shinysession$manageHiddenOutputs()
    }

    flushReact()
    flushAllSessions()
  }

  # If this R session is interactive, then call service() with a short timeout
  # to keep the session responsive to user input
  maxTimeout <- ifelse(interactive(), 100, 1000)

  timeout <- max(1, min(maxTimeout, timerCallbacks$timeToNextEvent()))
  service(timeout)
}

.shinyServerMinVersion <- '0.3.4'

#' Run Shiny Application
#'
#' Runs a Shiny application. This function normally does not return; interrupt R
#' to stop the application (usually by pressing Ctrl+C or Esc).
#'
#' The host parameter was introduced in Shiny 0.9.0. Its default value of
#' \code{"127.0.0.1"} means that, contrary to previous versions of Shiny, only
#' the current machine can access locally hosted Shiny apps. To allow other
#' clients to connect, use the value \code{"0.0.0.0"} instead (which was the
#' value that was hard-coded into Shiny in 0.8.0 and earlier).
#'
#' @param appDir The application to run. Should be one of the following:
#'   \itemize{
#'   \item A directory containing \code{server.R}, plus, either \code{ui.R} or
#'    a \code{www} directory that contains the file \code{index.html}.
#'   \item A directory containing \code{app.R}.
#'   \item An \code{.R} file containing a Shiny application, ending with an
#'    expression that produces a Shiny app object.
#'   \item A list with \code{ui} and \code{server} components.
#'   \item A Shiny app object created by \code{\link{shinyApp}}.
#'   }
#' @param port The TCP port that the application should listen on. If the
#'   \code{port} is not specified, and the \code{shiny.port} option is set (with
#'   \code{options(shiny.port = XX)}), then that port will be used. Otherwise,
#'   use a random port.
#' @param launch.browser If true, the system's default web browser will be
#'   launched automatically after the app is started. Defaults to true in
#'   interactive sessions only. This value of this parameter can also be a
#'   function to call with the application's URL.
#' @param host The IPv4 address that the application should listen on. Defaults
#'   to the \code{shiny.host} option, if set, or \code{"127.0.0.1"} if not. See
#'   Details.
#' @param workerId Can generally be ignored. Exists to help some editions of
#'   Shiny Server Pro route requests to the correct process.
#' @param quiet Should Shiny status messages be shown? Defaults to FALSE.
#' @param display.mode The mode in which to display the application. If set to
#'   the value \code{"showcase"}, shows application code and metadata from a
#'   \code{DESCRIPTION} file in the application directory alongside the
#'   application. If set to \code{"normal"}, displays the application normally.
#'   Defaults to \code{"auto"}, which displays the application in the mode given
#'   in its \code{DESCRIPTION} file, if any.
#'
#' @examples
#' \dontrun{
#' # Start app in the current working directory
#' runApp()
#'
#' # Start app in a subdirectory called myapp
#' runApp("myapp")
#' }
#'
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'   # Apps can be run without a server.r and ui.r file
#'   runApp(list(
#'     ui = bootstrapPage(
#'       numericInput('n', 'Number of obs', 100),
#'       plotOutput('plot')
#'     ),
#'     server = function(input, output) {
#'       output$plot <- renderPlot({ hist(runif(input$n)) })
#'     }
#'   ))
#'
#'
#'   # Running a Shiny app object
#'   app <- shinyApp(
#'     ui = bootstrapPage(
#'       numericInput('n', 'Number of obs', 100),
#'       plotOutput('plot')
#'     ),
#'     server = function(input, output) {
#'       output$plot <- renderPlot({ hist(runif(input$n)) })
#'     }
#'   )
#'   runApp(app)
#' }
#' @export
runApp <- function(appDir=getwd(),
                   port=getOption('shiny.port'),
                   launch.browser=getOption('shiny.launch.browser',
                                            interactive()),
                   host=getOption('shiny.host', '127.0.0.1'),
                   workerId="", quiet=FALSE,
                   display.mode=c("auto", "normal", "showcase")) {
  on.exit({
    handlerManager$clear()
  }, add = TRUE)

  # Enable per-app Shiny options
  oldOptionSet <- .globals$options
  on.exit({
    .globals$options <- oldOptionSet
  },add = TRUE)

  if (is.null(host) || is.na(host))
    host <- '0.0.0.0'

  # Make warnings print immediately
  # Set pool.scheduler to support pool package
  ops <- options(warn = 1, pool.scheduler = scheduleTask)
  on.exit(options(ops), add = TRUE)

  workerId(workerId)

  if (inShinyServer()) {
    # If SHINY_PORT is set, we're running under Shiny Server. Check the version
    # to make sure it is compatible. Older versions of Shiny Server don't set
    # SHINY_SERVER_VERSION, those will return "" which is considered less than
    # any valid version.
    ver <- Sys.getenv('SHINY_SERVER_VERSION')
    if (utils::compareVersion(ver, .shinyServerMinVersion) < 0) {
      warning('Shiny Server v', .shinyServerMinVersion,
              ' or later is required; please upgrade!')
    }
  }

  # Showcase mode is disabled by default; it must be explicitly enabled in
  # either the DESCRIPTION file for directory-based apps, or via
  # the display.mode parameter. The latter takes precedence.
  setShowcaseDefault(0)

  # If appDir specifies a path, and display mode is specified in the
  # DESCRIPTION file at that path, apply it here.
  if (is.character(appDir)) {
    # if appDir specifies a .R file (single-file Shiny app), look for the
    # DESCRIPTION in the parent directory
    desc <- file.path.ci(
      if (tolower(tools::file_ext(appDir)) == "r")
        dirname(appDir)
      else
        appDir, "DESCRIPTION")
    if (file.exists(desc)) {
      con <- file(desc, encoding = checkEncoding(desc))
      on.exit(close(con), add = TRUE)
      settings <- read.dcf(con)
      if ("DisplayMode" %in% colnames(settings)) {
        mode <- settings[1, "DisplayMode"]
        if (mode == "Showcase") {
          setShowcaseDefault(1)
          if ("IncludeWWW" %in% colnames(settings)) {
            .globals$IncludeWWW <- as.logical(settings[1, "IncludeWWW"])
            if (is.na(.globals$IncludeWWW)) {
              stop("In your Description file, `IncludeWWW` ",
                   "must be set to `True` (default) or `False`")
            }
          } else {
            .globals$IncludeWWW <- TRUE
          }
        }
      }
    }
  }

  ## default is to show the .js, .css and .html files in the www directory
  ## (if not in showcase mode, this variable will simply be ignored)
  if (is.null(.globals$IncludeWWW) || is.na(.globals$IncludeWWW)) {
    .globals$IncludeWWW <- TRUE
  }

  # If display mode is specified as an argument, apply it (overriding the
  # value specified in DESCRIPTION, if any).
  display.mode <- match.arg(display.mode)
  if (display.mode == "normal") {
    setShowcaseDefault(0)
  }
  else if (display.mode == "showcase") {
    setShowcaseDefault(1)
  }

  require(shiny)

  # determine port if we need to
  if (is.null(port)) {

    # Try up to 20 random ports. If we don't succeed just plow ahead
    # with the final value we tried, and let the "real" startServer
    # somewhere down the line fail and throw the error to the user.
    #
    # If we (think we) succeed, save the value as .globals$lastPort,
    # and try that first next time the user wants a random port.

    for (i in 1:20) {
      if (!is.null(.globals$lastPort)) {
        port <- .globals$lastPort
        .globals$lastPort <- NULL
      }
      else {
        # Try up to 20 random ports
        while (TRUE) {
          port <- p_randomInt(3000, 8000)
          # Reject ports in this range that are considered unsafe by Chrome
          # http://superuser.com/questions/188058/which-ports-are-considered-unsafe-on-chrome
          if (!port %in% c(3659, 4045, 6000, 6665:6669)) {
            break
          }
        }
      }

      # Test port to see if we can use it
      tmp <- try(startServer(host, port, list()), silent=TRUE)
      if (!inherits(tmp, 'try-error')) {
        stopServer(tmp)
        .globals$lastPort <- port
        break
      }
    }
  }

  appParts <- as.shiny.appobj(appDir)

  # Extract appOptions (which is a list) and store them as shinyOptions, for
  # this app. (This is the only place we have to store settings that are
  # accessible both the UI and server portion of the app.)
  unconsumeAppOptions(appParts$appOptions)

  # Set up the onEnd before we call onStart, so that it gets called even if an
  # error happens in onStart.
  if (!is.null(appParts$onEnd))
    on.exit(appParts$onEnd(), add = TRUE)
  if (!is.null(appParts$onStart))
    appParts$onStart()

  server <- startApp(appParts, port, host, quiet)

  on.exit({
    stopServer(server)
  }, add = TRUE)

  if (!is.character(port)) {
    # http://0.0.0.0/ doesn't work on QtWebKit (i.e. RStudio viewer)
    browseHost <- if (identical(host, "0.0.0.0")) "127.0.0.1" else host

    appUrl <- paste("http://", browseHost, ":", port, sep="")
    if (is.function(launch.browser))
      launch.browser(appUrl)
    else if (launch.browser)
      utils::browseURL(appUrl)
  } else {
    appUrl <- NULL
  }

  # call application hooks
  callAppHook("onAppStart", appUrl)
  on.exit({
    callAppHook("onAppStop", appUrl)
  }, add = TRUE)

  .globals$reterror <- NULL
  .globals$retval <- NULL
  .globals$stopped <- FALSE
  # Top-level ..stacktraceoff..; matches with ..stacktraceon in observe(),
  # reactive(), Callbacks$invoke(), and others
  ..stacktraceoff..(
    captureStackTraces(
      while (!.globals$stopped) {
        serviceApp()
        Sys.sleep(0.001)
      }
    )
  )

  if (isTRUE(.globals$reterror)) {
    stop(.globals$retval)
  }
  else if (.globals$retval$visible)
    .globals$retval$value
  else
    invisible(.globals$retval$value)
}

#' Stop the currently running Shiny app
#'
#' Stops the currently running Shiny app, returning control to the caller of
#' \code{\link{runApp}}.
#'
#' @param returnValue The value that should be returned from
#'   \code{\link{runApp}}.
#' @export
stopApp <- function(returnValue = invisible()) {
  # reterror will indicate whether retval is an error (i.e. it should be passed
  # to stop() when the serviceApp loop stops) or a regular value (in which case
  # it should simply be returned with the appropriate visibility).
  .globals$reterror <- FALSE
  ..stacktraceoff..(
    tryCatch(
      {
        captureStackTraces(
          .globals$retval <- withVisible(..stacktraceon..(force(returnValue)))
        )
      },
      error = function(e) {
        .globals$retval <- e
        .globals$reterror <- TRUE
      }
    )
  )

  .globals$stopped <- TRUE
  httpuv::interrupt()
}

#' Run Shiny Example Applications
#'
#' Launch Shiny example applications, and optionally, your system's web browser.
#'
#' @param example The name of the example to run, or \code{NA} (the default) to
#'   list the available examples.
#' @param port The TCP port that the application should listen on. Defaults to
#'   choosing a random port.
#' @param launch.browser If true, the system's default web browser will be
#'   launched automatically after the app is started. Defaults to true in
#'   interactive sessions only.
#' @param host The IPv4 address that the application should listen on. Defaults
#'   to the \code{shiny.host} option, if set, or \code{"127.0.0.1"} if not.
#' @param display.mode The mode in which to display the example. Defaults to
#'   \code{showcase}, but may be set to \code{normal} to see the example without
#'   code or commentary.
#'
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'   # List all available examples
#'   runExample()
#'
#'   # Run one of the examples
#'   runExample("01_hello")
#'
#'   # Print the directory containing the code for all examples
#'   system.file("examples", package="shiny")
#' }
#' @export
runExample <- function(example=NA,
                       port=NULL,
                       launch.browser=getOption('shiny.launch.browser',
                                                interactive()),
                       host=getOption('shiny.host', '127.0.0.1'),
                       display.mode=c("auto", "normal", "showcase")) {
  examplesDir <- system.file('examples', package='shiny')
  dir <- resolve(examplesDir, example)
  if (is.null(dir)) {
    if (is.na(example)) {
      errFun <- message
      errMsg <- ''
    }
    else {
      errFun <- stop
      errMsg <- paste('Example', example, 'does not exist. ')
    }

    errFun(errMsg,
           'Valid examples are "',
           paste(list.files(examplesDir), collapse='", "'),
           '"')
  }
  else {
    runApp(dir, port = port, host = host, launch.browser = launch.browser,
           display.mode = display.mode)
  }
}

#' Run a gadget
#'
#' Similar to \code{runApp}, but handles \code{input$cancel} automatically, and
#' if running in RStudio, defaults to viewing the app in the Viewer pane.
#'
#' @param app Either a Shiny app object as created by
#'   \code{\link[=shiny]{shinyApp}} et al, or, a UI object.
#' @param server Ignored if \code{app} is a Shiny app object; otherwise, passed
#'   along to \code{shinyApp} (i.e. \code{shinyApp(ui = app, server = server)}).
#' @param port See \code{\link[=shiny]{runApp}}.
#' @param viewer Specify where the gadget should be displayed--viewer pane,
#'   dialog window, or external browser--by passing in a call to one of the
#'   \code{\link{viewer}} functions.
#' @param stopOnCancel If \code{TRUE} (the default), then an \code{observeEvent}
#'   is automatically created that handles \code{input$cancel} by calling
#'   \code{stopApp()} with an error. Pass \code{FALSE} if you want to handle
#'   \code{input$cancel} yourself.
#' @return The value returned by the gadget.
#'
#' @examples
#' \dontrun{
#' library(shiny)
#'
#' ui <- fillPage(...)
#'
#' server <- function(input, output, session) {
#'   ...
#' }
#'
#' # Either pass ui/server as separate arguments...
#' runGadget(ui, server)
#'
#' # ...or as a single app object
#' runGadget(shinyApp(ui, server))
#' }
#' @export
runGadget <- function(app, server = NULL, port = getOption("shiny.port"),
  viewer = paneViewer(), stopOnCancel = TRUE) {

  if (!is.shiny.appobj(app)) {
    app <- shinyApp(app, server)
  }

  if (isTRUE(stopOnCancel)) {
    app <- decorateServerFunc(app, function(input, output, session) {
      observeEvent(input$cancel, {
        stopApp(stop("User cancel", call. = FALSE))
      })
    })
  }

  if (is.null(viewer)) {
    viewer <- utils::browseURL
  }

  shiny::runApp(app, port = port, launch.browser = viewer)
}

# Add custom functionality to a Shiny app object's server func
decorateServerFunc <- function(appobj, serverFunc) {
  origServerFuncSource <- appobj$serverFuncSource
  appobj$serverFuncSource <- function() {
    origServerFunc <- origServerFuncSource()
    function(input, output, session) {
      serverFunc(input, output, session)

      # The clientData and session arguments are optional; check if
      # each exists
      args <- argsForServerFunc(origServerFunc, session)
      do.call(origServerFunc, args)
    }
  }
  appobj
}

#' Viewer options
#'
#' Use these functions to control where the gadget is displayed in RStudio (or
#' other R environments that emulate RStudio's viewer pane/dialog APIs). If
#' viewer APIs are not available in the current R environment, then the gadget
#' will be displayed in the system's default web browser (see
#' \code{\link[utils]{browseURL}}).
#'
#' @return A function that takes a single \code{url} parameter, suitable for
#'   passing as the \code{viewer} argument of \code{\link{runGadget}}.
#'
#' @rdname viewer
#' @name viewer
NULL

#' @param minHeight The minimum height (in pixels) desired to show the gadget in
#'   the viewer pane. If a positive number, resize the pane if necessary to show
#'   at least that many pixels. If \code{NULL}, use the existing viewer pane
#'   size. If \code{"maximize"}, use the maximum available vertical space.
#' @rdname viewer
#' @export
paneViewer <- function(minHeight = NULL) {
  viewer <- getOption("viewer")
  if (is.null(viewer)) {
    utils::browseURL
  } else {
    function(url) {
      viewer(url, minHeight)
    }
  }
}

#' @param dialogName The window title to display for the dialog.
#' @param width,height The desired dialog width/height, in pixels.
#' @rdname viewer
#' @export
dialogViewer <- function(dialogName, width = 600, height = 600) {
  viewer <- getOption("shinygadgets.showdialog")
  if (is.null(viewer)) {
    utils::browseURL
  } else {
    function(url) {
      viewer(dialogName, url, width = width, height = height)
    }
  }
}

#' @param browser See \code{\link[utils]{browseURL}}.
#' @rdname viewer
#' @export
browserViewer <- function(browser = getOption("browser")) {
  function(url) {
    utils::browseURL(url, browser = browser)
  }
}

# Returns TRUE if we're running in Shiny Server or other hosting environment,
# otherwise returns FALSE.
inShinyServer <- function() {
  nzchar(Sys.getenv('SHINY_PORT'))
}
