#' @include globals.R

appsByToken <- Map$new()

# Create a map for input handlers and register the defaults.
inputHandlers <- Map$new()

#' Register an Input Handler
#'
#' Adds an input handler for data of this type. When called, Shiny will use the
#' function provided to refine the data passed back from the client (after being
#' deserialized by RJSONIO) before making it available in the \code{input}
#' variable of the \code{server.R} file.
#'
#' This function will register the handler for the duration of the R process
#' (unless Shiny is explicitly reloaded). For that reason, the \code{type} used
#' should be very specific to this package to minimize the risk of colliding
#' with another Shiny package which might use this data type name. We recommend
#' the format of "packageName.widgetName".
#'
#' Currently Shiny registers the following handlers: \code{shiny.matrix},
#' \code{shiny.number}, and \code{shiny.date}.
#'
#' The \code{type} of a custom Shiny Input widget will be deduced using the
#' \code{getType()} JavaScript function on the registered Shiny inputBinding.
#' @param type The type for which the handler should be added -- should be a
#' single-element character vector.
#' @param fun The handler function. This is the function that will be used to
#'   parse the data delivered from the client before it is available in the
#'   \code{input} variable. The function will be called with the following three
#'   parameters:
#'    \enumerate{
#'      \item{The value of this input as provided by the client, deserialized
#'      using RJSONIO.}
#'      \item{The \code{shinysession} in which the input exists.}
#'      \item{The name of the input.}
#'    }
#' @param force If \code{TRUE}, will overwrite any existing handler without
#' warning. If \code{FALSE}, will throw an error if this class already has
#' a handler defined.
#' @examples
#' \dontrun{
#' # Register an input handler which rounds a input number to the nearest integer
#' registerInputHandler("mypackage.validint", function(x, shinysession, name) {
#'   if (is.null(x)) return(NA)
#'   round(x)
#' })
#'
#' ## On the Javascript side, the associated input binding must have a corresponding getType method:
#' getType: function(el) {
#'   return "mypackage.validint";
#' }
#'
#' }
#' @seealso \code{\link{removeInputHandler}}
#' @export
registerInputHandler <- function(type, fun, force=FALSE){
  if (inputHandlers$containsKey(type) && !force){
    stop("There is already an input handler for type: ", type)
  }
  inputHandlers$set(type, fun)
}

#' Deregister an Input Handler
#'
#' Removes an Input Handler. Rather than using the previously specified handler
#' for data of this type, the default RJSONIO serialization will be used.
#'
#' @param type The type for which handlers should be removed.
#' @return The handler previously associated with this \code{type}, if one
#'   existed. Otherwise, \code{NULL}.
#' @seealso \code{\link{registerInputHandler}}
#' @export
removeInputHandler <- function(type){
  inputHandlers$remove(type)
}

# Takes a list-of-lists and returns a matrix. The lists
# must all be the same length. NULL is replaced by NA.
registerInputHandler("shiny.matrix", function(data, ...) {
  if (length(data) == 0)
    return(matrix(nrow=0, ncol=0))

  m <- matrix(unlist(lapply(data, function(x) {
    sapply(x, function(y) {
      ifelse(is.null(y), NA, y)
    })
  })), nrow = length(data[[1]]), ncol = length(data))
  return(m)
})

registerInputHandler("shiny.number", function(val, ...){
  ifelse(is.null(val), NA, val)
})

registerInputHandler("shiny.date", function(val, ...){
  # First replace NULLs with NA, then convert to Date vector
  datelist <- ifelse(lapply(val, is.null), NA, val)
  as.Date(unlist(datelist))
})

registerInputHandler("shiny.action", function(val, ...) {
  # mark up the action button value with a special class so we can recognize it later
  class(val) <- c(class(val), "shinyActionButtonValue")
  val
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
#'
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
#'
#' @param func The server function for this application. See the details section
#'   for more information.
#'
#' @details
#' Call \code{shinyServer} from your application's \code{server.R} file, passing
#' in a "server function" that provides the server-side logic of your
#' application.
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
#' @examples
#' \dontrun{
#' # A very simple Shiny app that takes a message from the user
#' # and outputs an uppercase version of it.
#' shinyServer(function(input, output, session) {
#'   output$uppercase <- renderText({
#'     toupper(input$message)
#'   })
#' })
#' }
#'
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
    # use native encoding for the message
    nativeData <- iconv(rawToChar(data), 'UTF-8')
    return(fromJSON(nativeData, asText=TRUE, simplify=FALSE))
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

      shinysession <- ShinySession$new(ws)
      appsByToken$set(shinysession$token, shinysession)
      shinysession$setShowcase(.globals$showcaseDefault)

      ws$onMessage(function(binary, msg) {
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

        # Do our own list simplifying here. sapply/simplify2array give names to
        # character vectors, which is rarely what we want.
        if (!is.null(msg$data)) {
          for (name in names(msg$data)) {
            val <- msg$data[[name]]

            splitName <- strsplit(name, ':')[[1]]
            if (length(splitName) > 1) {
              msg$data[[name]] <- NULL

              if (!inputHandlers$containsKey(splitName[[2]])){
                # No input handler registered for this type
                stop("No handler registered for for type ", name)
              }

              msg$data[[ splitName[[1]] ]] <-
                  inputHandlers$get(splitName[[2]])(
                      val,
                      shinysession,
                      splitName[[1]] )
            }
            else if (is.list(val) && is.null(names(val))) {
              val_flat <- unlist(val, recursive = TRUE)

              if (is.null(val_flat)) {
                # This is to assign NULL instead of deleting the item
                msg$data[name] <- list(NULL)
              } else {
                msg$data[[name]] <- val_flat
              }
            }
          }
        }

        switch(
          msg$method,
          init = {

            serverFunc <- serverFuncSource()
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
              shinysession$singletons <<- strsplit(
                msg$data$.clientdata_singletons, ',')[[1]]
            }

            local({
              args <- list(
                input=shinysession$input,
                output=.createOutputWriter(shinysession))

              # The clientData and session arguments are optional; check if
              # each exists
              if ('clientData' %in% names(formals(serverFunc)))
                args$clientData <- shinysession$clientData

              if ('session' %in% names(formals(serverFunc)))
                args$session <- shinysession$session

              withReactiveDomain(shinysession$session, {
                do.call(appvars$server, args)
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
        lapply(appsByToken$values(), function(shinysession) {
          shinysession$flushOutput()
          NULL
        })
      })

      ws$onClose(function() {
        shinysession$close()
        appsByToken$remove(shinysession$token)
      })

      return(TRUE)
    }
  )
  return(appHandlers)
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

    for (shinysession in appsByToken$values()) {
      shinysession$flushOutput()
    }
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
#' Runs a Shiny application. This function normally does not return; interrupt
#' R to stop the application (usually by pressing Ctrl+C or Esc).
#'
#' The host parameter was introduced in Shiny 0.9.0. Its default value of
#' \code{"127.0.0.1"} means that, contrary to previous versions of Shiny, only
#' the current machine can access locally hosted Shiny apps. To allow other
#' clients to connect, use the value \code{"0.0.0.0"} instead (which was the
#' value that was hard-coded into Shiny in 0.8.0 and earlier).
#'
#' @param appDir The directory of the application. Should contain
#'   \code{server.R}, plus, either \code{ui.R} or a \code{www} directory that
#'   contains the file \code{index.html}. Defaults to the working directory.
#' @param port The TCP port that the application should listen on. Defaults to
#'   choosing a random port.
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
#'   Defaults to \code{"auto"}, which displays the application in the mode
#'   given in its \code{DESCRIPTION} file, if any.
#'
#' @examples
#' \dontrun{
#' # Start app in the current working directory
#' runApp()
#'
#' # Start app in a subdirectory called myapp
#' runApp("myapp")
#'
#'
#' # Apps can be run without a server.r and ui.r file
#' runApp(list(
#'   ui = bootstrapPage(
#'     numericInput('n', 'Number of obs', 100),
#'     plotOutput('plot')
#'   ),
#'   server = function(input, output) {
#'     output$plot <- renderPlot({ hist(runif(input$n)) })
#'   }
#' ))
#' }
#' @export
runApp <- function(appDir=getwd(),
                   port=NULL,
                   launch.browser=getOption('shiny.launch.browser',
                                            interactive()),
                   host=getOption('shiny.host', '127.0.0.1'),
                   workerId="", quiet=FALSE,
                   display.mode=c("auto", "normal", "showcase")) {
  on.exit({
    handlerManager$clear()
  }, add = TRUE)


  if (is.null(host) || is.na(host))
    host <- '0.0.0.0'

  # Make warnings print immediately
  ops <- options(warn = 1)
  on.exit(options(ops), add = TRUE)

  workerId(workerId)

  if (nzchar(Sys.getenv('SHINY_PORT'))) {
    # If SHINY_PORT is set, we're running under Shiny Server. Check the version
    # to make sure it is compatible. Older versions of Shiny Server don't set
    # SHINY_SERVER_VERSION, those will return "" which is considered less than
    # any valid version.
    ver <- Sys.getenv('SHINY_SERVER_VERSION')
    if (compareVersion(ver, .shinyServerMinVersion) < 0) {
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
    desc <- file.path.ci(appDir, "DESCRIPTION")
    if (file.exists(desc)) {
      con <- file(desc, encoding = checkEncoding(desc))
      on.exit(close(con), add = TRUE)
      settings <- read.dcf(con)
      if ("DisplayMode" %in% colnames(settings)) {
        mode <- settings[1,"DisplayMode"]
        if (mode == "Showcase") {
          setShowcaseDefault(1)
        }
      }
    }
  }

  # If display mode is specified as an argument, apply it (overriding the
  # value specified in DESCRIPTION, if any).
  display.mode <- match.arg(display.mode)
  if (display.mode == "normal")
    setShowcaseDefault(0)
  else if (display.mode == "showcase")
    setShowcaseDefault(1)

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
        port <- p_randomInt(3000, 8000)
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
  if (!is.null(appParts$onStart))
    appParts$onStart()
  if (!is.null(appParts$onEnd))
    on.exit(appParts$onEnd(), add = TRUE)

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

  .globals$retval <- NULL
  .globals$stopped <- FALSE
  shinyCallingHandlers(
    while (!.globals$stopped) {
      serviceApp()
      Sys.sleep(0.001)
    }
  )

  return(.globals$retval)
}

#' Stop the currently running Shiny app
#'
#' Stops the currently running Shiny app, returning control to the caller of
#' \code{\link{runApp}}.
#'
#' @param returnValue The value that should be returned from
#'   \code{\link{runApp}}.
#'
#' @export
stopApp <- function(returnValue = NULL) {
  .globals$retval <- returnValue
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
#' \dontrun{
#' # List all available examples
#' runExample()
#'
#' # Run one of the examples
#' runExample("01_hello")
#'
#' # Print the directory containing the code for all examples
#' system.file("examples", package="shiny")
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
