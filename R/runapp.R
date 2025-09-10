#' Run Shiny Application
#'
#' Runs a Shiny application. This function normally does not return; interrupt R
#' to stop the application (usually by pressing Ctrl+C or Esc).
#'
#' The host parameter was introduced in Shiny 0.9.0. Its default value of
#' `"127.0.0.1"` means that, contrary to previous versions of Shiny, only
#' the current machine can access locally hosted Shiny apps. To allow other
#' clients to connect, use the value `"0.0.0.0"` instead (which was the
#' value that was hard-coded into Shiny in 0.8.0 and earlier).
#'
#' @param appDir The application to run. Should be one of the following:
#'   \itemize{
#'   \item A directory containing `server.R`, plus, either `ui.R` or
#'    a `www` directory that contains the file `index.html`.
#'   \item A directory containing `app.R`.
#'   \item An `.R` file containing a Shiny application, ending with an
#'    expression that produces a Shiny app object.
#'   \item A list with `ui` and `server` components.
#'   \item A Shiny app object created by [shinyApp()].
#'   }
#' @param port The TCP port that the application should listen on. If the
#'   `port` is not specified, and the `shiny.port` option is set (with
#'   `options(shiny.port = XX)`), then that port will be used. Otherwise,
#'   use a random port between 3000:8000, excluding ports that are blocked
#'   by Google Chrome for being considered unsafe: 3659, 4045, 5060,
#'   5061, 6000, 6566, 6665:6669 and 6697. Up to twenty random
#'   ports will be tried.
#' @param launch.browser If true, the system's default web browser will be
#'   launched automatically after the app is started. Defaults to true in
#'   interactive sessions only. The value of this parameter can also be a
#'   function to call with the application's URL.
#' @param host The IPv4 address that the application should listen on. Defaults
#'   to the `shiny.host` option, if set, or `"127.0.0.1"` if not. See
#'   Details.
#' @param workerId Can generally be ignored. Exists to help some editions of
#'   Shiny Server Pro route requests to the correct process.
#' @param quiet Should Shiny status messages be shown? Defaults to FALSE.
#' @param display.mode The mode in which to display the application. If set to
#'   the value `"showcase"`, shows application code and metadata from a
#'   `DESCRIPTION` file in the application directory alongside the
#'   application. If set to `"normal"`, displays the application normally.
#'   Defaults to `"auto"`, which displays the application in the mode given
#'   in its `DESCRIPTION` file, if any.
#' @param test.mode Should the application be launched in test mode? This is
#'   only used for recording or running automated tests. Defaults to the
#'   `shiny.testmode` option, or FALSE if the option is not set.
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
#'   options(device.ask.default = FALSE)
#'
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
runApp <- function(
  appDir=getwd(),
  port=getOption('shiny.port'),
  launch.browser = getOption('shiny.launch.browser', interactive()),
  host=getOption('shiny.host', '127.0.0.1'),
  workerId="", quiet=FALSE,
  display.mode=c("auto", "normal", "showcase"),
  test.mode=getOption('shiny.testmode', FALSE)
) {

  # * Wrap **all** execution of the app inside the otel promise domain
  # * While this could be done at a lower level, it allows for _anything_ within
  #   shiny's control to allow for the opportunity to have otel active spans be
  #   reactivated upon promise domain restoration
  local_ospan_promise_domain()

  on.exit({
    handlerManager$clear()
  }, add = TRUE)

  if (isRunning()) {
    stop("Can't call `runApp()` from within `runApp()`. If your ",
         "application code contains `runApp()`, please remove it.")
  }

  # Make warnings print immediately
  # Set pool.scheduler to support pool package
  ops <- options(
    # Raise warn level to 1, but don't lower it
    warn = max(1, getOption("warn", default = 1)),
    pool.scheduler = scheduleTask
  )
  on.exit(options(ops), add = TRUE)

  # ============================================================================
  # Global onStart/onStop callbacks
  # ============================================================================
  # Invoke user-defined onStop callbacks, before the application's internal
  # onStop callbacks.
  on.exit({
    .globals$onStopCallbacks$invoke()
    .globals$onStopCallbacks <- Callbacks$new()
  }, add = TRUE)

  require(shiny)

  # ============================================================================
  # Convert to Shiny app object
  # ============================================================================
  appParts <- as.shiny.appobj(appDir)

  # ============================================================================
  # Initialize app state object
  # ============================================================================
  # This is so calls to getCurrentAppState() can be used to find (A) whether an
  # app is running and (B), get options and data associated with the app.
  initCurrentAppState(appParts)
  on.exit(clearCurrentAppState(), add = TRUE)
  # Any shinyOptions set after this point will apply to the current app only
  # (and will not persist after the app stops).

  # ============================================================================
  # shinyOptions
  # ============================================================================
  # A unique identifier associated with this run of this application. It is
  # shared across sessions.
  shinyOptions(appToken = createUniqueId(8))

  # Set up default cache for app.
  if (is.null(getShinyOption("cache", default = NULL))) {
    shinyOptions(cache = cachem::cache_mem(max_size = 200 * 1024^2))
  }

  # Extract appOptions (which is a list) and store them as shinyOptions, for
  # this app. (This is the only place we have to store settings that are
  # accessible both the UI and server portion of the app.)
  applyCapturedAppOptions(appParts$appOptions)

  # ============================================================================
  # runApp options set via shinyApp(options = list(...))
  # ============================================================================
  # The lines below set some of the app's running options, which
  # can be:
  #   - left unspecified (in which case the arguments' default
  #     values from `runApp` kick in);
  #   - passed through `shinyApp`
  #   - passed through `runApp` (this function)
  #   - passed through both `shinyApp` and `runApp` (the latter
  #     takes precedence)
  #
  # Matrix of possibilities:
  # | IN shinyApp | IN runApp | result       | check                                                                                                                                  |
  # |-------------|-----------|--------------|----------------------------------------------------------------------------------------------------------------------------------------|
  # | no          | no        | use defaults | exhaust all possibilities: if it's missing (runApp does not specify); THEN if it's not in shinyApp appParts$options; THEN use defaults |
  # | yes         | no        | use shinyApp | if it's missing (runApp does not specify); THEN if it's in shinyApp appParts$options; THEN use shinyApp                                |
  # | no          | yes       | use runApp   | if it's not missing (runApp specifies), use those                                                                                      |
  # | yes         | yes       | use runApp   | if it's not missing (runApp specifies), use those                                                                                      |
  #
  # I tried to make this as compact and intuitive as possible,
  # given that there are four distinct possibilities to check
  appOps <- appParts$options
  findVal <- function(arg, default) {
    if (arg %in% names(appOps)) appOps[[arg]] else default
  }

  if (missing(port))
    port <- findVal("port", port)
  if (missing(launch.browser))
    launch.browser <- findVal("launch.browser", launch.browser)
  if (missing(host))
    host <- findVal("host", host)
  if (missing(quiet))
    quiet <- findVal("quiet", quiet)
  if (missing(display.mode))
    display.mode <- findVal("display.mode", display.mode)
  if (missing(test.mode))
    test.mode <- findVal("test.mode", test.mode)

  if (is.null(host) || is.na(host)) host <- '0.0.0.0'

  # ============================================================================
  # Hosted environment
  # ============================================================================
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

  # ============================================================================
  # Shinytest
  # ============================================================================
  # Set the testmode shinyoption so that this can be read by both the
  # ShinySession and the UI code (which executes separately from the
  # ShinySession code).
  shinyOptions(testmode = test.mode)
  if (test.mode) {
    message("Running application in test mode.")
  }

  # ============================================================================
  # Showcase mode
  # ============================================================================
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

  # ============================================================================
  # Server port
  # ============================================================================
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
          # https://github.com/rstudio/shiny/issues/1784
          # https://chromium.googlesource.com/chromium/src.git/+/refs/heads/main/net/base/port_util.cc
          if (!port %in% c(3659, 4045, 5060, 5061, 6000, 6566, 6665:6669, 6697)) {
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

  # ============================================================================
  # onStart/onStop callbacks
  # ============================================================================
  # Set up the onStop before we call onStart, so that it gets called even if an
  # error happens in onStart.
  if (!is.null(appParts$onStop))
    on.exit(appParts$onStop(), add = TRUE)
  if (!is.null(appParts$onStart))
    appParts$onStart()

  # ============================================================================
  # Start/stop httpuv app
  # ============================================================================
  server <- startApp(appParts, port, host, quiet)

  # Make the httpuv server object accessible. Needed for calling
  # addResourcePath while app is running.
  shinyOptions(server = server)

  on.exit({
    stopServer(server)
  }, add = TRUE)

  # ============================================================================
  # Launch web browser
  # ============================================================================
  if (!is.character(port)) {
    browseHost <- host
    if (identical(host, "0.0.0.0")) {
      # http://0.0.0.0/ doesn't work on QtWebKit (i.e. RStudio viewer)
      browseHost <- "127.0.0.1"
    } else if (identical(host, "::")) {
      browseHost <- "::1"
    }

    if (httpuv::ipFamily(browseHost) == 6L) {
      browseHost <- paste0("[", browseHost, "]")
    }

    appUrl <- paste("http://", browseHost, ":", port, sep="")
    if (is.function(launch.browser))
      launch.browser(appUrl)
    else if (launch.browser)
      utils::browseURL(appUrl)
  } else {
    appUrl <- NULL
  }

  # ============================================================================
  # Application hooks
  # ============================================================================
  callAppHook("onAppStart", appUrl)
  on.exit({
    callAppHook("onAppStop", appUrl)
  }, add = TRUE)

  # ============================================================================
  # Run event loop via httpuv
  # ============================================================================
  .globals$reterror <- NULL
  .globals$retval <- NULL
  .globals$stopped <- FALSE
  # Top-level ..stacktraceoff..; matches with ..stacktraceon in observe(),
  # reactive(), Callbacks$invoke(), and others
  ..stacktraceoff..(
    captureStackTraces({
      while (!.globals$stopped) {
        ..stacktracefloor..(serviceApp())
      }
    })
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
#' [runApp()].
#'
#' @param returnValue The value that should be returned from
#'   [runApp()].
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
#' @param example The name of the example to run, or `NA` (the default) to
#'   list the available examples.
#' @param launch.browser If true, the system's default web browser will be
#'   launched automatically after the app is started. Defaults to true in
#'   interactive sessions only.
#' @param host The IPv4 address that the application should listen on. Defaults
#'   to the `shiny.host` option, if set, or `"127.0.0.1"` if not.
#' @param display.mode The mode in which to display the example. Defaults to
#'   `"auto"`, which uses the value of `DisplayMode` in the example's
#'   `DESCRIPTION` file. Set to `"showcase"` to show the app code and
#'   description with the running app, or `"normal"` to see the example without
#'   code or commentary.
#' @param package The package in which to find the example (defaults to
#'   `"shiny"`).
#'
#'   To provide examples in your package, store examples in the
#'   `inst/examples-shiny` directory of your package. Each example should be
#'   in its own subdirectory and should be runnable when [runApp()] is called
#'   on the subdirectory. Example apps can include a `DESCRIPTION` file and a
#'   `README.md` file to provide metadata and commentary about the example. See
#'   the article on [Display Modes](https://shiny.posit.co/r/articles/build/display-modes/)
#'   on the Shiny website for more information.
#' @inheritParams runApp
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
runExample <- function(
  example = NA,
  port = getOption("shiny.port"),
  launch.browser = getOption("shiny.launch.browser", interactive()),
  host = getOption("shiny.host", "127.0.0.1"),
  display.mode = c("auto", "normal", "showcase"),
  package = "shiny"
) {
  if (!identical(package, "shiny") && !is_installed(package)) {
    rlang::check_installed(package)
  }

  use_legacy_shiny_examples <-
    identical(package, "shiny") &&
    isTRUE(getOption('shiny.legacy.examples', FALSE))

  examplesDir <- system_file(
    if (use_legacy_shiny_examples) "examples" else "examples-shiny",
    package = package
  )

  dir <- resolve(examplesDir, example)

  if (is.null(dir)) {
    valid_examples <- sprintf(
      'Valid examples in {%s}: "%s"',
      package,
      paste(list.files(examplesDir), collapse = '", "')
    )

    if (is.na(example)) {
      message(valid_examples)
      return(invisible())
    }

    stop("Example '", example, "' does not exist. ", valid_examples)
  }

  runApp(dir, port = port, host = host, launch.browser = launch.browser,
         display.mode = display.mode)
}

#' Run a gadget
#'
#' Similar to `runApp`, but handles `input$cancel` automatically, and
#' if running in RStudio, defaults to viewing the app in the Viewer pane.
#'
#' @param app Either a Shiny app object as created by
#'   [`shinyApp()`][shiny] et al, or, a UI object.
#' @param server Ignored if `app` is a Shiny app object; otherwise, passed
#'   along to `shinyApp` (i.e. `shinyApp(ui = app, server = server)`).
#' @param port See [`runApp()`][shiny].
#' @param viewer Specify where the gadget should be displayed--viewer pane,
#'   dialog window, or external browser--by passing in a call to one of the
#'   [viewer()] functions.
#' @param stopOnCancel If `TRUE` (the default), then an `observeEvent`
#'   is automatically created that handles `input$cancel` by calling
#'   `stopApp()` with an error. Pass `FALSE` if you want to handle
#'   `input$cancel` yourself.
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
