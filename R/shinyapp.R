# TODO: Subapp global.R

#' Create a Shiny app object
#'
#' These functions create Shiny app objects from either an explicit UI/server
#' pair (`shinyApp`), or by passing the path of a directory that contains a
#' Shiny app (`shinyAppDir`).
#'
#' Normally when this function is used at the R console, the Shiny app object is
#' automatically passed to the `print()` function, which runs the app. If
#' this is called in the middle of a function, the value will not be passed to
#' `print()` and the app will not be run. To make the app run, pass the app
#' object to `print()` or [runApp()].
#'
#' @param ui The UI definition of the app (for example, a call to
#'   `fluidPage()` with nested controls).
#'
#'    If bookmarking is enabled (see `enableBookmarking`), this must be
#'    a single argument function that returns the UI definition.
#' @param server A function with three parameters: `input`, `output`, and
#'   `session`. The function is called once for each session ensuring that each
#'   app is independent.
#' @param onStart A function that will be called before the app is actually run.
#'   This is only needed for `shinyAppObj`, since in the `shinyAppDir`
#'   case, a `global.R` file can be used for this purpose.
#' @param options Named options that should be passed to the `runApp` call
#'   (these can be any of the following: "port", "launch.browser", "host", "quiet",
#'   "display.mode" and "test.mode"). You can also specify `width` and
#'   `height` parameters which provide a hint to the embedding environment
#'   about the ideal height/width for the app.
#' @param uiPattern A regular expression that will be applied to each `GET`
#'   request to determine whether the `ui` should be used to handle the
#'   request. Note that the entire request path must match the regular
#'   expression in order for the match to be considered successful.
#' @param enableBookmarking Can be one of `"url"`, `"server"`, or
#'   `"disable"`. The default value, `NULL`, will respect the setting from
#'   any previous calls to  [enableBookmarking()]. See [enableBookmarking()]
#'   for more information on bookmarking your app.
#' @return An object that represents the app. Printing the object or passing it
#'   to [runApp()] will run the app.
#'
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'   options(device.ask.default = FALSE)
#'
#'   shinyApp(
#'     ui = fluidPage(
#'       numericInput("n", "n", 1),
#'       plotOutput("plot")
#'     ),
#'     server = function(input, output) {
#'       output$plot <- renderPlot( plot(head(cars, input$n)) )
#'     }
#'   )
#'
#'   shinyAppDir(system.file("examples/01_hello", package="shiny"))
#'
#'
#'   # The object can be passed to runApp()
#'   app <- shinyApp(
#'     ui = fluidPage(
#'       numericInput("n", "n", 1),
#'       plotOutput("plot")
#'     ),
#'     server = function(input, output) {
#'       output$plot <- renderPlot( plot(head(cars, input$n)) )
#'     }
#'   )
#'
#'   runApp(app)
#' }
#' @export
shinyApp <- function(ui, server, onStart=NULL, options=list(),
                     uiPattern="/", enableBookmarking=NULL) {
  if (!is.function(server)) {
    stop("`server` must be a function", call. = FALSE)
  }

  # Ensure that the entire path is a match
  uiPattern <- sprintf("^%s$", uiPattern)

  httpHandler <- uiHttpHandler(ui, uiPattern)

  serverFuncSource <- function() {
    server
  }

  if (!is.null(enableBookmarking)) {
    bookmarkStore <- match.arg(enableBookmarking, c("url", "server", "disable"))
    enableBookmarking(bookmarkStore)
  }

  # Store the appDir and bookmarking-related options, so that we can read them
  # from within the app.
  appOptions <- captureAppOptions()

  structure(
    list(
      httpHandler = httpHandler,
      serverFuncSource = serverFuncSource,
      onStart = onStart,
      options = options,
      appOptions = appOptions
    ),
    class = "shiny.appobj"
  )
}

#' @rdname shinyApp
#' @param appDir Path to directory that contains a Shiny app (i.e. a server.R
#'   file and either ui.R or www/index.html)
#' @export
shinyAppDir <- function(appDir, options=list()) {
  if (!utils::file_test('-d', appDir)) {
    rlang::abort(
      paste0("No Shiny application exists at the path \"", appDir, "\""),
      class = "invalidShinyAppDir"
    )
  }

  # In case it's a relative path, convert to absolute (so we're not adversely
  # affected by future changes to the path)
  appDir <- normalizePath(appDir, mustWork = TRUE)

  if (file.exists.ci(appDir, "server.R")) {
    shinyAppDir_serverR(appDir, options = options)
  } else if (file.exists.ci(appDir, "app.R")) {
    shinyAppDir_appR("app.R", appDir, options = options)
  } else {
    rlang::abort(
      "App dir must contain either app.R or server.R.",
      class = "invalidShinyAppDir"
    )
  }
}

#' @rdname shinyApp
#' @param appFile Path to a .R file containing a Shiny application
#' @export
shinyAppFile <- function(appFile, options=list()) {
  appFile <- normalizePath(appFile, mustWork = TRUE)
  appDir <- dirname(appFile)

  shinyAppDir_appR(basename(appFile), appDir, options = options)
}

# This reads in an app dir in the case that there's a server.R (and ui.R/www)
# present, and returns a shiny.appobj.
# appDir must be a normalized (absolute) path, not a relative one
shinyAppDir_serverR <- function(appDir, options=list()) {
  # Most of the complexity here comes from needing to hot-reload if the .R files
  # change on disk, or are created, or are removed.

  # In an upcoming version of shiny, this option will go away.
  if (getOption("shiny.autoload.r", TRUE)) {
    # Create a child env which contains all the helpers and will be the shared parent
    # of the ui.R and server.R load.
    sharedEnv <- new.env(parent = globalenv())
  } else {
    # old behavior
    sharedEnv <- globalenv()
  }

  # uiHandlerSource is a function that returns an HTTP handler for serving up
  # ui.R as a webpage. The "cachedFuncWithFile" call makes sure that the closure
  # we're creating here only gets executed when ui.R's contents change.
  uiHandlerSource <- cachedFuncWithFile(appDir, "ui.R", case.sensitive = FALSE,
    function(uiR) {
      if (file.exists(uiR)) {
        # If ui.R contains a call to shinyUI (which sets .globals$ui), use that.
        # If not, then take the last expression that's returned from ui.R.
        .globals$ui <- NULL
        on.exit(.globals$ui <- NULL, add = FALSE)
        ui <- sourceUTF8(uiR, envir = new.env(parent = sharedEnv))
        if (!is.null(.globals$ui)) {
          ui <- .globals$ui[[1]]
        }
        return(uiHttpHandler(ui))
      } else {
        return(function(req) NULL)
      }
    }
  )
  uiHandler <- function(req) {
    uiHandlerSource()(req)
  }

  wwwDir <- file.path.ci(appDir, "www")
  if (dirExists(wwwDir)) {
    staticPaths <- list("/" = staticPath(wwwDir, indexhtml = FALSE, fallthrough = TRUE))
  } else {
    staticPaths <- list()
  }

  fallbackWWWDir <- system_file("www-dir", package = "shiny")

  serverSource <- cachedFuncWithFile(appDir, "server.R", case.sensitive = FALSE,
    function(serverR) {
      # If server.R contains a call to shinyServer (which sets .globals$server),
      # use that. If not, then take the last expression that's returned from
      # server.R.
      .globals$server <- NULL
      on.exit(.globals$server <- NULL, add = TRUE)
      result <- sourceUTF8(serverR, envir = new.env(parent = sharedEnv))
      if (!is.null(.globals$server)) {
        result <- .globals$server[[1]]
      }
      return(result)
    }
  )

  # This function stands in for the server function, and reloads the
  # real server function as necessary whenever server.R changes
  serverFuncSource <- function() {
    serverFunction <- serverSource()
    if (is.null(serverFunction)) {
      return(function(input, output) NULL)
    } else if (is.function(serverFunction)) {
      # This is what we normally expect; run the server function
      return(serverFunction)
    } else {
      stop("server.R returned an object of unexpected type: ",
        typeof(serverFunction))
    }
  }

  shinyOptions(appDir = appDir)

  oldwd <- NULL
  monitorHandle <- NULL
  onStart <- function() {
    oldwd <<- getwd()
    setwd(appDir)
    # TODO: we should support hot reloading on global.R and R/*.R changes.
    if (getOption("shiny.autoload.r", TRUE)) {
      loadSupport(appDir, renv=sharedEnv, globalrenv=globalenv())
    }  else {
      if (file.exists(file.path.ci(appDir, "global.R")))
        sourceUTF8(file.path.ci(appDir, "global.R"))
    }
    monitorHandle <<- initAutoReloadMonitor(appDir)
  }
  onStop <- function() {
    setwd(oldwd)
    # It is possible that while calling appObj()$onStart() or loadingSupport, an error occured
    # This will cause `onStop` to be called.
    #   The `oldwd` will exist, but `monitorHandle` is not a function yet.
    if (is.function(monitorHandle)) {
      monitorHandle()
      monitorHandle <<- NULL
    }
  }

  structure(
    list(
      staticPaths = staticPaths,
      # Even though the wwwDir is handled as a static path, we need to include
      # it here to be handled by R as well. This is because the special case
      # of index.html: it is specifically not handled as a staticPath for
      # reasons explained above, but if someone does want to serve up an
      # index.html, we need to handle it, and we do it by using the
      # staticHandler in the R code path. (#2380)
      httpHandler = joinHandlers(c(uiHandler, wwwDir, fallbackWWWDir)),
      serverFuncSource = serverFuncSource,
      onStart = onStart,
      onStop = onStop,
      options = options
    ),
    class = "shiny.appobj"
  )
}

# Start a reactive observer that continually monitors dir for changes to files
# that have the extensions: r, htm, html, js, css, png, jpg, jpeg, gif. Case is
# ignored when checking extensions. If any changes are detected, all connected
# Shiny sessions are reloaded.
#
# Use options(shiny.autoreload = TRUE) to enable this behavior. Since monitoring
# for changes is expensive (we are polling for mtimes here, nothing fancy) this
# feature is intended only for development.
#
# You can customize the file patterns Shiny will monitor by setting the
# shiny.autoreload.pattern option. For example, to monitor only ui.R:
# options(shiny.autoreload.pattern = glob2rx("ui.R"))
#
# The return value is a function that halts monitoring when called.
initAutoReloadMonitor <- function(dir) {
  if (!get_devmode_option("shiny.autoreload", FALSE)) {
    return(function(){})
  }

  filePattern <- getOption("shiny.autoreload.pattern",
    ".*\\.(r|html?|js|css|png|jpe?g|gif)$")

  lastValue <- NULL
  observeLabel <- paste0("File Auto-Reload - '", basename(dir), "'")
  obs <- observe(label = observeLabel, {
    files <- sort_c(
      list.files(dir, pattern = filePattern, recursive = TRUE, ignore.case = TRUE)
    )
    times <- file.info(files)$mtime
    names(times) <- files

    if (is.null(lastValue)) {
      # First run
      lastValue <<- times
    } else if (!identical(lastValue, times)) {
      # We've changed!
      lastValue <<- times
      autoReloadCallbacks$invoke()
    }

    invalidateLater(getOption("shiny.autoreload.interval", 500))
  })

  onStop(obs$destroy)

  obs$destroy
}

#' Load an app's supporting R files
#'
#' Loads all of the supporting R files of a Shiny application. Specifically,
#' this function loads any top-level supporting `.R` files in the `R/` directory
#' adjacent to the `app.R`/`server.R`/`ui.R` files.
#'
#' Since Shiny 1.5.0, this function is called by default when running an
#' application. If it causes problems, there are two ways to opt out. You can
#' either place a file named `_disable_autoload.R` in your R/ directory, or
#' set `options(shiny.autoload.r=FALSE)`. If you set this option, it will
#' affect any application that runs later in the same R session, potentially
#' breaking it, so after running your application, you should unset option with
#' `options(shiny.autoload.r=NULL)`
#'
#' @details The files are sourced in alphabetical order (as determined by
#'   [list.files]). `global.R` is evaluated before the supporting R files in the
#'   `R/` directory.
#' @param appDir The application directory. If `appDir` is `NULL` or
#'   not supplied, the nearest enclosing directory that is a Shiny app, starting
#'   with the current directory, is used.
#' @param renv The environment in which the files in the `R/` directory should
#'   be evaluated.
#' @param globalrenv The environment in which `global.R` should be evaluated. If
#'   `NULL`, `global.R` will not be evaluated at all.
#' @export
loadSupport <- function(appDir=NULL, renv=new.env(parent=globalenv()), globalrenv=globalenv()){
  require(shiny)

  if (is.null(appDir)) {
    appDir <- findEnclosingApp(".")
  }

  if (!is.null(globalrenv)){
    # Evaluate global.R, if it exists.
    globalPath <- file.path.ci(appDir, "global.R")
    if (file.exists(globalPath)){
      withr::with_dir(appDir, {
        sourceUTF8(basename(globalPath), envir=globalrenv)
      })
    }
  }


  helpersDir <- file.path(appDir, "R")

  disabled <- list.files(helpersDir, pattern="^_disable_autoload\\.r$", recursive=FALSE, ignore.case=TRUE)
  if (length(disabled) > 0) {
    return(invisible(renv))
  }

  warn_if_app_dir_is_package(appDir)

  helpers <- list.files(helpersDir, pattern="\\.[rR]$", recursive=FALSE, full.names=TRUE)
  # Ensure files in R/ are sorted according to the 'C' locale before sourcing.
  # This convention is based on the default for packages. For details, see:
  # https://cran.r-project.org/doc/manuals/r-release/R-exts.html#The-DESCRIPTION-file
  helpers <- sort_c(helpers)
  helpers <- normalizePath(helpers)

  withr::with_dir(appDir, {
    lapply(helpers, sourceUTF8, envir=renv)
  })

  invisible(renv)
}

warn_if_app_dir_is_package <- function(appDir) {
  has_namespace <- file.exists(file.path.ci(appDir, "NAMESPACE"))
  has_desc_pkg <- FALSE

  if (!has_namespace) {
    descFile <- file.path.ci(appDir, "DESCRIPTION")

    has_desc_pkg <-
      file.exists(descFile) &&
      identical(as.character(read.dcf(descFile, fields = "Type")), "Package")
  }

  if (has_namespace || has_desc_pkg) {
    warning(
      "Loading R/ subdirectory for Shiny application, but this directory appears ",
      "to contain an R package. Sourcing files in R/ may cause unexpected behavior."
    )
  }
}

# This reads in an app dir for a single-file application (e.g. app.R), and
# returns a shiny.appobj.
# appDir must be a normalized (absolute) path, not a relative one
shinyAppDir_appR <- function(fileName, appDir, options=list())
{
  fullpath <- file.path.ci(appDir, fileName)

  # This sources app.R and caches the content. When appObj() is called but
  # app.R hasn't changed, it won't re-source the file. But if called and
  # app.R has changed, it'll re-source the file and return the result.
  appObj <- cachedFuncWithFile(appDir, fileName, case.sensitive = FALSE,
    function(appR) {
      wasDir <- setwd(appDir)
      on.exit(setwd(wasDir))

      # TODO: we should support hot reloading on R/*.R changes.
      # In an upcoming version of shiny, this option will go away.
      if (getOption("shiny.autoload.r", TRUE)) {
        # Create a child env which contains all the helpers and will be the shared parent
        # of the ui.R and server.R load.
        sharedEnv <- new.env(parent = globalenv())
        loadSupport(appDir, renv=sharedEnv, globalrenv=NULL)
      } else {
        sharedEnv <- globalenv()
      }
      result <- sourceUTF8(fullpath, envir = new.env(parent = sharedEnv))

      if (!is.shiny.appobj(result))
        stop("app.R did not return a shiny.appobj object.")

      applyCapturedAppOptions(result$appOptions)

      return(result)
    }
  )

  # A function that invokes the http handler from the appObj in app.R, but
  # since this uses appObj(), it only re-sources the file when it changes.
  dynHttpHandler <- function(...) {
    appObj()$httpHandler(...)
  }

  dynServerFuncSource <- function(...) {
    appObj()$serverFuncSource(...)
  }

  wwwDir <- file.path.ci(appDir, "www")
  if (dirExists(wwwDir)) {
    # wwwDir is a static path served by httpuv. It does _not_ serve up
    # index.html, for two reasons. (1) It's possible that the user's
    # www/index.html file is not actually used as the index, but as a template
    # that gets processed before being sent; and (2) the index content may be
    # modified by the hosting environment (as in SockJSAdapter.R).
    #
    # The call to staticPath normalizes the path, so that if the working dir
    # later changes, it will continue to point to the right place.
    staticPaths <- list("/" = staticPath(wwwDir, indexhtml = FALSE, fallthrough = TRUE))
  } else {
    staticPaths <- list()
  }

  fallbackWWWDir <- system_file("www-dir", package = "shiny")

  oldwd <- NULL
  monitorHandle <- NULL
  onStart <- function() {
    oldwd <<- getwd()
    setwd(appDir)
    if (!is.null(appObj()$onStart)) appObj()$onStart()
    monitorHandle <<- initAutoReloadMonitor(appDir)
    invisible()
  }
  onStop <- function() {
    setwd(oldwd)
    # It is possible that while calling appObj()$onStart() or loadingSupport, an error occured
    # This will cause `onStop` to be called.
    #   The `oldwd` will exist, but `monitorHandle` is not a function yet.
    if (is.function(monitorHandle)) {
      monitorHandle()
      monitorHandle <<- NULL
    }
  }

  appObjOptions <- appObj()$options

  structure(
    list(
      # fallbackWWWDir is _not_ listed in staticPaths, because it needs to
      # come after the uiHandler. It also does not need to be fast, since it
      # should rarely be hit. The order is wwwDir (in staticPaths), then
      # uiHandler, then falbackWWWDir (which is served up by the R
      # staticHandler function).
      staticPaths = staticPaths,
      # Even though the wwwDir is handled as a static path, we need to include
      # it here to be handled by R as well. This is because the special case
      # of index.html: it is specifically not handled as a staticPath for
      # reasons explained above, but if someone does want to serve up an
      # index.html, we need to handle it, and we do it by using the
      # staticHandler in the R code path. (#2380)
      httpHandler = joinHandlers(c(dynHttpHandler, wwwDir, fallbackWWWDir)),
      serverFuncSource = dynServerFuncSource,
      onStart = onStart,
      onStop = onStop,
      options = joinOptions(appObjOptions, options)
    ),
    class = "shiny.appobj"
  )
}


#' Shiny App object
#'
#' Internal methods for the `shiny.appobj` S3 class.
#'
#' @keywords internal
#' @name shiny.appobj
NULL

#' @rdname shiny.appobj
#' @param x Object to convert to a Shiny app.
#' @export
as.shiny.appobj <- function(x) {
  UseMethod("as.shiny.appobj", x)
}

#' @rdname shiny.appobj
#' @export
as.shiny.appobj.shiny.appobj <- function(x) {
  x
}

#' @rdname shiny.appobj
#' @export
as.shiny.appobj.list <- function(x) {
  shinyApp(ui = x$ui, server = x$server)
}

#' @rdname shiny.appobj
#' @export
as.shiny.appobj.character <- function(x) {
  if (identical(tolower(tools::file_ext(x)), "r"))
    shinyAppFile(x)
  else
    shinyAppDir(x)
}

#' @rdname shiny.appobj
#' @export
is.shiny.appobj <- function(x) {
  inherits(x, "shiny.appobj")
}

#' @rdname shiny.appobj
#' @param ... Ignored.
#' @export
print.shiny.appobj <- function(x, ...) {
  runApp(x)
}

# Joins two options objects (i.e. the `options` argument to shinyApp(),
# shinyAppDir(), etc.). The values in `b` should take precedence over the values
# in `a`. Given the current options available, it is safe to throw away any
# values in `a` that are provided in `b`. But in the future, if new options are
# introduced that need to be combined in some way instead of simply overwritten,
# then this will be the place to do it. See the implementations of
# print.shiny.appobj() and runApp() (for the latter, look specifically for
# "findVal()") to determine the set of possible options.
joinOptions <- function(a, b) {
  stopifnot(is.null(a) || is.list(a))
  stopifnot(is.null(b) || is.list(b))

  mergeVectors(a, b)
}

#' @rdname shiny.appobj
#' @method as.tags shiny.appobj
#' @export
as.tags.shiny.appobj <- function(x, ...) {
  # jcheng 06/06/2014: Unfortunate copy/paste between this function and
  # knit_print.shiny.appobj, but I am trying to make the most conservative
  # change possible due to upcoming release.
  opts <- x$options %||% list()
  width <- if (is.null(opts$width)) "100%" else opts$width
  height <- if (is.null(opts$height)) "400" else opts$height

  path <- addSubApp(x)
  deferredIFrame(path, width, height)
}

# Generate subapp iframes in such a way that they will not actually load right
# away. Loading subapps immediately upon app load can result in a storm of
# connections, all of which are contending for the few concurrent connections
# that a browser will make to a specific origin. Instead, we load dummy iframes
# and let the client load them when convenient. (See the initIframes function in
# init_shiny.js.)
deferredIFrame <- function(path, width, height) {
  tags$iframe("data-deferred-src" = path,
    width = width, height = height,
    class = "shiny-frame shiny-frame-deferred"
  )
}
