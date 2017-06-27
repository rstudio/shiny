# TODO: Subapp global.R

#' Create a Shiny app object
#'
#' These functions create Shiny app objects from either an explicit UI/server
#' pair (\code{shinyApp}), or by passing the path of a directory that contains a
#' Shiny app (\code{shinyAppDir}). You generally shouldn't need to use these
#' functions to create/run applications; they are intended for interoperability
#' purposes, such as embedding Shiny apps inside a \pkg{knitr} document.
#'
#' Normally when this function is used at the R console, the Shiny app object is
#' automatically passed to the \code{print()} function, which runs the app. If
#' this is called in the middle of a function, the value will not be passed to
#' \code{print()} and the app will not be run. To make the app run, pass the app
#' object to \code{print()} or \code{\link{runApp}()}.
#'
#' @param ui The UI definition of the app (for example, a call to
#'   \code{fluidPage()} with nested controls)
#' @param server A server function
#' @param onStart A function that will be called before the app is actually run.
#'   This is only needed for \code{shinyAppObj}, since in the \code{shinyAppDir}
#'   case, a \code{global.R} file can be used for this purpose.
#' @param options Named options that should be passed to the \code{runApp} call
#'   (these can be any of the following: "port", "launch.browser", "host", "quiet",
#'   "display.mode" and "test.mode"). You can also specify \code{width} and
#'   \code{height} parameters which provide a hint to the embedding environment
#'   about the ideal height/width for the app.
#' @param uiPattern A regular expression that will be applied to each \code{GET}
#'   request to determine whether the \code{ui} should be used to handle the
#'   request. Note that the entire request path must match the regular
#'   expression in order for the match to be considered successful.
#' @param enableBookmarking Can be one of \code{"url"}, \code{"server"}, or
#'   \code{"disable"}. This is equivalent to calling the
#'   \code{\link{enableBookmarking}()} function just before calling
#'   \code{shinyApp()}. With the default value (\code{NULL}), the app will
#'   respect the setting from any previous calls to \code{enableBookmarking()}.
#'   See \code{\link{enableBookmarking}} for more information.
#' @param onStop A function that will be called after the app is run and all
#'   sessions have been disconnected.
#' @return An object that represents the app. Printing the object or passing it
#'   to \code{\link{runApp}} will run the app.
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
shinyApp <- function(ui=NULL, server=NULL, onStart=NULL, options=list(),
                     uiPattern="/", enableBookmarking=NULL, onStop=NULL) {
  if (is.null(server)) {
    stop("`server` missing from shinyApp")
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

  onStop <- if (!is.null(onStop)) onStop else getShinyOption("onStop")

  # Store the appDir and bookmarking-related options, so that we can read them
  # from within the app.
  shinyOptions(appDir = getwd())
  appOptions <- consumeAppOptions()

  structure(
    list(
      httpHandler = httpHandler,
      serverFuncSource = serverFuncSource,
      onStart = onStart,
      onStop = onStop,
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
    stop("No Shiny application exists at the path \"", appDir, "\"")
  }

  # In case it's a relative path, convert to absolute (so we're not adversely
  # affected by future changes to the path)
  appDir <- normalizePath(appDir, mustWork = TRUE)

  if (file.exists.ci(appDir, "server.R")) {
    shinyAppDir_serverR(appDir, options = options)
  } else if (file.exists.ci(appDir, "app.R")) {
    shinyAppDir_appR("app.R", appDir, options = options)
  } else {
    stop("App dir must contain either app.R or server.R.")
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
shinyAppDir_serverR <- function(appDir, options=list()) {
  # Most of the complexity here comes from needing to hot-reload if the .R files
  # change on disk, or are created, or are removed.

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
        ui <- sourceUTF8(uiR, envir = new.env(parent = globalenv()))
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
  fallbackWWWDir <- system.file("www-dir", package = "shiny")
  serverSource <- cachedFuncWithFile(appDir, "server.R", case.sensitive = FALSE,
    function(serverR) {
      # If server.R contains a call to shinyServer (which sets .globals$server),
      # use that. If not, then take the last expression that's returned from
      # server.R.
      .globals$server <- NULL
      on.exit(.globals$server <- NULL, add = TRUE)
      result <- sourceUTF8(serverR, envir = new.env(parent = globalenv()))
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
    monitorHandle <<- initAutoReloadMonitor(appDir)
    if (file.exists(file.path.ci(appDir, "global.R")))
      sourceUTF8(file.path.ci(appDir, "global.R"))
  }
  onStop <- function() {
    onStopFunc <- getShinyOption("onStop", default = function() {})
    if (!is(onStopFunc, "function")) warning("`onStop` must be a function")
    else onStopFunc()
    setwd(oldwd)
    monitorHandle()
    monitorHandle <<- NULL
  }

  structure(
    list(
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
  if (!getOption("shiny.autoreload", FALSE)) {
    return(function(){})
  }

  filePattern <- getOption("shiny.autoreload.pattern",
    ".*\\.(r|html?|js|css|png|jpe?g|gif)$")

  lastValue <- NULL
  obs <- observe({
    files <- sort(list.files(dir, pattern = filePattern, recursive = TRUE,
      ignore.case = TRUE))
    times <- file.info(files)$mtime
    names(times) <- files

    if (is.null(lastValue)) {
      # First run
      lastValue <<- times
    } else if (!identical(lastValue, times)) {
      # We've changed!
      lastValue <<- times
      for (session in appsByToken$values()) {
        session$reload()
      }
    }

    invalidateLater(getOption("shiny.autoreload.interval", 500))
  })

  obs$destroy
}

# This reads in an app dir for a single-file application (e.g. app.R), and
# returns a shiny.appobj.
shinyAppDir_appR <- function(fileName, appDir, options=list())
{
  fullpath <- file.path.ci(appDir, fileName)

  # This sources app.R and caches the content. When appObj() is called but
  # app.R hasn't changed, it won't re-source the file. But if called and
  # app.R has changed, it'll re-source the file and return the result.
  appObj <- cachedFuncWithFile(appDir, fileName, case.sensitive = FALSE,
    function(appR) {
      result <- sourceUTF8(fullpath, envir = new.env(parent = globalenv()))

      if (!is.shiny.appobj(result))
        stop("app.R did not return a shiny.appobj object.")

      unconsumeAppOptions(result$appOptions)

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
  fallbackWWWDir <- system.file("www-dir", package = "shiny")

  oldwd <- NULL
  monitorHandle <- NULL
  onStart <- function() {
    oldwd <<- getwd()
    setwd(appDir)
    monitorHandle <<- initAutoReloadMonitor(appDir)
  }
  onStop <- function() {
    if (!is.null(appObj()$onStop)) appObj()$onStop()
    setwd(oldwd)
    monitorHandle()
    monitorHandle <<- NULL
  }

  structure(
    list(
      httpHandler = joinHandlers(c(dynHttpHandler, wwwDir, fallbackWWWDir)),
      serverFuncSource = dynServerFuncSource,
      onStart = onStart,
      onStop = onStop,
      options = options
    ),
    class = "shiny.appobj"
  )
}


#' @rdname shinyApp
#' @param x Object to convert to a Shiny app.
#' @export
as.shiny.appobj <- function(x) {
  UseMethod("as.shiny.appobj", x)
}

#' @rdname shinyApp
#' @export
as.shiny.appobj.shiny.appobj <- function(x) {
  x
}

#' @rdname shinyApp
#' @export
as.shiny.appobj.list <- function(x) {
  shinyApp(ui = x$ui, server = x$server)
}

#' @rdname shinyApp
#' @export
as.shiny.appobj.character <- function(x) {
  if (identical(tolower(tools::file_ext(x)), "r"))
    shinyAppFile(x)
  else
    shinyAppDir(x)
}

#' @rdname shinyApp
#' @export
is.shiny.appobj <- function(x) {
  inherits(x, "shiny.appobj")
}

#' @rdname shinyApp
#' @param ... Additional parameters to be passed to print.
#' @export
print.shiny.appobj <- function(x, ...) {
  opts <- x$options %OR% list()
  opts <- opts[names(opts) %in%
      c("port", "launch.browser", "host", "quiet",
        "display.mode", "test.mode")]

  args <- c(list(x), opts)

  do.call(runApp, args)
}

#' @rdname shinyApp
#' @method as.tags shiny.appobj
#' @export
as.tags.shiny.appobj <- function(x, ...) {
  # jcheng 06/06/2014: Unfortunate copy/paste between this function and
  # knit_print.shiny.appobj, but I am trying to make the most conservative
  # change possible due to upcoming release.
  opts <- x$options %OR% list()
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

#' Knitr S3 methods
#'
#' These S3 methods are necessary to help Shiny applications and UI chunks embed
#' themselves in knitr/rmarkdown documents.
#'
#' @name knitr_methods
#' @param x Object to knit_print
#' @param ... Additional knit_print arguments
NULL

# If there's an R Markdown runtime option set but it isn't set to Shiny, then
# return a warning indicating the runtime is inappropriate for this object.
# Returns NULL in all other cases.
shiny_rmd_warning <- function() {
  runtime <- knitr::opts_knit$get("rmarkdown.runtime")
  if (!is.null(runtime) && runtime != "shiny")
    # note that the RStudio IDE checks for this specific string to detect Shiny
    # applications in static document
    list(structure(
      "Shiny application in a static R Markdown document",
      class = "rmd_warning"))
  else
    NULL
}

#' @rdname knitr_methods
#' @export
knit_print.shiny.appobj <- function(x, ...) {
  opts <- x$options %OR% list()
  width <- if (is.null(opts$width)) "100%" else opts$width
  height <- if (is.null(opts$height)) "400" else opts$height

  runtime <- knitr::opts_knit$get("rmarkdown.runtime")
  if (!is.null(runtime) && runtime != "shiny") {
    # If not rendering to a Shiny document, create a box exactly the same
    # dimensions as the Shiny app would have had (so the document continues to
    # flow as it would have with the app), and display a diagnostic message
    width <- validateCssUnit(width)
    height <- validateCssUnit(height)
    output <- tags$div(
      style=paste("width:", width, "; height:", height, "; text-align: center;",
                  "box-sizing: border-box;", "-moz-box-sizing: border-box;",
                  "-webkit-box-sizing: border-box;"),
      class="muted well",
      "Shiny applications not supported in static R Markdown documents")
  }
  else {
    path <- addSubApp(x)
    output <- deferredIFrame(path, width, height)
  }

  # If embedded Shiny apps ever have JS/CSS dependencies (like pym.js) we'll
  # need to grab those and put them in meta, like in knit_print.shiny.tag. But
  # for now it's not an issue, so just return the HTML and warning.

  knitr::asis_output(htmlPreserve(format(output, indent=FALSE)),
                     meta = shiny_rmd_warning(), cacheable = FALSE)
}

# Let us use a nicer syntax in knitr chunks than literally
# calling output$value <- renderFoo(...) and fooOutput().
#' @rdname knitr_methods
#' @param inline Whether the object is printed inline.
#' @export
knit_print.shiny.render.function <- function(x, ..., inline = FALSE) {
  x <- htmltools::as.tags(x, inline = inline)
  output <- knitr::knit_print(tagList(x))
  attr(output, "knit_cacheable") <- FALSE
  attr(output, "knit_meta") <- append(attr(output, "knit_meta"),
                                      shiny_rmd_warning())
  output
}

# Lets us drop reactive expressions directly into a knitr chunk and have the
# value printed out! Nice for teaching if nothing else.
#' @rdname knitr_methods
#' @export
knit_print.reactive <- function(x, ..., inline = FALSE) {
  renderFunc <- if (inline) renderText else renderPrint
  knitr::knit_print(renderFunc({
    x()
  }), inline = inline)
}

#' Run code after app is run
#'
#' This function allows you to register a callback function that is invoked
#' after your app is run and all sessions have been disconnected.
#' @param fun A function that will be called after the app is run.
#' @export
onStop <- function(fun) {
  shinyOptions(onStop = fun)
}
