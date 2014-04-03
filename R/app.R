# TODO: Showcase mode should work in non-runApp(string) cases
# TODO: Check that workerId works properly in subapps
# TODO: Subapp global.R
# TODO: Ensure that debugger still works (see especially shinyUI and shinyServer
#       changes)
# TODO: Figure out why superzip table view doesn't work


#' Create a Shiny app object
#'
#' These functions create Shiny app objects from either an explicit UI/server
#' pair (\code{shinyApp}), or by passing the path of a directory that
#' contains a Shiny app (\code{shinyAppDir}). You generally shouldn't need to
#' use these functions to create/run applications; they are intended for
#' interoperability purposes, such as embedding Shiny apps inside a \pkg{knitr}
#' document.
#'
#' @param ui The UI definition of the app (for example, a call to
#'   \code{fluidPage()} with nested controls)
#' @param server A server function
#' @param appDir Path to directory that contains a Shiny app (i.e. a server.R
#'   file and either ui.R or www/index.html)
#' @param onStart A function that will be called before the app is actually run.
#'   This is only needed for \code{shinyAppObj}, since in the \code{shinyAppDir}
#'   case, a \code{global.R} file can be used for this purpose.
#' @param options Named options that should be passed to the `runApp` call. You
#'   can also specify \code{width} and \code{height} parameters which provide a
#'   hint to the embedding environment about the ideal height/width for the app.
#' @return An object that represents the app. Printing the object will run the
#'   app.
#'
#' @examples
#' \dontrun{
#' shinyApp(
#'   ui = fluidPage(
#'     numericInput("n", "n", 1),
#'     plotOutput("plot")
#'   ),
#'   server = function(input, output) {
#'     output$plot <- renderPlot( plot(head(cars, input$n)) )
#'   },
#'   options=list(launch.browser = rstudio::viewer)
#' )
#'
#' shinyAppDir(system.file("examples/01_hello", package="shiny"))
#' }
#'
#' @export
shinyApp <- function(ui, server, onStart=NULL, options=list()) {
  httpHandler <- function(req) {
    if (!identical(req$REQUEST_METHOD, 'GET'))
      return(NULL)

    if (req$PATH_INFO != '/')
      return(NULL)

    textConn <- textConnection(NULL, "w")
    on.exit(close(textConn))

    uiValue <- if (is.function(ui)) {
      ui()
    } else {
      ui
    }
    renderPage(uiValue, textConn)
    html <- paste(textConnectionValue(textConn), collapse='\n')
    return(httpResponse(200, content=html))
  }

  serverFuncSource <- function() {
    server
  }

  structure(
    list(
      httpHandler = httpHandler,
      serverFuncSource = serverFuncSource,
      onStart = onStart,
      options = options),
    class = "shiny.appobj"
  )
}

#' Create a Shiny app object from an existing app directory
#' @export
shinyAppDir <- function(appDir, options=list()) {
  # Most of the complexity here comes from needing to hot-reload if the .R files
  # change on disk, or are created, or are removed.

  # In case it's a relative path, convert to absolute (so we're not adversely
  # affected by future changes to the path)
  appDir <- normalizePath(appDir, mustWork = TRUE)

  # uiHandlerSource is a function that returns an HTTP handler for serving up
  # ui.R as a webpage. The "cachedFuncWithFile" call makes sure that the closure
  # we're creating here only gets executed when ui.R's contents change.
  uiHandlerSource <- cachedFuncWithFile(appDir, "ui.R", case.sensitive = FALSE,
    function() {
      # Have to use file.path.ci every time in case the case of ui.R has
      # changed. (Hmmm, overengineering a bit?)
      uiR <- file.path.ci(appDir, "ui.R")
      if (file.exists(uiR)) {
        ui <- source(uiR,
          local = new.env(parent = globalenv()),
          keep.source = TRUE)$value
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
  serverSource <- cachedSource(appDir, "server.R", case.sensitive = FALSE)

  # This function stands in for the server function, and reloads the
  # real server function as necessary whenever server.R changes
  serverFuncSource <- function() {
    serverFunction <- serverSource(
      local = new.env(parent = globalenv()),
      keep.source = TRUE)$value
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

  oldwd <- NULL
  onStart <- function() {
    oldwd <<- getwd()
    setwd(appDir)
    if (file.exists(file.path.ci(appDir, "global.R")))
      source(file.path.ci(appDir, "global.R"), keep.source = TRUE)
  }
  onEnd <- function() {
    setwd(oldwd)
  }

  structure(
    list(
      httpHandler = joinHandlers(c(uiHandler, wwwDir)),
      serverFuncSource = serverFuncSource,
      onStart = onStart,
      onEnd = onEnd,
      options = options),
    class = "shiny.appobj"
  )
}

#' @export
as.shiny.appobj <- function(x) {
  UseMethod("as.shiny.appobj", x)
}

#' @export
as.shiny.appobj.shiny.appobj <- function(x) {
  x
}

#' @export
as.shiny.appobj.list <- function(x) {
  shinyApp(ui = x$ui, server = x$server)
}

#' @export
as.shiny.appobj.character <- function(x) {
  shinyAppDir(x)
}

#' Run a Shiny app object
#'
#' @param x A Shiny app, as returned from \code{\link{shinyAppObj}} or
#'   \code{\link{shinyAppDir}}.
#'
#' @export
print.shiny.appobj <- function(x, ...) {
  opts <- attr(x, "shiny.options")
  opts <- opts[names(opts) %in%
      c("port", "launch.browser", "host", "quiet", "display.mode")]

  args <- c(list(x), opts)

  do.call(runApp, args)
}

#' @rdname print.shiny.appobj
#' @export
print.shiny.appdir <- print.shiny.appobj

#' @export
knit_print.shiny.appobj <- function(x) {
  path <- addSubApp(x, "")
  opts <- attr(x, "shiny.options")
  width <- if (is.null(opts$width)) "100%" else opts$width
  height <- if (is.null(opts$height)) "400" else opts$height
  iframe <- tags$iframe(src=path, width=width, height=height)
  knitr::asis_output(format(iframe))
}

#' @export
knit_print.shiny.tag <- function(x) {
  knitr::asis_output(format(x))
}

#' @export
knit_print.shiny.tag.list <- knit_print.shiny.tag

#' @export
runRmdContainer <- function(input, text = NULL, ..., knit.options = list()) {
  appdir <- tempfile()
  dir.create(appdir)
  on.exit(unlink(appdir, recursive = TRUE), add = TRUE)

  wwwdir <- file.path(appdir, "www")
  dir.create(wwwdir)

  if (missing(input))
    input <- NULL
  output <- file.path(wwwdir, "index.html")
  knitArgs <- c(list(
    input = input, text = text,
    output = if (!is.null(text)) NULL else output
  ), knit.options)

  result <- do.call(knitr::knit2html, knitArgs)
  if (!is.null(text))
    writeLines(result, output)

  writeLines("shinyServer(function(input, output) NULL)",
    file.path(appdir, "server.R"))

  runApp(appdir, ...)
}

#' @export
runReactiveDoc <- function(input) {
  inputFile <- input

  ui <- fluidPage(
    uiOutput("__reactivedoc__")
  )

  server <- function(input, output, session) {
    doc <- knit2html(text=readLines(inputFile), fragment.only=TRUE,
      quiet = TRUE)

    output$`__reactivedoc__` <- renderUI({
      HTML(doc)
    })
  }

  shinyAppObj(ui = ui, server = server)
}
