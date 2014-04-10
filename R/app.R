# TODO: Subapp global.R

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

#' @rdname shinyApp
#' @param appDir Path to directory that contains a Shiny app (i.e. a server.R
#'   file and either ui.R or www/index.html)
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
  shinyAppDir(x)
}

#' @rdname shinyApp
#' @param ... Additional parameters to be passed to print.
#' @export
print.shiny.appobj <- function(x, ...) {
  opts <- x$options %OR% list()
  opts <- opts[names(opts) %in%
      c("port", "launch.browser", "host", "quiet", "display.mode")]

  args <- c(list(x), opts)

  do.call(runApp, args)
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

#' @rdname knitr_methods
#' @export
knit_print.shiny.appobj <- function(x, ...) {
  path <- addSubApp(x)
  opts <- x$options %OR% list()
  width <- if (is.null(opts$width)) "100%" else opts$width
  height <- if (is.null(opts$height)) "400" else opts$height
  iframe <- tags$iframe(class="shiny-frame", src=path,
                        width=width, height=height)
  knitr::asis_output(format(iframe))
}

#' @rdname knitr_methods
#' @export
knit_print.shiny.tag <- function(x, ...) {
  knitr::asis_output(format(x))
}

#' @rdname knitr_methods
#' @export
knit_print.shiny.tag.list <- knit_print.shiny.tag


# Lets us use a nicer syntax in knitr chunks than literally
# calling output$value <- renderFoo(...) and fooOutput().
#' @rdname knitr_methods
#' @export
knit_print.shiny.render.function <- function(x, ...) {
  outputFunction <- attr(x, "outputFunc")
  id <- shiny:::createUniqueId(8)
  o <- getDefaultReactiveDomain()$output
  o[[id]] <- x
  knit_print(outputFunction(id))
}
