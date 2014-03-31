#' Create a Shiny app object
#'
#' These functions create Shiny app objects from either an explicit UI/server
#' pair (\code{shinyAppObj}), or by passing the path of a directory that
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
#' shinyAppObj(
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
shinyAppObj <- function(ui, server, onStart=NULL, options=list()) {
  structure(
    list(ui=ui, server=server, onStart=onStart),
    shiny.options = options,
    class = "shiny.appobj"
  )
}

#' @rdname shinyAppObj
#' @export
shinyAppDir <- function(dir, options=list()) {
  dir <- normalizePath(dir, mustWork = TRUE)
  structure(
    dir,
    shiny.options = options,
    class = "shiny.appdir"
  )
}

#' Run a Shiny app object
#'
#' @param x A Shiny app, as returned from \code{\link{shinyAppObj}} or
#'   \code{\link{shinyAppDir}}.
#'
#' @export
print.shiny.appobj <- function(x) {
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
  path <- addSubAppObj(x)
  opts <- attr(x, "shiny.options")
  width <- if (is.null(opts$width)) "100%" else opts$width
  height <- if (is.null(opts$height)) "400" else opts$height
  iframe <- tags$iframe(src=path, width=width, height=height)
  knitr::asis_output(format(iframe))
}

#' @export
knit_print.shiny.appdir <- function(x) {
  path <- addSubAppDir(x)
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
