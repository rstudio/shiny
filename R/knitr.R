#' Knitr S3 methods
#'
#' These S3 methods are necessary to help Shiny applications and UI chunks embed
#' themselves in knitr/rmarkdown documents.
#'
#' @name knitr_methods
#' @keywords internal
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
knit_print.shiny.appobj <- function(x, ...) {
  opts <- x$options %||% list()
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
knit_print.shiny.render.function <- function(x, ..., inline = FALSE) {
  x <- htmltools::as.tags(x, inline = inline)
  output <- knitr::knit_print(tagList(x), ..., inline = inline)
  attr(output, "knit_cacheable") <- FALSE
  attr(output, "knit_meta") <- append(attr(output, "knit_meta"),
                                      shiny_rmd_warning())
  output
}

# Lets us drop reactive expressions directly into a knitr chunk and have the
# value printed out! Nice for teaching if nothing else.
#' @rdname knitr_methods
knit_print.reactive <- function(x, ..., inline = FALSE) {
  renderFunc <- if (inline) renderText else renderPrint
  knitr::knit_print(renderFunc({
    x()
  }), ..., inline = inline)
}
