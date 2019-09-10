#' Create a page with a sidebar
#'
#' **DEPRECATED**: use [fluidPage()] and [sidebarLayout()] instead.
#'
#' @param headerPanel The [headerPanel] with the application title
#' @param sidebarPanel The [sidebarPanel] containing input controls
#' @param mainPanel The [mainPanel] containing outputs
#' @keywords internal
#' @return A UI defintion that can be passed to the [shinyUI] function
#' @export
pageWithSidebar <- function(headerPanel,
                            sidebarPanel,
                            mainPanel) {

  bootstrapPage(
    # basic application container divs
    div(
      class="container-fluid",
      div(class="row",
          headerPanel
      ),
      div(class="row",
          sidebarPanel,
          mainPanel
      )
    )
  )
}

#' Create a header panel
#'
#' **DEPRECATED**: use [titlePanel()] instead.
#'
#' @param title An application title to display
#' @param windowTitle The title that should be displayed by the browser window.
#'   Useful if `title` is not a string.
#' @return A headerPanel that can be passed to [pageWithSidebar]
#' @keywords internal
#' @export
headerPanel <- function(title, windowTitle=title) {
  tagList(
    tags$head(tags$title(windowTitle)),
    div(class="col-sm-12",
      h1(title)
    )
  )
}


#' Create a Bootstrap page
#'
#' **DEPRECATED**: use [fluidPage()] instead.
#'
#' @param ... The contents of the document body.
#' @param title The browser window title (defaults to the host URL of the page)
#' @param responsive This option is deprecated; it is no longer optional with
#'   Bootstrap 3.
#' @param theme Alternative Bootstrap stylesheet (normally a css file within the
#'   www directory, e.g. `www/bootstrap.css`)
#'
#' @return A UI defintion that can be passed to the [shinyUI] function.
#'
#' @keywords internal
#' @seealso [fluidPage()], [fixedPage()]
#' @export
bootstrapPage <- function(..., title = NULL, responsive = NULL, theme = NULL) {

  if (!is.null(responsive)) {
    shinyDeprecated("The 'responsive' argument is no longer used with Bootstrap 3.")
  }

  attachDependencies(
    tagList(
      if (!is.null(title)) tags$head(tags$title(title)),
      if (!is.null(theme)) {
        tags$head(tags$link(rel="stylesheet", type="text/css", href = theme))
      },

      # remainder of tags passed to the function
      list(...)
    ),
    bootstrapLib()
  )
}


#' @rdname bootstrapPage
#' @export
basicPage <- function(...) {
  bootstrapPage(div(class="container-fluid", list(...)))
}
