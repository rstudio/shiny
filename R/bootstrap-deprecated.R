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
