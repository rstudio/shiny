\name{navbarPage}
\alias{navbarMenu}
\alias{navbarPage}
\title{Create a page with a top level navigation bar}
\usage{
  navbarPage(title, ..., header = NULL, footer = NULL,
    inverse = FALSE, collapsable = FALSE, fluid = TRUE,
    responsive = TRUE, theme = NULL)

  navbarMenu(title, ..., icon = NULL)
}
\arguments{
  \item{title}{The title to display in the navbar}

  \item{...}{\code{\link{tabPanel}} elements to include in
  the page}

  \item{header}{Tag of list of tags to display as a common
  header above all tabPanels.}

  \item{footer}{Tag or list of tags to display as a common
  footer below all tabPanels}

  \item{inverse}{\code{TRUE} to use a dark background and
  light text for the navigation bar}

  \item{collapsable}{\code{TRUE} to automatically collapse
  the navigation elements into a menu when the width of the
  browser is less than 940 pixels (useful for viewing on
  smaller touchscreen device)}

  \item{fluid}{\code{TRUE} to use a fluid layout.
  \code{FALSE} to use a fixed layout.}

  \item{responsive}{\code{TRUE} to use responsive layout
  (automatically adapt and resize page elements based on
  the size of the viewing device)}

  \item{theme}{Alternative Bootstrap stylesheet (normally a
  css file within the www directory). For example, to use
  the theme located at \code{www/bootstrap.css} you would
  use \code{theme = "bootstrap.css"}.}

  \item{icon}{Optional icon to appear on a
  \code{navbarMenu} tab.}
}
\value{
  A UI defintion that can be passed to the \link{shinyUI}
  function.
}
\description{
  Create a page that contains a top level navigation bar
  that can be used to toggle a set of
  \code{\link{tabPanel}} elements.
}
\details{
  The \code{navbarMenu} function can be used to create an
  embedded menu within the navbar that in turns includes
  additional tabPanels (see example below).
}
\examples{
shinyUI(navbarPage("App Title",
  tabPanel("Plot"),
  tabPanel("Summary"),
  tabPanel("Table")
))

shinyUI(navbarPage("App Title",
  tabPanel("Plot"),
  navbarMenu("More",
    tabPanel("Summary"),
    tabPanel("Table")
  )
))
}
\seealso{
  \code{\link{tabPanel}}, \code{\link{tabsetPanel}}
}

