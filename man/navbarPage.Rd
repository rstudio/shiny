\name{navbarPage}
\alias{navbarPage}
\title{Create a page with a top level navigation bar}
\usage{
  navbarPage(title, ..., inverse = FALSE,
    windowTitle = title)
}
\arguments{
  \item{title}{The application title to display in the
  navbar}

  \item{...}{\code{\link{tabPanel}} elements to include in
  the page}

  \item{inverse}{\code{TRUE} to use a dark background and
  light text for the navigation bar}

  \item{windowTitle}{The title that should be displayed by
  the browser window.  Useful if \code{title} is not a
  string.}
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
\examples{
shinyUI(navbarPage("App Title",
  tabPanel("Plot"),
  tabPanel("Summary"),
  tabPanel("Table")
))
}
\seealso{
  \code{\link{tabPanel}}, \code{\link{tabsetPanel}}
}

