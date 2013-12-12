\name{verticalLayout}
\alias{verticalLayout}
\title{Layout UI elements vertically}
\usage{
  verticalLayout(...)
}
\arguments{
  \item{...}{Elements to include within the container}
}
\description{
  Create a container that includes one or more rows of
  content (each element passed to the container will appear
  on it's own line in the UI)
}
\note{
  The \code{verticalLayout} function can only be used
  within a \code{\link{fluidPage}}.
}
\examples{
shinyUI(fluidPage(
  verticalLayout(
    a(href="http://example.com/link1", "Link One"),
    a(href="http://example.com/link2", "Link Two"),
    a(href="http://example.com/link3", "Link Three")
  )
))
}
\seealso{
  \code{\link{fluidPage}}
}

