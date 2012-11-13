\name{headerPanel}
\alias{headerPanel}
\title{Create a header panel}
\usage{
  headerPanel(title, windowTitle = title)
}
\arguments{
  \item{title}{An application title to display}

  \item{windowTitle}{The title that should be displayed by
  the browser window.  Useful if \code{title} is not a
  string.}
}
\value{
  A headerPanel that can be passed to
  \link{pageWithSidebar}
}
\description{
  Create a header panel containing an application title.
}
\examples{
headerPanel("Hello Shiny!")
}

