\name{titlePanel}
\alias{titlePanel}
\title{Create a panel containing an application title.}
\usage{
  titlePanel(title, windowTitle = title)
}
\arguments{
  \item{title}{An application title to display}

  \item{windowTitle}{The title that should be displayed by
  the browser window.}
}
\description{
  Create a panel containing an application title.
}
\details{
  Calling this function has the side effect of including a
  \code{title} tag within the head.
}
\note{
  The \code{titlePanel} function can only be used within a
  \code{\link{fluidPage}}.
}
\examples{
titlePanel("Hello Shiny!")
}

