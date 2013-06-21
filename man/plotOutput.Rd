\name{plotOutput}
\alias{plotOutput}
\title{Create an plot output element}
\usage{
  plotOutput(outputId, width = "100\%", height = "400px",
    clickId = NULL, hoverId = NULL)
}
\arguments{
  \item{outputId}{output variable to read the plot from}

  \item{width}{Plot width. Must be a valid CSS unit (like
  \code{"100\%"}, \code{"400px"}, \code{"auto"}) or a
  number, which will be coerced to a string and have
  \code{"px"} appended.}

  \item{height}{Plot height}

  \item{clickId}{If not \code{NULL}, the plot will send
  coordinates to the server whenever it is clicked. This
  information will be accessible on the \code{input} object
  using \code{input$\emph{clickId}}. The value will be a
  named list or vector with \code{x} and \code{y} elements
  indicating the mouse position in user units.}

  \item{hoverId}{If not \code{NULL}, the plot will send
  coordinates to the server whenever the mouse pauses on
  the plot for more than 500 milliseconds. This information
  will be accessible on the \code{input} object using
  \code{input$\emph{clickId}}. The value will be
  \code{NULL} if the user is not hovering, and a named list
  or vector with \code{x} and \code{y} elements indicating
  the mouse position in user units.}
}
\value{
  A plot output element that can be included in a panel
}
\description{
  Render a \link{renderPlot} within an application page.
}
\examples{
# Show a plot of the generated distribution
mainPanel(
  plotOutput("distPlot")
)
}

