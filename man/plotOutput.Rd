\name{plotOutput}
\alias{plotOutput}
\title{Create an plot output element}
\usage{
  plotOutput(outputId, width = "100\%", height = "400px",
    clickId = NULL, hoverId = NULL, hoverDelay = 300,
    hoverDelayType = c("debounce", "throttle"))
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
  using \code{input$}\emph{\code{clickId}}. The value will
  be a named list or vector with \code{x} and \code{y}
  elements indicating the mouse position in user units.}

  \item{hoverId}{If not \code{NULL}, the plot will send
  coordinates to the server whenever the mouse pauses on
  the plot for more than the number of milliseconds
  determined by \code{hoverTimeout}. This information will
  be The value will be \code{NULL} if the user is not
  hovering, and a named list or vector with \code{x} and
  \code{y} elements indicating the mouse position in user
  units.}

  \item{hoverDelay}{The delay for hovering, in
  milliseconds.}

  \item{hoverDelayType}{The type of algorithm for limiting
  the number of hover events. Use \code{"throttle"} to
  limit the number of hover events to one every
  \code{hoverDelay} milliseconds. Use \code{"debounce"} to
  suspend events while the cursor is moving, and wait until
  the cursor has been at rest for \code{hoverDelay}
  milliseconds before sending an event.}
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

