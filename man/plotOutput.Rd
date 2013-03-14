\name{plotOutput}
\alias{plotOutput}
\title{Create an plot output element}
\usage{
  plotOutput(outputId, width = "100\%", height = "400px")
}
\arguments{
  \item{outputId}{output variable to read the plot from}

  \item{width}{Plot width. Must be a valid CSS unit (like
  \code{"100\%"}, \code{"400px"}, \code{"auto"}) or a
  number, which will be coerced to a string and have
  \code{"px"} appended.}

  \item{height}{Plot height}
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

