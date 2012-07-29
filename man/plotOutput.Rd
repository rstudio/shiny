\name{plotOutput}
\alias{plotOutput}
\title{Create a plot output element}
\usage{
  plotOutput(outputId, width = "100\%", height = "400px")
}
\arguments{
  \item{outputId}{output variable to read the plot from}

  \item{width}{Plot width}

  \item{height}{Plot height}
}
\value{
  A plot output element that can be included in a panel
}
\description{
  Render a \link{reactivePlot} within an application page.
}
\examples{
# Show a plot of the generated distribution
mainPanel(
  plotOutput("distPlot")
)
}

