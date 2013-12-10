\name{columnLayout}
\alias{columnLayout}
\title{Layout a set of columns}
\usage{
  columnLayout(...)
}
\arguments{
  \item{...}{Columns to include within the layout}
}
\description{
  Layout a set of columns created using the
  \code{\link{column}} function. The widths of the columns
  should total no more than 12 units.
}
\note{
  The \code{columnLayout} function can only be used within
  a \code{\link{fluidPage}}.
}
\examples{
shinyUI(fluidPage(

  titlePanel("New Application"),

  columnLayout(

    # Sidebar with a slider input for number of observations
    column(width = 4,
      wellPanel(
        sliderInput("obs",
                    "Number of observations:",
                    min = 1,
                    max = 1000,
                    value = 500)
      )
    ),

    column(width = 8,
      # Show a plot of the generated distribution
      plotOutput("distPlot")
    )
)
}

