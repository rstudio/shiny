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
columnLayout(
  column(4,
    sliderInput("obs", "Number of observations:",
                min = 1, max = 1000, value = 500)
  ),
  column(8,
    plotOutput("distPlot")
  )
)

fluidRow(
  column(width = 4,
    "4"
  ),
  column(width = 3, offset = 2,
    "3 offset 2"
  )
)
}
\seealso{
  \code{\link{column}}, \code{\link{fluidRow}}.
}

