\name{fluidContainer}
\alias{fixedContainer}
\alias{fixedRow}
\alias{fluidContainer}
\alias{fluidRow}
\title{Functions for creating bootstrap grid layouts}
\usage{
  fluidContainer(...)

  fluidRow(...)

  fixedContainer(...)

  fixedRow(...)
}
\arguments{
  \item{...}{Elements to include within the grid}
}
\value{
  A bootstrap grid layout that can be added to a UI
  definition.
}
\description{
  Functions for creating a bootstrap fluid or fixed grid
  layout. Fluid layouts scale their components in realtime
  to fill all available browser width. Fixed layouts scale
  components at predefined size thresholds and limit the
  total width to 940 pixels.
}
\details{
  A bootstrap grid layout consists of containers that
  include rows which in turn include columns. Rows exist
  for the purpose of making sure their elements appear on
  the same line (if the browser has adequate width).
  Columns exist for the purpose of defining how much
  horizontal space within a 12-unit wide grid it's elements
  should occupy.

  To create a fluid grid layout use the
  \code{fluidContainer} function and include instances of
  \code{fluidRow} within it. To create a fixed grid layout
  use the \code{fixedContainer} function includes instances
  of \code{fixedRow} within it.
}
\note{
  See the documentation on the bootstrap
  \href{http://getbootstrap.com/2.3.2/scaffolding.html#fluidGridSystem}{fluid
  grid system} and
  \href{http://getbootstrap.com/2.3.2/scaffolding.html#gridSystem}{fixed
  grid system} for more details on the differences between
  fluid and fixed layouts.
}
\examples{
shinyUI(bootstrapPage(
  fluidContainer(
    fluidRow(
      column(width = 4,
        "4"
      ),
      column(width = 3, offset = 2,
        "3 offset 2"
      )
    )
 )
))
}
\seealso{
  \code{\link{bootstrapPage}}, \code{\link{column}}
}

