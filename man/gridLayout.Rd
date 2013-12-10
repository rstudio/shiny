\name{gridContainer}
\alias{gridCol}
\alias{gridContainer}
\alias{gridRow}
\title{Functions for creating bootstrap grid layouts}
\usage{
  gridContainer(..., fluid = isPageFluid())

  gridRow(..., fluid = isPageFluid())

  gridCol(width, ..., offset = NA)
}
\arguments{
  \item{fluid}{Whether to use fluid layout for the grid.
  Automatically defaults to the appropriate value for the
  current page being built.}

  \item{width}{The grid width of the column (must be
  between 1 and 12)}

  \item{offset}{The number of columns to offset this column
  from the end of the previous column.}
}
\value{
  A grid container or element that can be added to a UI
  definition.
}
\description{
  Functions for creating bootstrap grid layouts
}
\details{
  See the documentation on the bootstrap
  \href{http://getbootstrap.com/2.3.2/scaffolding.html#fluidGridSystem}{fluid
  grid system} and
  \href{http://getbootstrap.com/2.3.2/scaffolding.html#gridSystem}{fixed
  grid system} for more details on using containers, rows,
  and columns to layout pages.
}
\seealso{
  \code{\link{fluidPage}}, \code{\link{fixedPage}}
}

