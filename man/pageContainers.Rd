\name{fluidPage}
\alias{fixedPage}
\alias{fluidPage}
\alias{isPageFluid}
\title{Functions for creating top-level page containers}
\usage{
  fluidPage(..., head = list())

  fixedPage(..., head = list())

  isPageFluid()
}
\arguments{
  \item{head}{Tag or list of tags to be inserted into the
  head of the document (for example, addition of required
  Javascript or CSS resources via \code{tags$script} or
  \code{tags$style})}
}
\value{
  A UI defintion that can be passed to the \link{shinyUI}
  function.
}
\description{
  Functions for creating a top-level application page that
  uses either fluid or fixed layout. Fluid pages scale
  their components in realtime to fill all available
  browser width. Fixed pages scale components at predefined
  size thresholds and limit the total width to 940 pixels.
}
\details{
  The \code{isPageFluid} function can be used within other
  UI definition functions to determine whether the current
  top-level page is using fluid or fixed layout.

  See the documentation on the bootstrap
  \href{http://getbootstrap.com/2.3.2/scaffolding.html#fluidGridSystem}{fluid
  grid system} and
  \href{http://getbootstrap.com/2.3.2/scaffolding.html#gridSystem}{fixed
  grid system} for more details on the differences between
  fluid and fixed pages.
}
\seealso{
  \code{\link{gridContainer}}, \code{\link{gridCol}},
  \code{\link{gridRow}}
}

