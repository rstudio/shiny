\name{fixedPage}
\alias{fixedPage}
\alias{fixedRow}
\title{Create a page with a fixed layout}
\usage{
  fixedPage(..., head = list())

  fixedRow(...)
}
\arguments{
  \item{...}{Elements to include within the page}

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
  Functions for creating fixed page layouts. A fixed page
  layout consists of rows which in turn include columns.
  Rows exist for the purpose of making sure their elements
  appear on the same line (if the browser has adequate
  width). Columns exist for the purpose of defining how
  much horizontal space within a 12-unit wide grid it's
  elements should occupy. Fixed pages limit their width to
  940 pixels on a typical display, and 724px or 1170px on
  smaller and larger displays respectively.
}
\details{
  To create a fixed page use the \code{fixedPage} function
  and include instances of \code{fixedRow} and
  \code{\link{column}} within it. Note that unlike
  \code{\link{fluidPage}}, fixed pages cannot make use of
  higher-level layout functions like \code{sidebarLayout},
  rather, all layout must be done with \code{fixedRow} and
  \code{column}.
}
\note{
  See the documentation on the bootstrap
  \href{http://getbootstrap.com/2.3.2/scaffolding.html#gridSystem}{
  fixed grid system} for additional details.
}
\examples{
shinyUI(fixedPage(
  fixedRow(
    column(width = 4,
      "4"
    ),
    column(width = 3, offset = 2,
      "3 offset 2"
    )
  )
))
}
\seealso{
  \code{\link{column}}
}

