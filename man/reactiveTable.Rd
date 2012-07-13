\name{reactiveTable}
\alias{reactiveTable}
\title{Creates a reactive table that is suitable for assigning to an \code{output}
slot.}
\usage{
  reactiveTable(func, ...)
}
\arguments{
  \item{func}{A function that returns an R object that can
  be used with \link{xtable::xtable}.}

  \item{...}{Arguments to be passed through to
  \code{\link{xtable::xtable}}.}
}
\description{
  The corresponding HTML output tag should be \code{div}
  and have the CSS class name \code{live-html}.
}

