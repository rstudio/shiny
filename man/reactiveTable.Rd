\name{reactiveTable}
\alias{reactiveTable}
\title{Table Output}
\usage{
  reactiveTable(func, ...)
}
\arguments{
  \item{func}{A function that returns an R object that can
  be used with \code{\link[xtable]{xtable}}.}

  \item{...}{Arguments to be passed through to
  \code{\link[xtable]{xtable}} and
  \code{\link[xtable]{print.xtable}}.}
}
\description{
  Creates a reactive table that is suitable for assigning
  to an \code{output} slot.
}
\details{
  The corresponding HTML output tag should be \code{div}
  and have the CSS class name \code{shiny-html-output}.
}

