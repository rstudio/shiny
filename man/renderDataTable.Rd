\name{renderDataTable}
\alias{renderDataTable}
\title{Table output with the JavaScript library DataTables}
\usage{
  renderDataTable(expr, options = NULL, searchDelay = 500,
    env = parent.frame(), quoted = FALSE)
}
\arguments{
  \item{expr}{An expression that returns a data frame or a
  matrix.}

  \item{options}{A list of initialization options to be
  passed to DataTables.}

  \item{searchDelay}{The delay for searching, in
  milliseconds (to avoid too frequent search requests).}

  \item{env}{The environment in which to evaluate
  \code{expr}.}

  \item{quoted}{Is \code{expr} a quoted expression (with
  \code{quote()})? This is useful if you want to save an
  expression in a variable.}
}
\description{
  Makes a reactive version of the given function that
  returns a data frame (or matrix), which will be rendered
  with the DataTables library. Paging, searching,
  filtering, and sorting can be done on the R side using
  Shiny as the server infrastructure.
}
\references{
  \url{http://datatables.net}
}

