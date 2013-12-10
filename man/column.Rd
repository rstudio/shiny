\name{column}
\alias{column}
\title{Create a column within a UI definition}
\usage{
  column(width, ..., offset = 0)
}
\arguments{
  \item{width}{The grid width of the column (must be
  between 1 and 12)}

  \item{...}{Elements to include within the column}

  \item{offset}{The number of columns to offset this column
  from the end of the previous column.}
}
\value{
  A column that can be included within a
  \code{columnLayout}, \code{\link{fluidRow}}, or
  \code{\link{fixedRow}}.
}
\description{
  Create a column for use within a
  \code{\link{columnLayout}}, \code{\link{fluidRow}}, or
  \code{\link{fixedRow}}
}

