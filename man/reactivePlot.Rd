\name{reactivePlot}
\alias{reactivePlot}
\title{Creates a reactive plot that is suitable for assigning to an \code{output}
slot.}
\usage{
  reactivePlot(func, ...)
}
\arguments{
  \item{func}{A function that generates a plot.}

  \item{...}{Arguments to be passed through to
  \code{\link{grDevices::png}}.  These can be used to set
  the width, height, background color, etc.}
}
\description{
  The corresponding HTML output tag should be \code{div} or
  \code{img} and have the CSS class name \code{live-plot}.
}

