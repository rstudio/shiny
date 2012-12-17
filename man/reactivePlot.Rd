\name{reactivePlot}
\alias{reactivePlot}
\title{Plot Output}
\usage{
  reactivePlot(func, width = "auto", height = "auto", ...)
}
\arguments{
  \item{func}{A function that generates a plot.}

  \item{width}{The width of the rendered plot, in pixels;
  or \code{'auto'} to use the \code{offsetWidth} of the
  HTML element that is bound to this plot.  You can also
  pass in a function that returns the width in pixels or
  \code{'auto'}; in the body of the function you may
  reference reactive values and functions.}

  \item{height}{The height of the rendered plot, in pixels;
  or \code{'auto'} to use the \code{offsetHeight} of the
  HTML element that is bound to this plot.  You can also
  pass in a function that returns the width in pixels or
  \code{'auto'}; in the body of the function you may
  reference reactive values and functions.}

  \item{...}{Arguments to be passed through to
  \code{\link[grDevices]{png}}.  These can be used to set
  the width, height, background color, etc.}
}
\description{
  Creates a reactive plot that is suitable for assigning to
  an \code{output} slot.
}
\details{
  The corresponding HTML output tag should be \code{div} or
  \code{img} and have the CSS class name
  \code{shiny-plot-output}.

  For output, it will try to use the following devices, in
  this order: quartz (via \code{\link[grDevices]{png}}),
  then \code{\link[Cairo]{CairoPNG}}, and finally
  \code{\link[grDevices]{png}}. This is in order of quality
  of output. Notably, plain \code{png} output on Linux and
  Windows may not antialias some point shapes, resulting in
  poor quality output.
}

