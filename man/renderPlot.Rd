\name{renderPlot}
\alias{renderPlot}
\title{Plot Output}
\usage{
  renderPlot(expr, width = "auto", height = "auto",
    res = 72, ..., env = parent.frame(), quoted = FALSE,
    func = NULL)
}
\arguments{
  \item{expr}{An expression that generates a plot.}

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

  \item{res}{Resolution of resulting plot, in pixels per
  inch. This value is passed to \code{\link{png}}. Note
  that this affects the resolution of PNG rendering in R;
  it won't change the actual ppi of the browser.}

  \item{...}{Arguments to be passed through to
  \code{\link[grDevices]{png}}.  These can be used to set
  the width, height, background color, etc.}

  \item{env}{The environment in which to evaluate
  \code{expr}.}

  \item{quoted}{Is \code{expr} a quoted expression (with
  \code{quote()})? This is useful if you want to save an
  expression in a variable.}

  \item{func}{A function that generates a plot (deprecated;
  use \code{expr} instead).}
}
\description{
  Renders a reactive plot that is suitable for assigning to
  an \code{output} slot.
}
\details{
  The corresponding HTML output tag should be \code{div} or
  \code{img} and have the CSS class name
  \code{shiny-plot-output}.
}

