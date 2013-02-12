\name{outputOptions}
\alias{outputOptions}
\title{Set options for an output object.}
\usage{
  outputOptions(x, name, ...)
}
\arguments{
  \item{x}{A shinyoutput object (typically \code{output}).}

  \item{name}{The name of an output observer in the
  shinyoutput object.}

  \item{...}{Options to set for the output observer.}
}
\description{
  These are the available options for an output object:
  \itemize{ \item suspendWhenHidden. When \code{TRUE} (the
  default), the output object will be suspended (not
  execute) when it is hidden on the web page. When
  \code{FALSE}, the output object will not suspend when
  hidden, and if it was already hidden and suspended, then
  it will resume immediately. }
}
\examples{
\dontrun{
# Get the list of options for all observers within output
outputOptions(output)

# Disable suspend for output$myplot
outputOptions(output, "myplot", suspendWhenHidden = FALSE)

# Get the list of options for output$myplot
outputOptions(output, "myplot")
}
}

