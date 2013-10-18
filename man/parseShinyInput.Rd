\name{parseShinyInput}
\alias{parseShinyInput}
\title{Provide the new generic for parsing Shiny Input}
\usage{
  parseShinyInput(x, ...)
}
\description{
  Provides an S3 generic to parse incoming Shiny JSON data.
  When defined on a particular class, Shiny will use S3
  dispatch to refine the data passed back from the client
  before making it available in the \code{input} variable
  of the \code{server.R} file.
}
\details{
  The \code{type} of a custom Shiny Input widget will be
  deduced using the \code{getType()} JavaScript function on
  the registered Shiny inputBinding. This type will then be
  set as the incoming object's class.

  The provided function will be used to parse the data
  delivered from the client before it is available in the
  \code{input} variable. The function will be called with
  the following three parameters: \enumerate{ \item{The
  value of this input as provided by the client,
  deserialized using RJSONIO, then passed through a call to
  \code{\link{structure}} to set the class of the object.
  Note that the call to \code{structure} does convert
  \code{NULL} values to an empty \code{\link{list}}.}
  \item{The \code{shinysession} in which the input exists.}
  \item{The name of the input.} }
}
\examples{
parseShinyInput.myNonNullClass <- function(x, ...){
  # Check for an empty list (the NULL equivalent after setting a class using
  # `structure`).
  ifelse((is.list(val) && length(val) == 0), NA, val)
}
}

