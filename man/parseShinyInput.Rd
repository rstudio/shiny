\name{parseShinyInput}
\alias{parseShinyInput}
\alias{parseShinyInput.default}
\alias{parseShinyInput.shinyDate}
\alias{parseShinyInput.shinyMatrix}
\title{Provide the new generic for parsing Shiny Input}
\usage{
  parseShinyInput(x, shinysession, name)

  parseShinyInput.default(x, shinysession, name)

  parseShinyInput.shinyMatrix(x, shinysession, name)

  parseShinyInput.shinyDate(x, shinysession, name)
}
\arguments{
  \item{x}{The value of this input as provided by the
  client, deserialized using RJSONIO, then passed through a
  call to \code{\link{structure}} to set the class of the
  object. Note that the call to \code{structure} does
  convert \code{NULL} values to an empty
  \code{\link{list}}.}

  \item{shinysession}{The \code{shinysession} in which the
  input exists.}

  \item{name}{The name of the input}
}
\description{
  Provides an S3 generic to parse incoming Shiny JSON data.
  When defined on a particular class, Shiny will use S3
  dispatch to refine the data passed back from the client
  before making it available in the \code{input} variable
  of the \code{server.R} file.

  Pass the data straight through if no class is provided.

  Takes a list-of-lists and returns a matrix. The lists
  must all be the same length. NULLs and empty lists are
  replaced by NA.
}
\details{
  The \code{type} of a custom Shiny Input widget will be
  deduced using the \code{getType()} JavaScript function on
  the registered Shiny inputBinding. This type will then be
  set as the incoming object's class.

  The provided function will be used to parse the data
  delivered from the client before it is available in the
  \code{input} variable.
}
\examples{
parseShinyInput.myIntegerClass <- function(x, shinysession, name){
  as.integer(x)
}
}

