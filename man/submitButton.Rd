\name{submitButton}
\alias{submitButton}
\title{Create a submit button}
\usage{
  submitButton(text = "Apply Changes")
}
\arguments{
  \item{text}{Button caption}
}
\value{
  A submit button that can be added to a UI definition.
}
\description{
  Create a submit button for an input form. Forms that
  include a submit button do not automatically update their
  outputs when inputs change, rather they wait until the
  user explicitly clicks the submit button.
}
\examples{
submitButton("Update View")
}

