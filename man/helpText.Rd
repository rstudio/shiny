\name{helpText}
\alias{helpText}
\title{Create a help text element}
\usage{
  helpText(text, ...)
}
\arguments{
  \item{text}{Help text string}

  \item{...}{Additional help text strings}
}
\value{
  A help text element that can be added to a UI definition.
}
\description{
  Create help text which can be added to an input form to
  provide additional explanation or context.
}
\examples{
helpText("Note: while the data view will show only",
         "the specified number of observations, the",
         "summary will be based on the full dataset.")
}

