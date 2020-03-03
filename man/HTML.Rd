\name{HTML}
\alias{HTML}
\title{Mark Characters as HTML}
\usage{
HTML(text, ..., .noWS = NULL)
}
\arguments{
\item{text}{The text value to mark with HTML}

\item{...}{Any additional values to be converted to character and
concatenated together}

\item{.noWS}{Character vector used to omit some of the whitespace that would
normally be written around this HTML. Valid options include \code{before},
\code{after}, and \code{outside} (equivalent to \code{before} and
\code{end}).}
}
\value{
The same value, but marked as HTML.
}
\description{
Marks the given text as HTML, which means the \link{tag} functions will know
not to perform HTML escaping on it.
}
\examples{
el <- div(HTML("I like <u>turtles</u>"))
cat(as.character(el))

}
