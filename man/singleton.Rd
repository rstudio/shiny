\name{singleton}
\alias{singleton}
\alias{is.singleton}
\title{Include content only once}
\usage{
singleton(x, value = TRUE)

is.singleton(x)
}
\arguments{
\item{x}{A \code{\link{tag}}, text, \code{\link{HTML}}, or list.}

\item{value}{Whether the object should be a singleton.}
}
\description{
Use \code{singleton} to wrap contents (tag, text, HTML, or lists) that should
be included in the generated document only once, yet may appear in the
document-generating code more than once. Only the first appearance of the
content (in document order) will be used.
}
