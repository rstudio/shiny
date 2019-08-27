\name{include}
\alias{include}
\alias{includeHTML}
\alias{includeText}
\alias{includeMarkdown}
\alias{includeCSS}
\alias{includeScript}
\title{Include Content From a File}
\usage{
includeHTML(path)

includeText(path)

includeMarkdown(path)

includeCSS(path, ...)

includeScript(path, ...)
}
\arguments{
\item{path}{The path of the file to be included. It is highly recommended to
use a relative path (the base path being the Shiny application directory),
not an absolute path.}

\item{...}{Any additional attributes to be applied to the generated tag.}
}
\description{
Load HTML, text, or rendered Markdown from a file and turn into HTML.
}
\details{
These functions provide a convenient way to include an extensive amount of
HTML, textual, Markdown, CSS, or JavaScript content, rather than using a
large literal R string.
}
\note{
\code{includeText} escapes its contents, but does no other processing.
  This means that hard breaks and multiple spaces will be rendered as they
  usually are in HTML: as a single space character. If you are looking for
  preformatted text, wrap the call with \code{\link{pre}}, or consider using
  \code{includeMarkdown} instead.

The \code{includeMarkdown} function requires the \code{markdown}
  package.
}
