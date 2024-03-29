% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shinyui.R
\name{shinyUI}
\alias{shinyUI}
\title{Create a Shiny UI handler}
\usage{
shinyUI(ui)
}
\arguments{
\item{ui}{A user interface definition}
}
\value{
The user interface definition, without modifications or side effects.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#superseded}{\figure{lifecycle-superseded.svg}{options: alt='[Superseded]'}}}{\strong{[Superseded]}}

Historically this function was used in ui.R files to register a user
interface with Shiny. It is no longer required as of Shiny 0.10; simply
ensure that the last expression to be returned from ui.R is a user interface.
This function is kept for backwards compatibility with older applications. It
returns the value that is passed to it.
}
\keyword{internal}
