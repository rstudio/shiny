\name{runGist}
\alias{runGist}
\title{Run a Shiny application from https://gist.github.com}
\usage{
  runGist(gist, port = 8100L,
    launch.browser = getOption("shiny.launch.browser", interactive()))
}
\arguments{
  \item{gist}{The identifier of the gist. For example, if
  the gist is https://gist.github.com/jcheng5/3239667, then
  \code{3239667}, \code{'3239667'}, and
  \code{'https://gist.github.com/jcheng5/3239667'} are all
  valid values.}

  \item{port}{The TCP port that the application should
  listen on. Defaults to port 8100.}

  \item{launch.browser}{If true, the system's default web
  browser will be launched automatically after the app is
  started. Defaults to true in interactive sessions only.}
}
\description{
  Download and launch a Shiny application that is hosted on
  GitHub as a gist.
}
\examples{
\dontrun{
runGist(3239667)
runGist("https://gist.github.com/jcheng5/3239667")

# Old URL format without username
runGist("https://gist.github.com/3239667")
}
}

