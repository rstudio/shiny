\name{runGist}
\alias{runGist}
\title{Run a Shiny application from https://gist.github.com}
\usage{
  runGist(gist, port = 8100L,
    launch.browser = getOption("shiny.launch.browser", interactive()))
}
\arguments{
  \item{gist}{The identifier of the gist. For example, if
  the gist is https://gist.github.com/3239667, then
  \code{3239667}, \code{'3239667'}, and
  \code{'https://gist.github.com/3239667'} are all valid
  values.}
}
\description{
  Download and launch a Shiny application that is hosted on
  GitHub as a gist.
}
\note{
  Requires the \code{RCurl} package.
}

