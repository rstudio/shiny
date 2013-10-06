\name{runExample}
\alias{runExample}
\title{Run Shiny Example Applications}
\usage{
  runExample(example = NA, port = NULL,
    launch.browser = getOption("shiny.launch.browser", interactive()))
}
\arguments{
  \item{example}{The name of the example to run, or
  \code{NA} (the default) to list the available examples.}

  \item{port}{The TCP port that the application should
  listen on. Defaults to choosing a random port.}

  \item{launch.browser}{If true, the system's default web
  browser will be launched automatically after the app is
  started. Defaults to true in interactive sessions only.}
}
\description{
  Launch Shiny example applications, and optionally, your
  system's web browser.
}
\examples{
\dontrun{
# List all available examples
runExample()

# Run one of the examples
runExample("01_hello")

# Print the directory containing the code for all examples
system.file("examples", package="shiny")
}
}

