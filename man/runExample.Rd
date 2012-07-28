\name{runExample}
\alias{runExample}
\title{Run Shiny Example Applications}
\usage{
  runExample(example = NA, port = 8100L,
    launch.browser = getOption("shiny.launch.browser", interactive()))
}
\arguments{
  \item{example}{The name of the example to run, or
  \code{NA} (the default) to list the available examples.}

  \item{port}{The TCP port that the application should
  listen on. Defaults to port 8100.}

  \item{launch.browser}{If true, the system's default web
  browser will be launched automatically after the app is
  started. Defaults to true in interactive sessions only.}
}
\description{
  Launch Shiny example applications, and optionally, your
  system's web browser.
}

