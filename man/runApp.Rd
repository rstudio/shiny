\name{runApp}
\alias{runApp}
\title{Run Shiny Application}
\usage{
  runApp(appDir = getwd(), port = 8100L,
    launch.browser = getOption("shiny.launch.browser", interactive()))
}
\arguments{
  \item{appDir}{The directory of the application. Should
  contain \code{server.R}, plus, either \code{ui.R} or a
  \code{www} directory that contains the file
  \code{index.html}. Defaults to the working directory.}

  \item{port}{The TCP port that the application should
  listen on. Defaults to port 8100.}

  \item{launch.browser}{If true, the system's default web
  browser will be launched automatically after the app is
  started. Defaults to true in interactive sessions only.}
}
\description{
  Runs a Shiny application. This function normally does not
  return; interrupt R to stop the application (usually by
  pressing Ctrl+C or Esc).
}

