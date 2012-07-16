\name{runApp}
\alias{runApp}
\title{Run an application. This function normally does not return.}
\usage{
  runApp(app = "./app.R", www.root = "./www",
    sys.www.root = system.file("www", package = "shiny"),
    port = 8101L, launch.browser = interactive())
}
\arguments{
  \item{app}{If a character string, a path to the R file
  that contains the server application logic. If a
  function, the actual server application logic (should
  take \code{input} and \code{output} parameters).}

  \item{www.root}{Path to the root of the
  application-specific www files (which should include
  index.html).}

  \item{sys.www.root}{Path to the system www root, that is,
  the assets that are shared by all Shiny applications
  (shiny.css, shiny.js, etc.).}

  \item{port}{The TCP port that the application should
  listen on.}

  \item{launch.browser}{If true, the system's default web
  browser will be launched automatically after the app is
  started. Defaults to true in interactive sessions only.}
}
\description{
  Run an application. This function normally does not
  return.
}

