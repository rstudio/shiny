\name{startApp}
\alias{startApp}
\title{Creates a new app with the given properties.}
\usage{
  startApp(client = "./www", server = "./app.R",
    sys.www.root = system.file("www", package = "shiny"),
    port = 8101L)
}
\arguments{
  \item{client}{Path to the root of the
  application-specific www files (which should include
  index.html); or, a function that knows how to serve up
  www files (TODO: document); or, a list of one or more
  paths and/or functions.}

  \item{server}{If a character string, a path to the R file
  that contains the server application logic. If a
  function, the actual server application logic (should
  take \code{input} and \code{output} parameters).}

  \item{sys.www.root}{Path to the system www root, that is,
  the assets that are shared by all Shiny applications
  (shiny.css, shiny.js, etc.).}

  \item{port}{The TCP port that the application should
  listen on.}
}
\description{
  Creates a new app with the given properties.
}

