\name{runUrl}
\alias{runUrl}
\title{Run a Shiny application from a URL}
\usage{
  runUrl(url, filetype = NULL, subdir = NULL, port = 8100,
    launch.browser = getOption("shiny.launch.browser", interactive()))
}
\arguments{
  \item{url}{URL of the application.}

  \item{filetype}{The file type (\code{".zip"},
  \code{".tar"}, or \code{".tar.gz"}. Defaults to the file
  extension taken from the url.}

  \item{subdir}{A subdirectory in the repository that
  contains the app. By default, this function will run an
  app from the top level of the repo, but you can use a
  path such as `\code{"inst/shinyapp"}.}

  \item{port}{The TCP port that the application should
  listen on. Defaults to port 8100.}

  \item{launch.browser}{If true, the system's default web
  browser will be launched automatically after the app is
  started. Defaults to true in interactive sessions only.}
}
\description{
  Download and launch a Shiny application that is hosted at
  a downloadable URL. The Shiny application must be saved
  in a .zip, .tar, or .tar.gz file. The Shiny application
  files must be contained in a subdirectory in the archive.
  For example, the files might be \code{myapp/server.r} and
  \code{myapp/ui.r}.
}
\examples{
\dontrun{
runUrl('https://github.com/rstudio/shiny_example/archive/master.tar.gz')

# Can run an app from a subdirectory in the archive
runUrl("https://github.com/rstudio/shiny_example/archive/master.zip",
 subdir = "inst/shinyapp/")
}
}

