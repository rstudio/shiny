\name{runGitHub}
\alias{runGitHub}
\title{Run a Shiny application from a GitHub repository}
\usage{
  runGitHub(repo, username = getOption("github.user"),
    ref = "master", subdir = NULL, port = 8100,
    launch.browser = getOption("shiny.launch.browser", interactive()))
}
\arguments{
  \item{repo}{Name of the repository}

  \item{username}{GitHub username}

  \item{ref}{Desired git reference. Could be a commit, tag,
  or branch name. Defaults to \code{"master"}.}

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
  Download and launch a Shiny application that is hosted in
  a GitHub repository.
}
\examples{
\dontrun{
runGitHub("shiny_example", "rstudio")

# Can run an app from a subdirectory in the repo
runGitHub("shiny_example", "rstudio", subdir = "inst/shinyapp/")
}
}

