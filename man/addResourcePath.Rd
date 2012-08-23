\name{addResourcePath}
\alias{addResourcePath}
\title{Resource Publishing}
\usage{
  addResourcePath(prefix, directoryPath)
}
\arguments{
  \item{prefix}{The URL prefix (without slashes). Valid
  characters are a-z, A-Z, 0-9, hyphen, and underscore; and
  must begin with a-z or A-Z. For example, a value of 'foo'
  means that any request paths that begin with '/foo' will
  be mapped to the given directory.}

  \item{directoryPath}{The directory that contains the
  static resources to be served.}
}
\description{
  Adds a directory of static resources to Shiny's web
  server, with the given path prefix. Primarily intended
  for package authors to make supporting JavaScript/CSS
  files available to their components.
}
\details{
  You can call \code{addResourcePath} multiple times for a
  given \code{prefix}; only the most recent value will be
  retained. If the normalized \code{directoryPath} is
  different than the directory that's currently mapped to
  the \code{prefix}, a warning will be issued.
}
\examples{
addResourcePath('datasets', system.file('data', package='datasets'))
}
\seealso{
  \code{\link{singleton}}
}

