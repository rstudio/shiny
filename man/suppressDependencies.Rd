\name{suppressDependencies}
\alias{suppressDependencies}
\title{Suppress web dependencies}
\usage{
suppressDependencies(...)
}
\arguments{
\item{...}{Names of the dependencies to suppress. For example,
\code{"jquery"} or \code{"bootstrap"}.}
}
\description{
This suppresses one or more web dependencies. It is meant to be used when a
dependency (like a JavaScript or CSS file) is declared in raw HTML, in an
HTML template.
}
\seealso{
\code{\link{htmlTemplate}} for more information about using HTML
  templates.

\code{\link[htmltools]{htmlDependency}}
}
