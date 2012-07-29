\name{shinyUI}
\alias{shinyUI}
\title{Create a Shiny UI handler}
\usage{
  shinyUI(ui, path = "/")
}
\arguments{
  \item{ui}{A user-interace definition}

  \item{path}{The web server path to server the UI from}
}
\value{
  Called for its side-effect of registering a UI handler
}
\description{
  Register a UI handler by providing a UI definition
  (created with e.g. \link{pageWithSidebar}) and web server
  path (typically "/", the default value).
}
\examples{
el <- div(HTML("I like <u>turtles</u>"))
cat(as.character(el))
}

