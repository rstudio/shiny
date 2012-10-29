\name{bootstrapPage}
\alias{bootstrapPage}
\title{Create a Twitter Bootstrap page}
\usage{
  bootstrapPage(...)
}
\arguments{
  \item{...}{The contents of the document body.}
}
\value{
  A UI defintion that can be passed to the \link{shinyUI}
  function.
}
\description{
  Create a Shiny UI page that loads the CSS and JavaScript
  for \href{http://getbootstrap.com}{Twitter Bootstrap},
  and has no content in the page body (other than what you
  provide).
}
\details{
  This function is primarily intended for users who are
  proficient in HTML/CSS, and know how to lay out pages in
  Bootstrap. Most users should use template functions like
  \code{\link{pageWithSidebar}}.
}

