\name{bootstrapPage}
\alias{basicPage}
\alias{bootstrapPage}
\title{Create a Twitter Bootstrap page}
\usage{
  bootstrapPage(...)

  basicPage(...)
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
  These functions are primarily intended for users who are
  proficient in HTML/CSS, and know how to lay out pages in
  Bootstrap. Most users should use template functions like
  \code{\link{pageWithSidebar}}.

  \code{basicPage} is the same as \code{bootstrapPage},
  with an added \code{<div class="container-fluid">}
  wrapper to provide a little padding.
}

