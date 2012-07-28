\name{reactive}
\alias{reactive}
\title{Create a Reactive Function}
\usage{
  reactive(x)
}
\arguments{
  \item{x}{The value or function to make reactive. The
  function must not have any parameters.}
}
\value{
  A reactive function. (Note that reactive functions can
  only be called from within other reactive functions.)
}
\description{
  Wraps a normal function to create a reactive function.
  Conceptually, a reactive function is a function whose
  result will change over time.
}
\details{
  Reactive functions are functions that can read reactive
  values and call other reactive functions. Whenever a
  reactive value changes, any reactive functions that
  depended on it are marked as "invalidated" and will
  automatically re-execute if necessary. If a reactive
  function is marked as invalidated, any other reactive
  functions that recently called it are also marked as
  invalidated. In this way, invalidations ripple through
  the functions that depend on each other.

  See the
  \href{http://rstudio.github.com/shiny/tutorial/}{Shiny
  tutorial} for more information about reactive functions.
}

