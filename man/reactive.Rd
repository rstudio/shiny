\name{reactive}
\alias{reactive}
\title{Wraps a normal function to create a reactive function.}
\usage{
  reactive(x)
}
\arguments{
  \item{x}{The value or function to make reactive.}
}
\description{
  A reactive function is a function that knows its result
  will change over time.
}
\details{


  Reactive values are values that can change over time.

  Reactive functions are functions that can read reactive
  values and call other reactive functions. Whenever a
  reactive value changes, any reactive functions that
  depended on it will re-execute.
}

