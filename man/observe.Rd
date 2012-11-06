\name{observe}
\alias{observe}
\title{Create a reactive observer}
\usage{
  observe(func)
}
\arguments{
  \item{func}{The function to observe. It must not have any
  parameters. Any return value from this function will be
  ignored.}
}
\description{
  Creates an observer from the given function. An observer
  is like a reactive function in that it can read reactive
  values and call reactive functions, and will
  automatically re-execute when those dependencies change.
  But unlike reactive functions, it doesn't yield a result
  and can't be used as an input to other reactive
  functions. Thus, observers are only useful for their side
  effects (for example, performing I/O).
}
\details{
  Another contrast between reactive functions and observers
  is their execution strategy. Reactive functions use lazy
  evaluation; that is, when their dependencies change, they
  don't re-execute right away but rather wait until they
  are called by someone else. Indeed, if they are not
  called then they will never re-execute. In contrast,
  observers use eager evaluation; as soon as their
  dependencies change, they schedule themselves to
  re-execute.
}

