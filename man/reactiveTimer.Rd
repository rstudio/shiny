\name{reactiveTimer}
\alias{reactiveTimer}
\title{Timer}
\usage{
  reactiveTimer(intervalMs = 1000)
}
\arguments{
  \item{intervalMs}{Interval to fire, in milliseconds}
}
\value{
  A function that can be called from a reactive context, in
  order to cause that context to be invalidated the next
  time the timer interval elapses.
}
\description{
  Creates a reactive timer with the given interval.
}

