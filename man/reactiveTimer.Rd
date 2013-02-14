\name{reactiveTimer}
\alias{reactiveTimer}
\title{Timer}
\usage{
  reactiveTimer(intervalMs = 1000)
}
\arguments{
  \item{intervalMs}{How often to fire, in milliseconds}
}
\value{
  A no-parameter function that can be called from a
  reactive context, in order to cause that context to be
  invalidated the next time the timer interval elapses.
  Calling the returned function also happens to yield the
  current time (as in \code{\link{Sys.time}}).
}
\description{
  Creates a reactive timer with the given interval. A
  reactive timer is like a reactive value, except reactive
  values are triggered when they are set, while reactive
  timers are triggered simply by the passage of time.
}
\details{
  \link[=reactive]{Reactive expressions} and observers that
  want to be invalidated by the timer need to call the
  timer function that \code{reactiveTimer} returns, even if
  the current time value is not actually needed.

  See \code{\link{invalidateLater}} as a safer and simpler
  alternative.
}
\seealso{
  invalidateLater
}

