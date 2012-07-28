\name{animationOptions}
\alias{animationOptions}
\title{Animation Options}
\usage{
  animationOptions(interval = 1000, loop = FALSE,
    playButton = NULL, pauseButton = NULL)
}
\arguments{
  \item{interval}{The interval, in milliseconds, between
  each animation step.}

  \item{loop}{\code{TRUE} to automatically restart the
  animation when it reaches the end.}

  \item{playButton}{Specifies the appearance of the play
  button. Valid values are a one-element character vector
  (for a simple text label), an HTML tag or list of tags
  (using \code{\link{tag}} and friends), or raw HTML (using
  \code{\link{HTML}}).}

  \item{pauseButton}{Similar to \code{playButton}, but for
  the pause button.}
}
\description{
  Creates an options object for customizing animations for
  \link{sliderInput}.
}

