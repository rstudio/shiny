hasDecimals <- function(value) {
  truncatedValue <- round(value)
  return (!identical(value, truncatedValue))
}

#' @rdname sliderInput
#'
#' @param interval The interval, in milliseconds, between each animation step.
#' @param loop \code{TRUE} to automatically restart the animation when it
#'   reaches the end.
#' @param playButton Specifies the appearance of the play button. Valid values
#'   are a one-element character vector (for a simple text label), an HTML tag
#'   or list of tags (using \code{\link{tag}} and friends), or raw HTML (using
#'   \code{\link{HTML}}).
#' @param pauseButton Similar to \code{playButton}, but for the pause button.
#'
#' @export
animationOptions <- function(interval=1000,
                             loop=FALSE,
                             playButton=NULL,
                             pauseButton=NULL) {
  list(interval=interval,
       loop=loop,
       playButton=playButton,
       pauseButton=pauseButton)
}
