#' Create an object representing hover options
#'
#' This generates an object representing hovering options, to be passed as the
#' \code{hoverOpts} argument of \code{\link{imageOutput}} or
#' \code{\link{plotOutput}}.
#'
#' @param delay How long to delay (in milliseconds) when debouncing or
#'   throttling, before sending the mouse location to the server.
#' @param delayType The type of algorithm for limiting the number of hover
#'   events. Use \code{"throttle"} to limit the number of hover events to one
#'   every \code{delay} milliseconds. Use \code{"debounce"} to suspend events
#'   while the cursor is moving, and wait until the cursor has been at rest for
#'   \code{delay} milliseconds before sending an event.
#' @param clip Should the hover area be clipped to the plotting area? If FALSE,
#'   then the server will receive hover events even when the mouse is outside
#'   the plotting area, as long as it is still inside the image.
#' @export
hoverOptions <- function(delay = 300, delayType = c("debounce", "throttle"),
                         clip = TRUE) {
  list(
    delay = delay,
    delayType = match.arg(delayType),
    clip = clip
  )
}

#' Create an object representing brushing options
#'
#' This generates an object representing brushing options, to be passed as the
#' \code{brushOpts} argument of \code{\link{imageOutput}} or
#' \code{\link{plotOutput}}.
#'
#' @param color Fill color of the brush.
#' @param outline Outline color of the brush.
#' @param opacity Opacity of the brush
#' @param delay How long to delay (in milliseconds) when debouncing or
#'   throttling, before sending the brush data to the server.
#' @param delayType The type of algorithm for limiting the number of brush
#'   events. Use \code{"throttle"} to limit the number of brush events to one
#'   every \code{delay} milliseconds. Use \code{"debounce"} to suspend events
#'   while the cursor is moving, and wait until the cursor has been at rest for
#'   \code{delay} milliseconds before sending an event.
#' @param clip Should the brush area be clipped to the plotting area? If FALSE,
#'   then the user will be able to brush outside the plotting area, as long as
#'   it is still inside the image.
#' @export
brushOptions <- function(color = "#666", outline = "#000", opacity = 0.3,
                         delay = 300, delayType = c("debounce", "throttle"),
                         clip = TRUE) {
  list(
    color = color,
    outline = outline,
    opacity = opacity,
    delay = delay,
    delayType = match.arg(delayType),
    clip = clip
  )
}
