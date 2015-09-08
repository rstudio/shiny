#' Create an object representing click options
#'
#' This generates an object representing click options, to be passed as the
#' \code{click} argument of \code{\link{imageOutput}} or
#' \code{\link{plotOutput}}.
#'
#' @param id Input value name. For example, if the value is \code{"plot_click"},
#'   then the click coordinates will be available as \code{input$plot_click}.
#' @param clip Should the click area be clipped to the plotting area? If FALSE,
#'   then the server will receive click events even when the mouse is outside
#'   the plotting area, as long as it is still inside the image.
#' @export
clickOpts <- function(id = NULL, clip = TRUE) {
  if (is.null(id))
    stop("id must not be NULL")

  list(
    id = id,
    clip = clip
  )
}


#' Create an object representing double-click options
#'
#' This generates an object representing dobule-click options, to be passed as
#' the \code{dblclick} argument of \code{\link{imageOutput}} or
#' \code{\link{plotOutput}}.
#'
#' @param id Input value name. For example, if the value is
#'   \code{"plot_dblclick"}, then the click coordinates will be available as
#'   \code{input$plot_dblclick}.
#' @param clip Should the click area be clipped to the plotting area? If FALSE,
#'   then the server will receive double-click events even when the mouse is
#'   outside the plotting area, as long as it is still inside the image.
#' @param delay Maximum delay (in ms) between a pair clicks for them to be
#'   counted as a double-click.
#' @export
dblclickOpts <- function(id = NULL, clip = TRUE, delay = 400) {
  if (is.null(id))
    stop("id must not be NULL")

  list(
    id = id,
    clip = clip,
    delay = delay
  )
}

#' Create an object representing hover options
#'
#' This generates an object representing hovering options, to be passed as the
#' \code{hover} argument of \code{\link{imageOutput}} or
#' \code{\link{plotOutput}}.
#'
#' @param id Input value name. For example, if the value is \code{"plot_hover"},
#'   then the hover coordinates will be available as \code{input$plot_hover}.
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
#' @param nullOutside If \code{TRUE} (the default), the value will be set to
#'   \code{NULL} when the mouse exits the plotting area. If \code{FALSE}, the
#'   value will stop changing when the cursor exits the plotting area.
#' @export
hoverOpts <- function(id = NULL, delay = 300,
                      delayType = c("debounce", "throttle"), clip = TRUE,
                      nullOutside = TRUE) {
  if (is.null(id))
    stop("id must not be NULL")

  list(
    id = id,
    delay = delay,
    delayType = match.arg(delayType),
    clip = clip,
    nullOutside = nullOutside
  )
}

#' Create an object representing brushing options
#'
#' This generates an object representing brushing options, to be passed as the
#' \code{brush} argument of \code{\link{imageOutput}} or
#' \code{\link{plotOutput}}.
#'
#' @param id Input value name. For example, if the value is \code{"plot_brush"},
#'   then the coordinates will be available as \code{input$plot_brush}. Multiple
#'   \code{imageOutput}/\code{plotOutput} calls may share the same \code{id}
#'   value; brushing one image or plot will cause any other brushes with the
#'   same \code{id} to disappear.
#' @param fill Fill color of the brush.
#' @param stroke Outline color of the brush.
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
#' @param direction The direction for brushing. If \code{"xy"}, the brush can be
#'   drawn and moved in both x and y directions. If \code{"x"}, or \code{"y"},
#'   the brush wil work horizontally or vertically.
#' @param resetOnNew When a new image is sent to the browser (via
#'   \code{\link{renderImage}}), should the brush be reset? The default,
#'   \code{FALSE}, is useful if you want to update the plot while keeping the
#'   brush. Using \code{TRUE} is useful if you want to clear the brush whenever
#'   the plot is updated.
#' @export
brushOpts <- function(id = NULL, fill = "#9cf", stroke = "#036",
                      opacity = 0.25, delay = 300,
                      delayType = c("debounce", "throttle"), clip = TRUE,
                      direction = c("xy", "x", "y"),
                      resetOnNew = FALSE) {
  if (is.null(id))
    stop("id must not be NULL")

  list(
    id = id,
    fill = fill,
    stroke = stroke,
    opacity = opacity,
    delay = delay,
    delayType = match.arg(delayType),
    clip = clip,
    direction = match.arg(direction),
    resetOnNew = resetOnNew
  )
}
