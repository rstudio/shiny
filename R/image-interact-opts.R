#' Create an object representing click options
#'
#' This generates an object representing click options, to be passed as the
#' `click` argument of [imageOutput()] or
#' [plotOutput()].
#'
#' @param id Input value name. For example, if the value is `"plot_click"`,
#'   then the click coordinates will be available as `input$plot_click`.
#' @param clip Should the click area be clipped to the plotting area? If FALSE,
#'   then the server will receive click events even when the mouse is outside
#'   the plotting area, as long as it is still inside the image.
#' @export
clickOpts <- function(id, clip = TRUE) {
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
#' the `dblclick` argument of [imageOutput()] or
#' [plotOutput()].
#'
#' @param id Input value name. For example, if the value is
#'   `"plot_dblclick"`, then the click coordinates will be available as
#'   `input$plot_dblclick`.
#' @param clip Should the click area be clipped to the plotting area? If FALSE,
#'   then the server will receive double-click events even when the mouse is
#'   outside the plotting area, as long as it is still inside the image.
#' @param delay Maximum delay (in ms) between a pair clicks for them to be
#'   counted as a double-click.
#' @export
dblclickOpts <- function(id, clip = TRUE, delay = 400) {
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
#' `hover` argument of [imageOutput()] or
#' [plotOutput()].
#'
#' @param id Input value name. For example, if the value is `"plot_hover"`,
#'   then the hover coordinates will be available as `input$plot_hover`.
#' @param delay How long to delay (in milliseconds) when debouncing or
#'   throttling, before sending the mouse location to the server.
#' @param delayType The type of algorithm for limiting the number of hover
#'   events. Use `"throttle"` to limit the number of hover events to one
#'   every `delay` milliseconds. Use `"debounce"` to suspend events
#'   while the cursor is moving, and wait until the cursor has been at rest for
#'   `delay` milliseconds before sending an event.
#' @param clip Should the hover area be clipped to the plotting area? If FALSE,
#'   then the server will receive hover events even when the mouse is outside
#'   the plotting area, as long as it is still inside the image.
#' @param nullOutside If `TRUE` (the default), the value will be set to
#'   `NULL` when the mouse exits the plotting area. If `FALSE`, the
#'   value will stop changing when the cursor exits the plotting area.
#' @export
hoverOpts <- function(id, delay = 300,
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
#' `brush` argument of [imageOutput()] or
#' [plotOutput()].
#'
#' @param id Input value name. For example, if the value is `"plot_brush"`,
#'   then the coordinates will be available as `input$plot_brush`. Multiple
#'   `imageOutput`/`plotOutput` calls may share the same `id`
#'   value; brushing one image or plot will cause any other brushes with the
#'   same `id` to disappear.
#' @param fill Fill color of the brush.
#' @param stroke Outline color of the brush.
#' @param opacity Opacity of the brush
#' @param delay How long to delay (in milliseconds) when debouncing or
#'   throttling, before sending the brush data to the server.
#' @param delayType The type of algorithm for limiting the number of brush
#'   events. Use `"throttle"` to limit the number of brush events to one
#'   every `delay` milliseconds. Use `"debounce"` to suspend events
#'   while the cursor is moving, and wait until the cursor has been at rest for
#'   `delay` milliseconds before sending an event.
#' @param clip Should the brush area be clipped to the plotting area? If FALSE,
#'   then the user will be able to brush outside the plotting area, as long as
#'   it is still inside the image.
#' @param direction The direction for brushing. If `"xy"`, the brush can be
#'   drawn and moved in both x and y directions. If `"x"`, or `"y"`,
#'   the brush wil work horizontally or vertically.
#' @param resetOnNew When a new image is sent to the browser (via
#'   [renderImage()]), should the brush be reset? The default,
#'   `FALSE`, is useful if you want to update the plot while keeping the
#'   brush. Using `TRUE` is useful if you want to clear the brush whenever
#'   the plot is updated.
#' @export
brushOpts <- function(id, fill = "#9cf", stroke = "#036",
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
