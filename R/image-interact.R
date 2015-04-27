#' Find rows of data that are selected by a brush
#'
#' This function returns rows from a data frame which are under a brush used
#' with \code{\link{plotOutput}}.
#'
#' The \code{xvar}, \code{yvar}, \code{panelvar1}, and \code{panelvar2}
#' arguments specify which columns in the data correspond to the x variable, y
#' variable, and panel variables of the plot. For example, if your plot is
#' \code{plot(x=cars$speed, y=cars$dist)}, and your brush is named
#' \code{"cars_brush"}, then you would use \code{selectBrush(cars,
#' input$cars_brush, "speed", "dist")}.
#'
#' For plots created with ggplot2, it should not be necessary to specify the
#' column names; that information will already be contained in the brush,
#' provided that variables are in the original data, and not computed. For
#' example, with \code{ggplot(cars, aes(x=speed, y=dist)) + geom_point()}, you
#' could use \code{selectBrush(cars, input$cars_brush)}. If, however, you use a
#' computed column, like \code{ggplot(cars, aes(x=speed/2, y=dist)) +
#' geom_point()}, then it will not be able to automatically extract column names
#' and filter on them. If you want to use this function to filter data, it is
#' recommended that you not use computed columns; instead, modify the data
#' first, and then make the plot with "raw" columns in the modified data.
#'
#' If a specified x or y column is a factor, then it will be coerced to an
#' integer vector. If it is a character vector, then it will be coerced to a
#' factor and then integer vector. This means that the brush will be considered
#' to cover a given character/factor value when it covers the center value.
#'
#' @param brush The data from a brush, such as \code{input$plot_brush}.
#' @param df A data frame from which to select rows.
#' @param xvar A string with the name of the variable on the x axis. This must
#'   also be the name of a column in \code{df}. If absent, then
#'   \code{selectBrush} will try to infer the variable from the brush (only
#'   works for ggplot2).
#' @param yvar A string with the name of the variable on the y axis. This must
#'   also be the name of a column in \code{df}. If absent, then
#'   \code{selectBrush} will try to infer the variable from the brush (only
#'   works for ggplot2).
#' @param panelvar1,panelvar2 Each of these is a string with the name of a panel
#'   variable. For example, if with ggplot2, you facet on a variable called
#'   \code{cyl}, then you can use \code{"cyl"} here. However, specifying the
#'   panel variable should not be necessary with ggplot2; Shiny should be able
#'   to auto-detect the panel variable.
#'
#' @seealso \code{\link{plotOutput}} for example usage.
#' @export
selectBrush <- function(df, brush, xvar = NULL, yvar = NULL,
                        panelvar1 = NULL, panelvar2 = NULL) {
  if (is.null(brush)) {
    return(df[0, , drop = FALSE])
  }

  # Try to extract vars from brush object
  if (is.null(xvar))      xvar      <- brush$mapping$x
  if (is.null(yvar))      yvar      <- brush$mapping$y
  if (is.null(panelvar1)) panelvar1 <- brush$mapping$panelvar1
  if (is.null(panelvar2)) panelvar2 <- brush$mapping$panelvar2

  if (is.null(xvar))
    stop("selectBrush: not able to automatically infer `xvar` from brush data.")
  if (is.null(yvar))
    stop("selectBrush: not able to automatically infer `yvar` from brush data.")

  # Extract data values from the data frame
  x <- df[[xvar]]
  y <- df[[yvar]]

  # Coerce characters and factors to integers, since the brush has numeric
  # coordinates.
  if (is.character(x)) x <- as.factor(x)
  if (is.factor(x)) x <- as.integer(x)

  if (is.character(y)) x <- as.factor(y)
  if (is.factor(y)) x <- as.integer(y)

  # Find which rows are matches for the panel vars (if present)
  keep_rows <- rep.int(TRUE, nrow(df))
  if (!is.null(panelvar1)) {
    brush_value <- brush$panelvar1
    col_vals <- df[[panelvar1]]

    # brush_value is always a character; may need to coerce to number
    if (is.numeric(col_vals))
      brush_value <- as.numeric(brush_value)

    keep_rows <- keep_rows & (brush_value == col_vals)
  }

  if (!is.null(panelvar2)) {
    brush_value <- brush$panelvar2
    col_vals <- df[[panelvar2]]

    # brush_value is always a character; may need to coerce to number
    if (is.numeric(col_vals))
      brush_value <- as.numeric(brush_value)

    keep_rows <- keep_rows & (brush_value == col_vals)
  }


  # Filter out x and y values
  keep_rows <- keep_rows &
               x >= brush$xmin & x <= brush$xmax &
               y >= brush$ymin & y <= brush$ymax

  df[keep_rows, , drop = FALSE]
}

