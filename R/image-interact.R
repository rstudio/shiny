#' Find points that are under a brush
#'
#' This function returns rows from a data frame which are under a brush used
#' with \code{\link{plotOutput}}.
#'
#' If a specified x or y column is a factor, then it will be coerced to an
#' integer vector. If it is a character vector, then it will be coerced to a
#' factor and then integer vector. This means that the brush will be considered
#' to cover a given character/factor value when it covers the center value.
#'
#' @param brush The data from a brush, such as \code{input$plot_brush}.
#' @param df A data frame from which to select rows.
#' @param xvar A string with the name of the variable on the x axis. This must
#'   also be the name of a column in \code{df}.
#' @param yvar A string with the name of the variable on the y axis. This must
#'   also be the name of a column in \code{df}.
#'
#' @seealso \code{\link{plotOutput}} for example usage.
#' @export
underBrush <- function(brush, df, xvar, yvar) {
  if (is.null(brush)) {
    return(df[0, , drop = FALSE])
  }

  x <- df[[xvar]]
  y <- df[[yvar]]

  if (is.character(x)) x <- as.factor(x)
  if (is.factor(x)) x <- as.integer(x)

  if (is.character(y)) x <- as.factor(y)
  if (is.factor(y)) x <- as.integer(y)

  # Panel vars, if present
  panel_names <- setdiff(names(brush), c("xmin", "xmax", "ymin", "ymax"))

  # Find which rows are matches for the pnael vars
  keep_rows <- rep.int(TRUE, nrow(df))
  lapply(panel_names, function(varname) {
    brush_value <- brush[varname]
    col_vals <- df[[varname]]

    # brush_value is always a character; may need to coerce to number
    if (is.numeric(col_vals))
      brush_value <- as.numeric(brush_value)

    keep_rows <<- keep_rows & (brush_value == col_vals)
  })

  # Filter out x and y values
  keep_rows <- keep_rows &
               x >= brush$xmin & x <= brush$xmax &
               y >= brush$ymin & y <= brush$ymax

  df[keep_rows, , drop = FALSE]
}

