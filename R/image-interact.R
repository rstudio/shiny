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

#' Find rows of data that are near a click/hover/double-click
#'
#' This function returns rows from a data frame which are near a click, hover,
#' or double-click, when used with \code{\link{plotOutput}}. The rows will be
#' sorted by their distance to the mouse event.
#'
#' The \code{xvar}, \code{yvar}, \code{panelvar1}, and \code{panelvar2}
#' arguments specify which columns in the data correspond to the x variable, y
#' variable, and panel variables of the plot. For example, if your plot is
#' \code{plot(x=cars$speed, y=cars$dist)}, and your click variable is named
#' \code{"cars_click"}, then you would use \code{nearPoints(cars,
#' input$cars_brush, "speed", "dist")}.
#'
#' @inheritParams selectBrush
#' @param threshold A maxmimum distance to the click point; rows in the data
#'   frame where the distance to the click is less than \code{threshold} will be
#'   returned.
#' @param maxrows Maximum number of rows to return. If NULL (the default),
#'   return all rows that are within the threshold distance.
#' @param addDist If TRUE, add a column named \code{_dist} that contains the
#'   distance from the coordinate to the point, in pixels.
#'
#' @seealso \code{\link{plotOutput}} for more examples.
#'
#' @examples
#' \dontrun{
#' # Note that in practice, these examples would need to go in reactives
#' # or observers.
#'
#' # This would select all points within 5 pixels of the click
#' nearPoints(mtcars, input$plot_click)
#'
#' # Select just the nearest point within 10 pixels of the click
#' nearPoints(mtcars, input$plot_click, threshold = 10, maxrows = 1)
#'
#' }
#' @export
nearPoints <- function(df, coordinfo, xvar = NULL, yvar = NULL,
                       panelvar1 = NULL, panelvar2 = NULL,
                       threshold = 5, maxrows = NULL, addDist = FALSE) {
  if (is.null(coordinfo)) {
    return(df[0, , drop = FALSE])
  }

  vars <- findCoordmapVars(coordinfo, xvar, yvar, panelvar1, panelvar2)

  # Extract data values from the data frame
  x <- asNumber(df[[vars$xvar]])
  y <- asNumber(df[[vars$yvar]])

  # Get the pixel coordinates of the point
  coordPx <- scaleCoords(coordinfo$x, coordinfo$y, coordinfo)

  # Get pixel coordinates of data points
  dataPx <- scaleCoords(x, y, coordinfo)

  # Distances of data points to coordPx
  dists <- sqrt((dataPx$x - coordPx$x) ^ 2 + (dataPx$y - coordPx$y) ^ 2)

  keep_rows <- (dists <= threshold)

  # Find which rows are matches for the panel vars (if present)
  if (!is.null(vars$panelvar1))
    keep_rows <- keep_rows & panelMatch(coordinfo$panelvar1, df[[vars$panelvar1]])
  if (!is.null(vars$panelvar2))
    keep_rows <- keep_rows & panelMatch(coordinfo$panelvar2, df[[vars$panelvar2]])

  if (addDist)
    df$`_dist` <- dists

  df <- df[keep_rows, , drop = FALSE]

  # Sort by distance
  dists <- dists[keep_rows]
  df <- df[order(dists), , drop = FALSE]

  # Keep max number of rows
  if (!is.null(maxrows) && nrow(df) > maxrows) {
    df <- df[seq_len(maxrows), , drop = FALSE]
  }

  df
}


findCoordmapVars <- function(coordmap, xvar = NULL, yvar = NULL,
                             panelvar1 = NULL, panelvar2 = NULL) {

  # Try to extract vars from coordmap. object
  if (is.null(xvar))      xvar      <- coordmap$mapping$x
  if (is.null(yvar))      yvar      <- coordmap$mapping$y
  if (is.null(panelvar1)) panelvar1 <- coordmap$mapping$panelvar1
  if (is.null(panelvar2)) panelvar2 <- coordmap$mapping$panelvar2

  if (is.null(xvar))
    stop("findCoordmapVars: not able to automatically infer `xvar` from coordmap.")
  if (is.null(yvar))
    stop("findCoordmapVars: not able to automatically infer `yvar` from coordmap.")

  list(
    xvar = xvar,
    yvar = yvar,
    panelvar1 = panelvar1,
    panelvar2 = panelvar2
  )
}

# Coerce characters and factors to integers. Used because the mouse coords
# are numeric.
asNumber <- function(x) {
  if (is.character(x)) x <- as.factor(x)
  if (is.factor(x)) x <- as.integer(x)
  x
}

# Given
panelMatch <- function(search_value, x) {
  # search_value is always a character; may need to coerce to number to match
  # x, because the faceting var might be numeric.
  if (is.numeric(x)) search_value <- as.numeric(match_value)

  x == search_value
}

#  ----------------------------------------------------------------------------
# Scaling functions
# These functions have direct analogs in Javascript code, except these are
# vectorized for x and y.

# Map a value x from a domain to a range. If clip is true, clip it to the
# range.
mapLinear <- function(x, domainMin, domainMax, rangeMin, rangeMax, clip = TRUE) {
  factor <- (rangeMax - rangeMin) / (domainMax - domainMin)
  val <- x - domainMin
  newval <- (val * factor) + rangeMin

  if (clip) {
    maxval <- max(rangeMax, rangeMin)
    minval <- min(rangeMax, rangeMin)
    newval[newval > maxval] <- maxval
    newval[newval < minval] <- minval
  }
  newval
}

# Scale val from domain to range. If logbase is present, use log scaling.
scale1D <- function(val, domainMin, domainMax, rangeMin, rangeMax,
                    logbase = NULL, clip = TRUE) {
  if (!is.null(logbase))
    val <- log(val, logbase)
  mapLinear(val, domainMin, domainMax, rangeMin, rangeMax, clip)
}

# Inverse scale val, from range to domain. If logbase is present, use inverse
# log (power) transformation.
scaleInv1D <- function(val, domainMin, domainMax, rangeMin, rangeMax,
                       logbase = NULL, clip = TRUE) {
    res <- mapLinear(val, rangeMin, rangeMax, domainMin, domainMax, clip)
    if (!is.null(logbase))
      res <- logbase ^ res
    res
}

# Scale x and y coordinates from domain to range, using information in
# scaleinfo. scaleinfo must contain items $domain, $range, and $log. The
# scaleinfo object corresponds to one element from the coordmap object generated
# by getPrevPlotCoordmap or getGgplotCoordmap; it is the scaling information for
# one panel in a plot.
scaleCoords <- function(x, y, scaleinfo) {
  if (is.null(scaleinfo))
    return(NULL)

  domain <- scaleinfo$domain
  range <- scaleinfo$range
  log <- scaleinfo$log

  list(
    x = scale1D(x, domain$left, domain$right, range$left, range$right, log$x),
    y = scale1D(y, domain$bottom, domain$top, range$bottom, range$top, log$y)
  )
}

# Inverse scale x and y coordinates from range to domain, using information in
# scaleinfo.
scaleInvCoords <- function(x, y, scaleinfo) {
  if (is.null(scaleinfo))
    return(NULL)

  domain <- scaleinfo$domain
  range <- scaleinfo$range
  log <- scaleinfo$log

  list(
    x = scaleInv1D(x, domain$left, domain$right, range$left, range$right, log$x),
    y = scaleInv1D(y, domain$bottom, domain$top, range$bottom, range$top, log$y)
  )
}
