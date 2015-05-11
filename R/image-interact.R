#' Find rows of data that are selected by a brush
#'
#' This function returns rows from a data frame which are under a brush used
#' with \code{\link{plotOutput}}.
#'
#' It is also possible for this function to return all rows from the input data
#' frame, but with an additional column \code{selected_}, which indicates which
#' rows of the input data frame are selected by the brush (\code{TRUE} for
#' selected, \code{FALSE} for not-selected). This is enabled by setting
#' \code{allRows=TRUE} option.
#'
#' The \code{xvar}, \code{yvar}, \code{panelvar1}, and \code{panelvar2}
#' arguments specify which columns in the data correspond to the x variable, y
#' variable, and panel variables of the plot. For example, if your plot is
#' \code{plot(x=cars$speed, y=cars$dist)}, and your brush is named
#' \code{"cars_brush"}, then you would use \code{brushedPoints(cars,
#' input$cars_brush, "speed", "dist")}.
#'
#' For plots created with ggplot2, it should not be necessary to specify the
#' column names; that information will already be contained in the brush,
#' provided that variables are in the original data, and not computed. For
#' example, with \code{ggplot(cars, aes(x=speed, y=dist)) + geom_point()}, you
#' could use \code{brushedPoints(cars, input$cars_brush)}. If, however, you use
#' a computed column, like \code{ggplot(cars, aes(x=speed/2, y=dist)) +
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
#' If the brush is operating in just the x or y directions (e.g., with
#' \code{brushOpts(direction = "x")}, then this function will filter out points
#' using just the x or y variable, whichever is appropriate.
#'
#' @param brush The data from a brush, such as \code{input$plot_brush}.
#' @param df A data frame from which to select rows.
#' @param xvar,yvar A string with the name of the variable on the x or y axis.
#'   This must also be the name of a column in \code{df}. If absent, then this
#'   function will try to infer the variable from the brush (only works for
#'   ggplot2).
#' @param panelvar1,panelvar2 Each of these is a string with the name of a panel
#'   variable. For example, if with ggplot2, you facet on a variable called
#'   \code{cyl}, then you can use \code{"cyl"} here. However, specifying the
#'   panel variable should not be necessary with ggplot2; Shiny should be able
#'   to auto-detect the panel variable.
#' @param allRows If \code{FALSE} (the default) return a data frame containing
#'   the selected rows. If \code{TRUE}, the input data frame will have a new
#'   column, \code{selected_}, which indicates whether the row was inside the
#'   brush (\code{TRUE}) or outside the brush (\code{FALSE}).
#'
#' @seealso \code{\link{plotOutput}} for example usage.
#' @export
brushedPoints <- function(df, brush, xvar = NULL, yvar = NULL,
                          panelvar1 = NULL, panelvar2 = NULL,
                          allRows = FALSE) {
  if (is.null(brush)) {
    if (allRows)
      df$selected_ <- FALSE
    else
      df <- df[0, , drop = FALSE]

    return(df)
  }

  if (is.null(brush$xmin)) {
    stop("brushedPoints requires a brush object with xmin, xmax, ymin, and ymax.")
  }

  # Which direction(s) the brush is selecting over. Direction can be 'x', 'y',
  # or 'xy'.
  use_x <- grepl("x", brush$direction)
  use_y <- grepl("y", brush$direction)

  # Try to extract vars from brush object
  xvar      <- xvar      %OR% brush$mapping$x
  yvar      <- yvar      %OR% brush$mapping$y
  panelvar1 <- panelvar1 %OR% brush$mapping$panelvar1
  panelvar2 <- panelvar2 %OR% brush$mapping$panelvar2

  # Filter out x and y values
  keep_rows <- rep(TRUE, nrow(df))
  if (use_x) {
    if (is.null(xvar))
      stop("brushedPoints: not able to automatically infer `xvar` from brush")
    # Extract data values from the data frame
    x <- asNumber(df[[xvar]])
    keep_rows <- keep_rows & (x >= brush$xmin & x <= brush$xmax)
  }
  if (use_y) {
    if (is.null(yvar))
      stop("brushedPoints: not able to automatically infer `yvar` from brush")
    y <- asNumber(df[[yvar]])
    keep_rows <- keep_rows & (y >= brush$ymin & y <= brush$ymax)
  }

  # Find which rows are matches for the panel vars (if present)
  if (!is.null(panelvar1))
    keep_rows <- keep_rows & panelMatch(brush$panelvar1, df[[panelvar1]])
  if (!is.null(panelvar2))
    keep_rows <- keep_rows & panelMatch(brush$panelvar2, df[[panelvar2]])

  if (allRows) {
    df$selected_ <- keep_rows
    df
  } else {
    df[keep_rows, , drop = FALSE]
  }
}

# The `brush` data structure will look something like the examples below.
# For base graphics, `mapping` is empty, and there are no panelvars:
# List of 8
#  $ xmin   : num 3.73
#  $ xmax   : num 4.22
#  $ ymin   : num 13.9
#  $ ymax   : num 19.8
#  $ mapping: Named list()
#  $ domain :List of 4
#   ..$ left  : num 1.36
#   ..$ right : num 5.58
#   ..$ bottom: num 9.46
#   ..$ top   : num 34.8
#  $ range  :List of 4
#   ..$ left  : num 58
#   ..$ right : num 429
#   ..$ bottom: num 226
#   ..$ top   : num 58
#  $ log    :List of 2
#   ..$ x: NULL
#   ..$ y: NULL
#  $ direction: chr "y"
#
# For ggplot2, the mapping vars usually will be included, and if faceting is
# used, they will be listed as panelvars:
# List of 10
#  $ xmin     : num 3.18
#  $ xmax     : num 3.78
#  $ ymin     : num 17.1
#  $ ymax     : num 20.4
#  $ panelvar1: int 6
#  $ panelvar2: int 0
#  $ mapping  :List of 4
#   ..$ x        : chr "wt"
#   ..$ y        : chr "mpg"
#   ..$ panelvar1: chr "cyl"
#   ..$ panelvar2: chr "am"
#  $ domain   :List of 4
#   ..$ left  : num 1.32
#   ..$ right : num 5.62
#   ..$ bottom: num 9.22
#   ..$ top   : num 35.1
#  $ range    :List of 4
#   ..$ left  : num 172
#   ..$ right : num 300
#   ..$ bottom: num 144
#   ..$ top   : num 28.5
#  $ log      :List of 2
#   ..$ x: NULL
#   ..$ y: NULL
#  $ direction: chr "y"


#'Find rows of data that are near a click/hover/double-click
#'
#'This function returns rows from a data frame which are near a click, hover, or
#'double-click, when used with \code{\link{plotOutput}}. The rows will be sorted
#'by their distance to the mouse event.
#'
#'It is also possible for this function to return all rows from the input data
#'frame, but with an additional column \code{selected_}, which indicates which
#'rows of the input data frame are selected by the brush (\code{TRUE} for
#'selected, \code{FALSE} for not-selected). This is enabled by setting
#'\code{allRows=TRUE} option. If this is used, the resulting data frame will not
#'be sorted by distance to the mouse event.
#'
#'The \code{xvar}, \code{yvar}, \code{panelvar1}, and \code{panelvar2} arguments
#'specify which columns in the data correspond to the x variable, y variable,
#'and panel variables of the plot. For example, if your plot is
#'\code{plot(x=cars$speed, y=cars$dist)}, and your click variable is named
#'\code{"cars_click"}, then you would use \code{nearPoints(cars,
#'input$cars_brush, "speed", "dist")}.
#'
#'@inheritParams brushedPoints
#'@param coordinfo The data from a mouse event, such as \code{input$plot_click}.
#'@param threshold A maxmimum distance to the click point; rows in the data
#'  frame where the distance to the click is less than \code{threshold} will be
#'  returned.
#'@param maxpoints Maximum number of rows to return. If NULL (the default),
#'  return all rows that are within the threshold distance.
#'@param addDist If TRUE, add a column named \code{dist_} that contains the
#'  distance from the coordinate to the point, in pixels. When no mouse event
#'  has yet occured, the value of \code{dist_} will be \code{NA}.
#'@param allRows If \code{FALSE} (the default) return a data frame containing
#'  the selected rows. If \code{TRUE}, the input data frame will have a new
#'  column, \code{selected_}, which indicates whether the row was inside the
#'  selected by the mouse event (\code{TRUE}) or not (\code{FALSE}).
#'
#'@seealso \code{\link{plotOutput}} for more examples.
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
#' nearPoints(mtcars, input$plot_click, threshold = 10, maxpoints = 1)
#'
#' }
#'@export
nearPoints <- function(df, coordinfo, xvar = NULL, yvar = NULL,
                       panelvar1 = NULL, panelvar2 = NULL,
                       threshold = 5, maxpoints = NULL, addDist = FALSE,
                       allRows = FALSE) {
  if (is.null(coordinfo)) {
    if (addDist)
      df$dist_ <- NA_real_

    if (allRows)
      df$selected_ <- FALSE
    else
      df <- df[0, , drop = FALSE]

    return(df)
  }

  if (is.null(coordinfo$x)) {
    stop("nearPoints requires a click/hover/double-click object with x and y values.")
  }

  # Try to extract vars from coordinfo object
  xvar      <- xvar      %OR% coordinfo$mapping$x
  yvar      <- yvar      %OR% coordinfo$mapping$y
  panelvar1 <- panelvar1 %OR% coordinfo$mapping$panelvar1
  panelvar2 <- panelvar2 %OR% coordinfo$mapping$panelvar2

  if (is.null(xvar))
    stop("nearPoints: not able to automatically infer `xvar` from coordinfo")
  if (is.null(yvar))
    stop("nearPoints: not able to automatically infer `yvar` from coordinfo")

  # Extract data values from the data frame
  x <- asNumber(df[[xvar]])
  y <- asNumber(df[[yvar]])

  # Get the pixel coordinates of the point
  coordPx <- scaleCoords(coordinfo$x, coordinfo$y, coordinfo)

  # Get pixel coordinates of data points
  dataPx <- scaleCoords(x, y, coordinfo)

  # Distances of data points to coordPx
  dists <- sqrt((dataPx$x - coordPx$x) ^ 2 + (dataPx$y - coordPx$y) ^ 2)

  if (addDist)
    df$dist_ <- dists

  keep_rows <- (dists <= threshold)

  # Find which rows are matches for the panel vars (if present)
  if (!is.null(panelvar1))
    keep_rows <- keep_rows & panelMatch(coordinfo$panelvar1, df[[panelvar1]])
  if (!is.null(panelvar2))
    keep_rows <- keep_rows & panelMatch(coordinfo$panelvar2, df[[panelvar2]])

  # Track the indices to keep
  keep_idx <- which(keep_rows)

  # Order by distance
  dists <- dists[keep_idx]
  keep_idx <- keep_idx[order(dists)]

  # Keep max number of rows
  if (!is.null(maxpoints) && length(keep_idx) > maxpoints) {
    keep_idx <- keep_idx[seq_len(maxpoints)]
  }

  if (allRows) {
    # Add selected_ column if needed
    df$selected_ <- FALSE
    df$selected_[keep_idx] <- TRUE

  } else {
    # If we don't keep all rows, return just the selected rows, sorted by
    # distance.
    df <- df[keep_idx, , drop = FALSE]
  }

  df
}

# The coordinfo data structure will look something like the examples below.
# For base graphics, `mapping` is empty, and there are no panelvars:
# List of 7
#  $ x      : num 4.37
#  $ y      : num 12
#  $ mapping: Named list()
#  $ domain :List of 4
#   ..$ left  : num 1.36
#   ..$ right : num 5.58
#   ..$ bottom: num 9.46
#   ..$ top   : num 34.8
#  $ range  :List of 4
#   ..$ left  : num 58
#   ..$ right : num 429
#   ..$ bottom: num 226
#   ..$ top   : num 58
#  $ log    :List of 2
#   ..$ x: NULL
#   ..$ y: NULL
#  $ .nonce : num 0.343
#
# For ggplot2, the mapping vars usually will be included, and if faceting is
# used, they will be listed as panelvars:
# List of 9
#  $ x        : num 3.78
#  $ y        : num 17.1
#  $ panelvar1: int 6
#  $ panelvar2: int 0
#  $ mapping  :List of 4
#   ..$ x        : chr "wt"
#   ..$ y        : chr "mpg"
#   ..$ panelvar1: chr "cyl"
#   ..$ panelvar2: chr "am"
#  $ domain   :List of 4
#   ..$ left  : num 1.32
#   ..$ right : num 5.62
#   ..$ bottom: num 9.22
#   ..$ top   : num 35.1
#  $ range    :List of 4
#   ..$ left  : num 172
#   ..$ right : num 300
#   ..$ bottom: num 144
#   ..$ top   : num 28.5
#  $ log      :List of 2
#   ..$ x: NULL
#   ..$ y: NULL
#  $ .nonce   : num 0.603



# Coerce various types of variables to numbers. This works for Date, POSIXt,
# characters, and factors. Used because the mouse coords are numeric.
asNumber <- function(x) {
  if (is.character(x)) x <- as.factor(x)
  if (is.factor(x)) x <- as.integer(x)
  as.numeric(x)
}

# Given a panelvar value and a vector x, return logical vector indicating which
# items match the panelvar value. Because the panelvar value is always a
# string but the vector could be numeric, it might be necessary to coerce the
# panelvar to a number before comparing to the vector.
panelMatch <- function(search_value, x) {
  if (is.numeric(x)) search_value <- as.numeric(search_value)
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
