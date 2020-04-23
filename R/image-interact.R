#' Find rows of data selected on an interactive plot.
#'
#' @description
#' `brushedPoints()` returns rows from a data frame which are under a brush.
#' `nearPoints()` returns rows from a data frame which are near a click, hover,
#' or double-click. Alternatively, set `allRows = TRUE` to return all rows from
#' the input data with an additional column `selected_` that indicates which
#' rows of the would be selected.
#'
#' @section ggplot2:
#' For plots created with ggplot2, it is not necessary to specify the
#' column names to `xvar`, `yvar`, `panelvar1`, and `panelvar2` as that
#' information can be automatically derived from the plot specification.
#'
#' Note, however, that this will not work if you use a computed column, like
#' `aes(speed/2, dist))`. Instead, we recommend that you modify the data
#' first, and then make the plot with "raw" columns in the modified data.
#'
#' @section Brushing:
#' If x or y column is a factor, then it will be coerced to an integer vector.
#' If it is a character vector, then it will be coerced to a factor and then
#' integer vector. This means that the brush will be considered to cover a
#' given character/factor value when it covers the center value.
#'
#' If the brush is operating in just the x or y directions (e.g., with
#' `brushOpts(direction = "x")`, then this function will filter out points
#' using just the x or y variable, whichever is appropriate.
#'
#' @returns
#' A data frame based on `df`, containing the observations selected by the
#' brush or near the click event. For `nearPoints()`, the rows will be sorted
#' by distance to the event.
#'
#' If `allRows = TRUE`, then all rows will returned, along with a new
#' `selected_` column that indicates whether or not the point was selected.
#' The output from `nearPoints()` will no longer be sorted, but you can
#' set `addDist = TRUE` to get an additional column that gives the pixel
#' distance to the pointer.
#'
#' @param df A data frame from which to select rows.
#' @param brush,coordinfo The data from a brush or click/dblclick/hover event
#'   e.g. `input$plot_brush`, `input$plot_click`.
#' @param xvar,yvar A string giving the name of the variable on the x or y axis.
#'   These are only required for base graphics, and must be the name of
#'   a column in `df`.
#' @param panelvar1,panelvar2 A string giving the name of a panel variable.
#'   For expert use only; in most cases these will be automatically
#'   derived from the ggplot2 spec.
#' @param allRows If `FALSE` (the default) return a data frame containing
#'   the selected rows. If `TRUE`, the input data frame will have a new
#'   column, `selected_`, which indicates whether the row was selected or not.
#' @param threshold A maximum distance (in pixels) to the pointer location.
#'   Rows in the data frame will be selected if the distance to the pointer is
#'   less than `threshold`.
#' @param maxpoints Maximum number of rows to return. If `NULL` (the default),
#'   will return all rows within the threshold distance.
#' @param addDist If TRUE, add a column named `dist_` that contains the
#'   distance from the coordinate to the point, in pixels. When no pointer
#'   event has yet occurred, the value of `dist_` will be `NA`.
#' @seealso [plotOutput()] for example usage.
#' @export
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
    if (!(xvar %in% names(df)))
      stop("brushedPoints: `xvar` ('", xvar ,"')  not in names of input")
    keep_rows <- keep_rows & within_brush(df[[xvar]], brush, "x")
  }
  if (use_y) {
    if (is.null(yvar))
      stop("brushedPoints: not able to automatically infer `yvar` from brush")
    if (!(yvar %in% names(df)))
      stop("brushedPoints: `yvar` ('", yvar ,"') not in names of input")
    keep_rows <- keep_rows & within_brush(df[[yvar]], brush, "y")
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
#  $ coords_css:List of 4
#   ..$ xmin: int 260
#   ..$ xmax: int 298
#   ..$ ymin: num 112
#   ..$ ymax: num 205
#  $ coords_img:List of 4
#   ..$ xmin: int 325
#   ..$ xmax: num 372
#   ..$ ymin: num 140
#   ..$ ymax: num 257
#  $ img_css_ratio:List of 2
#   ..$ x: num 1.25
#   ..$ y: num 1.25
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
#  $ coords_css:List of 4
#   ..$ xmin: int 260
#   ..$ xmax: int 298
#   ..$ ymin: num 112
#   ..$ ymax: num 205
#  $ coords_img:List of 4
#   ..$ xmin: int 325
#   ..$ xmax: num 372
#   ..$ ymin: num 140
#   ..$ ymax: num 257
#  $ img_css_ratio:List of 2
#   ..$ x: num 1.25
#   ..$ y: num 1.25
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


#' @export
#' @rdname brushedPoints
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

  if (!(xvar %in% names(df)))
    stop("nearPoints: `xvar` ('", xvar ,"')  not in names of input")
  if (!(yvar %in% names(df)))
    stop("nearPoints: `yvar` ('", yvar ,"')  not in names of input")

  # Extract data values from the data frame
  x <- asNumber(df[[xvar]], coordinfo$domain$discrete_limits$x)
  y <- asNumber(df[[yvar]], coordinfo$domain$discrete_limits$y)

  # Get the coordinates of the point (in img pixel coordinates)
  point_img <- coordinfo$coords_img

  # Get coordinates of data points (in img pixel coordinates)
  data_img <- scaleCoords(x, y, coordinfo)

  # Get x/y distances (in css coordinates)
  dist_css <- list(
    x = (data_img$x - point_img$x) / coordinfo$img_css_ratio$x,
    y = (data_img$y - point_img$y) / coordinfo$img_css_ratio$y
  )

  # Distances of data points to the target point, in css pixels.
  dists <- sqrt(dist_css$x^2 + dist_css$y^2)

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
#  $ x         : num 4.37
#  $ y         : num 12
#  $ coords_css:List of 2
#   ..$ x: int 286
#   ..$ y: int 192
#  $ coords_img:List of 2
#   ..$ x: num 358
#   ..$ y: int 240
#  $ img_css_ratio:List of 2
#   ..$ x: num 1.25
#   ..$ y: num 1.25
#  $ mapping   : Named list()
#  $ domain    :List of 4
#   ..$ left  : num 1.36
#   ..$ right : num 5.58
#   ..$ bottom: num 9.46
#   ..$ top   : num 34.8
#  $ range     :List of 4
#   ..$ left  : num 58
#   ..$ right : num 429
#   ..$ bottom: num 226
#   ..$ top   : num 58
#  $ log       :List of 2
#   ..$ x: NULL
#   ..$ y: NULL
#  $ .nonce    : num 0.343
#
# For ggplot2, the mapping vars usually will be included, and if faceting is
# used, they will be listed as panelvars:
# List of 9
#  $ x         : num 3.78
#  $ y         : num 17.1
#  $ coords_css:List of 2
#   ..$ x: int 286
#   ..$ y: int 192
#  $ coords_img:List of 2
#   ..$ x: num 358
#   ..$ y: int 240
#  $ img_css_ratio:List of 2
#   ..$ x: num 1.25
#   ..$ y: num 1.25
#  $ panelvar1 : int 6
#  $ panelvar2 : int 0
#  $ mapping   :List of 4
#   ..$ x        : chr "wt"
#   ..$ y        : chr "mpg"
#   ..$ panelvar1: chr "cyl"
#   ..$ panelvar2: chr "am"
#  $ domain    :List of 4
#   ..$ left  : num 1.32
#   ..$ right : num 5.62
#   ..$ bottom: num 9.22
#   ..$ top   : num 35.1
#  $ range     :List of 4
#   ..$ left  : num 172
#   ..$ right : num 300
#   ..$ bottom: num 144
#   ..$ top   : num 28.5
#  $ log       :List of 2
#   ..$ x: NULL
#   ..$ y: NULL
#  $ .nonce    : num 0.603

# Helper to determine if data values are within the limits of
# an input brush
within_brush <- function(vals, brush, var = "x") {
  var <- match.arg(var, c("x", "y"))
  vals <- asNumber(vals, brush$domain$discrete_limits[[var]])
  # It's possible for a non-missing data values to not
  # map to the axis limits, for example:
  # https://github.com/rstudio/shiny/pull/2410#issuecomment-488100881
  !is.na(vals) &
    vals >= brush[[paste0(var, "min")]] &
    vals <= brush[[paste0(var, "max")]]
}

# Coerce various types of variables to numbers. This works for Date, POSIXt,
# characters, and factors. Used because the mouse coords are numeric.
# The `levels` argument should be used when mapping this variable to
# a known set of discrete levels, which is needed for ggplot2 since
# it allows you to control ordering and possible values of a discrete
# positional scale (#2410)
asNumber <- function(x, levels = NULL) {
  if (length(levels)) return(match(x, levels))
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
