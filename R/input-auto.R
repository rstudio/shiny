#' Automatic Input Widget
#'
#' Constructs an input based on the type and values of provided data.
#'
#' @details Widgets are chosen according to the class of \code{x}:
#' \itemize{
#'   \item \strong{boolean}: \code{\link{checkboxInput}};
#'
#'   \item \strong{character}, \strong{factor}: \code{\link{checkboxGroupInput}} if there are 20 or fewer unique values, \code{\link{selectInput}} with \code{multiple=TRUE} otherwise;
#'
#'   \item \strong{Date}: \code{\link{dateInput}} if \code{range=FALSE}, \code{\link{dateRangeInput}} if \code{range=TRUE};
#'
#'   \item \strong{numeric}, \strong{POSIXct} (date and time): \code{\link{sliderInput}} with a single value if \code{range=FALSE} or two if \code{range=TRUE}.
#' }
#' In this last case, a simple heuristic is used to create good looking bounds for the slider, although the range selected by default is based on the actual data range. For example, if \code{x} varies between 2.16 and 3.98 and \code{selected_fraction=1}, the slider goes from 2 to 4 but the range initially selected would be [2.16, 3.98].
#'
#' @param x Input data, which can be a vector, a one-column \code{data.frame}, or a one-element \code{list}. NB: to extract a one-column data.frame or a single element list from a larger one, provide a \emph{single} index to \code{`[`}, i.e. use \code{df[1]} instead of \code{df[,1]} and \code{l[1]} instead of \code{l[[1]]}, the later forms extract the corresponding vector/content; see the examples.
#' @param inputId The \code{input} slot that will be used to access the value. With the default value of \code{NA}, if \code{x} has a \code{names} attribute of length one (i.e. in the case of a one-column \code{data.frame}, or a one-element \code{list}), it will be used as \code{inputId}. Specifying an \code{inputId} overrides this behaviour.
#' @param label Display label for the control, by default set to the value of \code{inputId}. Set this explicitly to \code{NULL} for no label.
#' @param selected_fraction Fraction of the data (between 0 and 1) to select upon creation of the widget.
#' @param range Whether to create a widget that allows selecting a range rather than a single value.
#' @param inline If TRUE, render the choices inline (i.e. horizontally) in \code{\link{checkboxGroupInput}}.
#' @param ... passed to the input widget creation functions.
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#' # simple example
#' set.seed(1)
#' autoInput(runif(10), "foo", "My automatic slider")
#'
#' # create a various types of data
#' n <- 40
#' d <- data.frame(
#'   boolean=rep(c(TRUE, FALSE), times=n/2),
#'   few_levels=letters[1:5],
#'   many_levels=rep(letters,2)[1:n],
#'   int=1:n,
#'   num=runif(n, 0, 5),
#'   large_num=runif(n, 0, 10^6),
#'   small_num=runif(n, 0, 0.001),
#'   date=Sys.Date()+1:n,
#'   date_time=Sys.time()+1:n
#' )
#' str(d)
#'
#' ui <- fluidPage(
#'   fluidRow(
#'     column(
#'       6,
#'       h2("Gallery"),
#'       autoInput(d["boolean"]),
#'       autoInput(d["few_levels"]),
#'       autoInput(d["many_levels"]),
#'       autoInput(d["int"]),
#'       autoInput(d["num"]),
#'       autoInput(d["large_num"]),
#'       autoInput(d["small_num"]),
#'       autoInput(d["date"]),
#'       autoInput(d["date"], range=F),
#'       autoInput(d["date_time"]),
#'       autoInput(d["date_time"], range=F)
#'     ),
#'     column(
#'       6,
#'       h2("Options"),
#'       p("width=100%:"), autoInput(d["int"], width="100%", inline=TRUE),
#'       p("selected_fraction=1:"), autoInput(d["int"], selected_fraction=1),
#'       p("selected_fraction=0:"), autoInput(d["int"], selected_fraction=0),
#'       p("selected_fraction=0.0001; when the variable is categorical, the number of elements is  rounded up to select at least one:"),
#'       autoInput(d["few_levels"], selected_fraction=0.0001),
#'       p("to select nothing, set selected_fraction to exactly 0:"),
#'       autoInput(d["few_levels"], selected_fraction=0),
#'       p("inline=FALSE:"), autoInput(d["few_levels"], inline=FALSE),
#'       p("Change label and ticks aspect:"),
#'       autoInput(d["small_num"], label=NULL),
#'       autoInput(d["small_num"], sep=""),
#'       autoInput(d["small_num"], ticks=FALSE),
#'       p("etc.")
#'     )
#'   )
#' )
#' server <- function(input, output) {}
#' shinyApp(ui, server)
#' }
#' @export
autoInput <- function(x, inputId=NA, label=inputId, selected_fraction=0.5, range=TRUE, inline=TRUE, ...) {
  # get variable name
  if (is.na(inputId)) {
    inputId <- names(x)
    if (length(inputId) != 1) {
      stop("Variable name cannot be extracted from `x` to be used as `inputId`. Provide one.")
    }
    if (!is.null(label)) {
      if (is.na(label)) {
        label <- inputId
      }
    }
  }

  # allow single column data.frame/list/tibble etc. as input
  if (is.list(x)) {
    x <- x[[1]]
  }

  ## Categorical data
  # logical
  if (is.logical(x)) {
    ui <- checkboxInput(inputId, label, ...)
    # NB: do not set the value, let the default in checkboxInput play this role
  }
  # character/factor
  else if (is.character(x) | is.factor(x)) {
    # turn variable into a factor to force ordering and ease extraction of unique levels
    x <- factor(x)
    choices <- levels(x)
    if (selected_fraction == 0) {
      selected_choices=NULL
    } else {
      selected_choices <- choices[1:ceiling(selected_fraction * nlevels(x))]
    }
    # depending on number of possibilities, use checkboxes or a multi-select
    if (length(choices) <= 20) {
      ui <- checkboxGroupInput(inputId, label, choices=choices, selected=selected_choices, inline=inline, ...)
    } else {
      ui <- selectInput(inputId, label, choices=choices, selected=selected_choices, multiple=TRUE, ...)
    }
  }
  ## Continuous variables
  else {
    # compute the range and the portion of it initially selected
    r <- range(x, na.rm=TRUE)
    span <- diff(r)
    value <- c(r[1], r[1]+span*selected_fraction)
    # if we do not select a range, pick the highest end
    # (the widget, e.g. slider, will naturally start at the lower end)
    if (!range) {
      value <- value[2]
    }

    # dates
    # NB: there's no is.Date() function that I can find
    if (class(x)[1] == "Date") {
      if (range) {
        ui <- dateRangeInput(inputId, label, min=r[1], max=r[2], start=value[1], end=value[2], ...)
      } else {
        ui <- dateInput(inputId, label, min=r[1], max=r[2], value=value, ...)
      }
    }
    # all other continous variables
    else {
      # guess a good rounding interval based in the span of the data, e.g.
      # data spanning 1 -> 10^6 can probably be rounded at 100
      # data spanning 0 -> 0.0234 needs to be rounded at 0.01
      if (span < 1) {
        n <- -nchar(prettyNum(round(1/span)))
      } else {
        n <- max(0, nchar(prettyNum(round(span))) - 2)
      }
      accuracy <- 10^n
      # redefine round_any from plyr to avoid depending on it
      # NB: this is complicated by the fact that we need to deal with POSIXct objects too here
      round_any <- function(x, accuracy, f=round){
        is_POSIX <- (class(x)[1] == "POSIXct") # NB: no is.POSIXct either
        if (is_POSIX) {tz <- format(x[1], "%Z")}
        xr <- f(as.numeric(x)/ accuracy) * accuracy
        if (is_POSIX) {xr <- as.POSIXct(xr, origin = "1970-01-01 00:00.00 UTC", tz = tz)}
        return(xr)
      }
      # TODO: deal with POSIXct separately and try to be clever about fate formatting based on the range; check what is in scales for this purpose
      # use it to round numbers of POSIXct ranges
      r <- c(round_any(r[1], accuracy, floor), round_any(r[2], accuracy, ceiling))

      ui <- sliderInput(inputId, label, min=r[1], max=r[2], value=value, ...)
    }
  }
  return(ui)
}
