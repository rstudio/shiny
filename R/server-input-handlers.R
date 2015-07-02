# Create a map for input handlers and register the defaults.
inputHandlers <- Map$new()

#' Register an Input Handler
#'
#' Adds an input handler for data of this type. When called, Shiny will use the
#' function provided to refine the data passed back from the client (after being
#' deserialized by jsonlite) before making it available in the \code{input}
#' variable of the \code{server.R} file.
#'
#' This function will register the handler for the duration of the R process
#' (unless Shiny is explicitly reloaded). For that reason, the \code{type} used
#' should be very specific to this package to minimize the risk of colliding
#' with another Shiny package which might use this data type name. We recommend
#' the format of "packageName.widgetName".
#'
#' Currently Shiny registers the following handlers: \code{shiny.matrix},
#' \code{shiny.number}, and \code{shiny.date}.
#'
#' The \code{type} of a custom Shiny Input widget will be deduced using the
#' \code{getType()} JavaScript function on the registered Shiny inputBinding.
#' @param type The type for which the handler should be added -- should be a
#' single-element character vector.
#' @param fun The handler function. This is the function that will be used to
#'   parse the data delivered from the client before it is available in the
#'   \code{input} variable. The function will be called with the following three
#'   parameters:
#'    \enumerate{
#'      \item{The value of this input as provided by the client, deserialized
#'      using jsonlite.}
#'      \item{The \code{shinysession} in which the input exists.}
#'      \item{The name of the input.}
#'    }
#' @param force If \code{TRUE}, will overwrite any existing handler without
#' warning. If \code{FALSE}, will throw an error if this class already has
#' a handler defined.
#' @examples
#' \dontrun{
#' # Register an input handler which rounds a input number to the nearest integer
#' registerInputHandler("mypackage.validint", function(x, shinysession, name) {
#'   if (is.null(x)) return(NA)
#'   round(x)
#' })
#'
#' ## On the Javascript side, the associated input binding must have a corresponding getType method:
#' getType: function(el) {
#'   return "mypackage.validint";
#' }
#'
#' }
#' @seealso \code{\link{removeInputHandler}}
#' @export
registerInputHandler <- function(type, fun, force=FALSE){
  if (inputHandlers$containsKey(type) && !force){
    stop("There is already an input handler for type: ", type)
  }
  inputHandlers$set(type, fun)
}

#' Deregister an Input Handler
#'
#' Removes an Input Handler. Rather than using the previously specified handler
#' for data of this type, the default jsonlite serialization will be used.
#'
#' @param type The type for which handlers should be removed.
#' @return The handler previously associated with this \code{type}, if one
#'   existed. Otherwise, \code{NULL}.
#' @seealso \code{\link{registerInputHandler}}
#' @export
removeInputHandler <- function(type){
  inputHandlers$remove(type)
}

# Takes a list-of-lists and returns a matrix. The lists
# must all be the same length. NULL is replaced by NA.
registerInputHandler("shiny.matrix", function(data, ...) {
  if (length(data) == 0)
    return(matrix(nrow=0, ncol=0))

  m <- matrix(unlist(lapply(data, function(x) {
    sapply(x, function(y) {
      ifelse(is.null(y), NA, y)
    })
  })), nrow = length(data[[1]]), ncol = length(data))
  return(m)
})

registerInputHandler("shiny.number", function(val, ...){
  ifelse(is.null(val), NA, val)
})

registerInputHandler("shiny.date", function(val, ...){
  # First replace NULLs with NA, then convert to Date vector
  datelist <- ifelse(lapply(val, is.null), NA, val)
  as.Date(unlist(datelist))
})

registerInputHandler("shiny.datetime", function(val, ...){
  # First replace NULLs with NA, then convert to POSIXct vector
  times <- lapply(val, function(x) {
    if (is.null(x)) NA
    else x
  })
  as.POSIXct(unlist(times), origin = "1970-01-01", tz = "UTC")
})

registerInputHandler("shiny.action", function(val, ...) {
  # mark up the action button value with a special class so we can recognize it later
  class(val) <- c(class(val), "shinyActionButtonValue")
  val
})
