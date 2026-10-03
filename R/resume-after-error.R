#' @include utils.R
NULL

# The error text the fatal-error dialog may show.
after_error_message <- function(e) {
  if (isTRUE(getOption("shiny.sanitize.errors", FALSE))) return(NULL)
  conditionMessage(e)
}
