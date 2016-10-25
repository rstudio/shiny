#' Register expressions that will be evaluated when a test snapshot occurs
#'
#' This function registers expressions that will be called when a test snapshot
#' occurs. These test snapshots are available at an API endpoint URL.
#'
#' This function only has an effect if the global option \code{shiny.testing} is
#' set to \code{TRUE}.
#'
#' @param quoted_ Are the expression quoted? Default is \code{FALSE}.
#' @param envir_ The environment in which the expression should be evaluated.
#' @param session_ A Shiny session object.
#' @param ... Named arguments that are quoted or unquoted expressions that will
#'   be captured and evaluated when API endpoint is visited.
#'
#' @export
onTestSnapshot <- function(..., quoted_ = FALSE, env_ = parent.frame(),
  session_ = getDefaultReactiveDomain())
{
  session_$onTestSnapshot(..., quoted_ = quoted_, env_ = env_)
}
