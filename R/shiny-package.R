# See also R/reexports.R

## usethis namespace: start
## usethis namespace: end
#' @importFrom lifecycle deprecated
#' @importFrom grDevices dev.set dev.cur
#' @importFrom fastmap fastmap
#' @importFrom promises %...!%
#' @importFrom promises %...>%
#' @importFrom promises
#'    promise promise_resolve promise_reject is.promising
#'    as.promise
#' @importFrom rlang
#'    quo enquo as_function get_expr get_env new_function enquos
#'    eval_tidy expr pairlist2 new_quosure enexpr as_quosure is_quosure inject
#'    enquos0 zap_srcref %||% is_na
#'    is_false list2
#'    missing_arg is_missing maybe_missing
#' @importFrom ellipsis
#'    check_dots_empty check_dots_unnamed
#' @import htmltools
#' @import httpuv
#' @import xtable
#' @import R6
#' @import mime
NULL

# It's necessary to Depend on methods so Rscript doesn't fail. It's necessary
# to import(methods) in NAMESPACE so R CMD check doesn't complain. This
# approach isn't foolproof because Rscript -e pkgname::func() doesn't actually
# cause methods to be attached, but it's not a problem for shiny::runApp()
# since we call require(shiny) as part of loading the app.
#' @import methods
NULL
