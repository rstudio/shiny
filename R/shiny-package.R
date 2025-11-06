# See also R/reexports.R

## usethis namespace: start
#' @importFrom lifecycle deprecated is_present
#' @importFrom grDevices dev.set dev.cur
#' @importFrom fastmap fastmap
#' @importFrom promises
#'     %...!% %...>%
#'    as.promise is.promising is.promise
#'    promise_resolve promise_reject
#'    hybrid_then
#'    with_promise_domain new_promise_domain
#' @importFrom rlang
#'    quo enquo enquo0 as_function get_expr get_env new_function enquos
#'    eval_tidy expr pairlist2 new_quosure enexpr as_quosure is_quosure inject
#'    quo_set_env quo_set_expr quo_get_expr
#'    enquos0 zap_srcref %||% is_na
#'    is_false list2
#'    missing_arg is_missing maybe_missing
#'    quo_is_missing fn_fmls<- fn_body fn_body<-
#'    check_dots_empty check_dots_unnamed
#' @import htmltools
#' @import httpuv
#' @import xtable
#' @import R6
#' @import mime
## usethis namespace: end
NULL

# It's necessary to Depend on methods so Rscript doesn't fail. It's necessary
# to import(methods) in NAMESPACE so R CMD check doesn't complain. This
# approach isn't foolproof because Rscript -e pkgname::func() doesn't actually
# cause methods to be attached, but it's not a problem for shiny::runApp()
# since we call require(shiny) as part of loading the app.
#' @import methods
NULL


# For usethis::use_release_issue()
release_bullets <- function() {
  c(
    "Update static imports: `staticimports::import()`"
  )
}


# To get around R CMD check lint
`_ignore` <- function() {
  otelsdk::with_otel_record
}
