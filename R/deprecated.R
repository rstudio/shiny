
#' Print message for deprecated functions in Shiny
#'
#' To disable these messages, use `options(shiny.deprecation.messages=FALSE)`.
#'
#' @param version Shiny version when the function was deprecated
#' @param what Function with possible arguments
#' @param with Possible function with arguments that should be used instead
#' @param details Additional information to be added after a new line to the displayed message
#' @keywords internal
shinyDeprecated <- function(
  version, what, with = NULL, details = NULL
) {
  if (is_false(getOption("shiny.deprecation.messages"))) {
    return(invisible())
  }

  msg <- paste0("`", what, "` is deprecated as of shiny ", version, ".")
  if (!is.null(with)) {
    msg <- paste0(msg, "\n", "Please use `", with, "` instead.")
  }
  if (!is.null(details)) {
    msg <- paste0(msg, "\n", details)
  }

  # lifecycle::deprecate_soft(when, what, with = with, details = details, id = id, env = env)
  rlang::inform(message = msg, .frequency = "always", .frequency_id = msg, .file = stderr())
}


deprecatedEnvQuotedMessage <- function() {
  if (!in_devmode()) return(invisible())
  if (is_false(getOption("shiny.deprecation.messages"))) return(invisible())

  # manually
  msg <- paste0(
    "The `env` and `quoted` arguments are deprecated as of shiny 1.6.0.",
    " Please use quosures from `rlang` instead.\n",
    "See <https://github.com/rstudio/shiny/issues/3108> for more information."
  )
  rlang::inform(message = msg, .frequency = "always", .frequency_id = msg, .file = stderr())
}


#' Create disk cache (deprecated)
#'
#' @param exec_missing Deprecated.
#' @inheritParams cachem::cache_disk
#' @keywords internal
#' @export
diskCache <- function(
  dir = NULL,
  max_size = 500 * 1024 ^ 2,
  max_age = Inf,
  max_n = Inf,
  evict = c("lru", "fifo"),
  destroy_on_finalize = FALSE,
  missing = key_missing(),
  exec_missing = FALSE,
  logfile = NULL)
{
  shinyDeprecated("1.5.1", "diskCache()", "cachem::cache_disk()")

  cachem::cache_disk(
    dir = dir,
    max_size = max_size,
    max_age = max_age,
    max_n = max_n,
    evict = evict,
    destroy_on_finalize = destroy_on_finalize,
    missing = missing,
    logfile = logfile
  )
}


#' Create memory cache (deprecated)
#'
#' @param exec_missing Deprecated.
#' @inheritParams cachem::cache_mem
#' @keywords internal
#' @export
memoryCache <- function(
  max_size = 200 * 1024 ^ 2,
  max_age = Inf,
  max_n = Inf,
  evict = c("lru", "fifo"),
  missing = key_missing(),
  exec_missing = FALSE,
  logfile = NULL)
{
  shinyDeprecated("1.5.1", "diskCache()", "cachem::cache_mem()")

  cachem::cache_mem(
    max_size = max_size,
    max_age = max_age,
    max_n = max_n,
    evict = evict,
    missing = missing,
    logfile = logfile
  )
}
