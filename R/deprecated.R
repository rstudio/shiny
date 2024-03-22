
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
  version,
  what,
  with = NULL,
  details = NULL,
  type = c("deprecated", "superseded")
) {
  if (is_false(getOption("shiny.deprecation.messages"))) {
    return(invisible())
  }

  type <- match.arg(type)

  msg <- paste0("`", what, "` is ", type, " as of shiny ", version, ".")
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

  # Capture calling function
  grandparent_call <- sys.call(-2)
  # Turn language into user friendly string
  grandparent_txt <- paste0(utils::capture.output({grandparent_call}), collapse = "\n")

  msg <- paste0(
    "The `env` and `quoted` arguments are deprecated as of shiny 1.7.0.",
    " Please use quosures from `rlang` instead.\n",
    "See <https://github.com/rstudio/shiny/issues/3108> for more information.\n",
    "Function call:\n",
    grandparent_txt
  )
  # Call less often as users do not have much control over this warning
  rlang::inform(message = msg, .frequency = "regularly", .frequency_id = msg, .file = stderr())
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
  exec_missing = deprecated(),
  logfile = NULL
) {
  shinyDeprecated("1.6.0", "diskCache()", "cachem::cache_disk()")
  if (is_present(exec_missing)) {
    shinyDeprecated("1.6.0", "diskCache(exec_missing =)")
  }

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
  exec_missing = deprecated(),
  logfile = NULL)
{
  shinyDeprecated("1.6.0", "diskCache()", "cachem::cache_mem()")
  if (is_present(exec_missing)) {
    shinyDeprecated("1.6.0", "diskCache(exec_missing =)")
  }

  cachem::cache_mem(
    max_size = max_size,
    max_age = max_age,
    max_n = max_n,
    evict = evict,
    missing = missing,
    logfile = logfile
  )
}
