

rlang_inform <- function(..., msg = paste0(...), frequency = "always", id = msg) {
  rlang::inform(
    message = msg,
    .frequency = frequency,
    .frequency_id = id
  )
}
rlang_warn <- function(..., msg = paste0(...), frequency = "always", id = msg) {
  rlang::warn(
    message = msg,
    .frequency = frequency,
    .frequency_id = id
  )
}

#' Print message for deprecated functions in Shiny
#'
#' To disable these messages, use `options(shiny.deprecation.messages=FALSE)`.
#'
#' @inheritParams lifecycle::deprecate_soft
#' @keywords internal
#' @describeIn shiny-deprecated Passes arguments to [lifecycle::deprecate_soft()]
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
  rlang_inform(msg = msg, frequency = "always")
}


#' Print message for deprecated functions in Shiny
#'
#' \lifecycle{superseded} Please use [shinySoftDeprecated()]
#'
#' To disable these messages, use `options(shiny.deprecation.messages=FALSE)`.
#'
#' @param new Name of replacement function.
#' @param msg Message to print. If used, this will override the default message.
#' @param old Name of deprecated function.
#' @param version The last version of Shiny before the item was deprecated.
#' @keywords internal
shinyDeprecated <- function(new=NULL, msg=NULL,
                            old=as.character(sys.call(sys.parent()))[1L],
                            version = NULL) {

  if (getOption("shiny.deprecation.messages") %OR% TRUE == FALSE)
    return(invisible())

  if (is.null(msg)) {
    msg <- paste(old, "is deprecated.")
    if (!is.null(new)) {
      msg <- paste(msg, "Please use", new, "instead.",
        "To disable this message, run options(shiny.deprecation.messages=FALSE)")
    }
  }

  if (!is.null(version)) {
    msg <- paste0(msg, " (Last used in version ", version, ")")
  }

  # Similar to .Deprecated(), but print a message instead of warning
  rlang_warn(msg, frequency = "always")
}


deprecatedEnvQuotedMessage <- function(env_arg = "env", quoted_arg = "quoted") {
  # Enable this message in a future version of Shiny, perhaps in a dev_edition()
  # mode.
  # shinyDeprecated(msg = paste(
  #   sprintf("The `%s` and `%s` arguments are deprecated.", env_arg, quoted_arg),
  #   "Please use quosures from rlang instead.",
  #   "See https://github.com/rstudio/shiny/issues/3108 for more information."
  # ))
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
  shinySoftDeprecated("1.5.1", "diskCache()", "cachem::cache_disk()")

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
  shinySoftDeprecated("1.5.1", "diskCache()", "cachem::cache_mem()")

  cachem::cache_mem(
    max_size = max_size,
    max_age = max_age,
    max_n = max_n,
    evict = evict,
    missing = missing,
    logfile = logfile
  )
}
