#' Print message for deprecated functions in Shiny
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
  message(msg)
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
  shinyDeprecated("cachem::cache_disk", version = "1.5.1")

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
  shinyDeprecated("cachem::cache_mem", version = "1.5.1")

  cachem::cache_mem(
    dir = dir,
    max_size = max_size,
    max_age = max_age,
    max_n = max_n,
    evict = evict,
    missing = missing,
    logfile = logfile
  )
}
