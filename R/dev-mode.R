

#' Enable Shiny developer mode
#'
#' Sets many options to be used by Shiny application developers.
#'
#' @param enable Value to set all Shiny developer settings. See details for logical values. If `NULL`, no action will be taken.
#'   [options()] set when `enable` is `TRUE`:
#'   * `shiny.devmode = TRUE`: Enable Shiny developer mode
#'   * `shiny.launch.browser = TRUE`: Always launch the shiny application window
#'   * `shiny.minified = FALSE`: Use `shiny.js` for easier debugging
#'   * `shiny.autoreload = TRUE`: Autoreload when `R/` files change
#'   * `shiny.fullstacktrace = TRUE`: Display full error stack traces
#'   * `sass.cache = FALSE `: Disable default `sass` caching
#'
#'   If `enable` is `FALSE`, all [options()] described above will be set to `NULL` to allow for default behavior.
#' @param verbose Value to enable printing output. To disable message output, set to `FALSE`.
#' @details
#' In [runApp()], options `shiny.devmode` and `shiny.devmode.verbose` are used to set `enable` and `verbose` respectively.
#'
#' @export
#' @examples
#'
#' # To enable Shiny dev mode for an application,
#' # place this line of code near the top of your Shiny application
#' \dontrun{shiny_dev_mode(TRUE)}
#'
#' # To enable Shiny dev mode for all locally ran applications,
#' # place this line of code within your `.Rprofile`.
#' \dontrun{options(shiny.devmode = TRUE)}
#' # (By only setting the option, `shiny` will not be loaded for every R session)
shiny_dev_mode <- function(
  enable = TRUE,
  verbose = TRUE
) {
  # no dev mode to be set. return!
  if (is.null(enable)) {
    return()
  }

  enable <- isTRUE(enable)
  verbose <- isTRUE(verbose)

  # list of list(desc, opt)
  messages <- list()
  # options list of information to set
  opts_to_set <- list()

  setup_option <- function(key, enable_val, enable_msg = "", disable_val = NULL) {
    val <- if (enable) enable_val else disable_val
    # store the key/val pair
    # store a list of size 1 at the key to allow storage of NULL values
    opts_to_set[key] <<- list(val)

    cur_val <- getOption(key)
    # only report values that have changed
    if (verbose && !identical(val, cur_val)) {
      info <- list()
      val_txt <- paste0(deparse(val, width.cutoff = 500L), collapse = "")
      info$desc <- if (enable) paste0(" - ", enable_msg) else ""
      info$opt <- paste0("options(", key, " = ", val_txt, ")")
      messages <<- append(messages, list(info))
    }
  }

  # shiny.devmode = TRUE
  setup_option(
    "shiny.devmode", TRUE,
    "Enables Shiny developer mode",
  )

  # shiny.launch.browser = TRUE
  setup_option(
    "shiny.launch.browser", TRUE,
    "Always launch Shiny app",
  )

  # shiny.minified = FALSE
  setup_option(
    "shiny.minified", FALSE,
    "Using un-minified `shiny.js`",
  )

  # shiny.autoreload = TRUE
  setup_option(
    "shiny.autoreload", TRUE,
    "Auto-reloading on R/ folder source changes",
  )

  # shiny.fullstacktrace = TRUE
  setup_option(
    "shiny.fullstacktrace", TRUE,
    "Printing full error stack traces",
  )

  # disable sass caching
  setup_option(
    "sass.cache", FALSE,
    "`sass` cache disabled",
  )

  if (verbose && length(messages) > 0) {
    # pretty print all information as a single, formatted message
    desc_txt <- vapply(messages, `[[`, character(1), "desc")
    opts_txt <- vapply(messages, `[[`, character(1), "opt")
    opts_txt <- format(opts_txt, justify = "left")
    pre_txt <-
      if (enable)
        "Using Shiny developer mode\nSetting options:"
      else
        "Stopping Shiny developer mode\nResetting options:"
    message(
      pre_txt, "\n",
      paste0(
        "  ", opts_txt, desc_txt,
        collapse = "\n"
      )
    )
  }

  # return the return value when setting the `options()`
  options(opts_to_set)
}

in_shiny_dev_mode <- function() {
  isTRUE(getOption("shiny.devmode", FALSE))
}
with_shiny_dev_mode <- function(enable, code, verbose = TRUE) {
  prior_val <- getOption("shiny.devmode", FALSE)
  on.exit({
    shiny_dev_mode(prior_val, verbose = verbose)
  })
  shiny_dev_mode(enable, verbose = verbose)
  force(code)
}
