

# #' Enable Shiny developer mode
# #'
# #' Sets many options to be used by Shiny application developers.
# #'
# #' @param enable Value to set all Shiny developer settings. See details for logical values. If `NULL`, no action will be taken.
# #'   [options()] set when `enable` is `TRUE`:
# #'   * `shiny.devmode = TRUE`: Enable Shiny developer mode
# #'   * `shiny.launch.browser = TRUE`: Always launch the shiny application window
# #'   * `shiny.minified = FALSE`: Use `shiny.js` for easier debugging
# #'   * `shiny.autoreload = TRUE`: Autoreload when `R/` files change
# #'   * `shiny.fullstacktrace = TRUE`: Display full error stack traces
# #'   * `sass.cache = FALSE `: Disable default `sass` caching
# #'
# #'   If `enable` is `FALSE`, all [options()] described above will be set to `NULL` to allow for default behavior.
# #' @param verbose Value to enable printing output. To disable message output, set to `FALSE`.
# #' @details
# #' In [runApp()], options `shiny.devmode` and `shiny.devmode.verbose` are used to set `enable` and `verbose` respectively.
# #'
# #' @export
# #' @examples
# #'
# #' # To enable Shiny dev mode for an application,
# #' # place this line of code near the top of your Shiny application
# #' \dontrun{shiny_dev_mode(TRUE)}
# #'
# #' # To enable Shiny dev mode for all locally ran applications,
# #' # place this line of code within your `.Rprofile`.
# #' \dontrun{options(shiny.devmode = TRUE)}
# #' # (By only setting the option, `shiny` will not be loaded for every R session)
# shiny_dev_mode <- function(
#   enable = TRUE,
#   verbose = TRUE
# ) {
#   # no dev mode to be set. return!
#   if (is.null(enable)) {
#     return()
#   }

#   enable <- isTRUE(enable)
#   verbose <- isTRUE(verbose)

#   # list of list(desc, opt)
#   messages <- list()
#   # options list of information to set
#   opts_to_set <- list()

#   setup_option <- function(key, enable_val, enable_msg = "", disable_val = NULL) {
#     val <- if (enable) enable_val else disable_val
#     # store the key/val pair
#     # store a list of size 1 at the key to allow storage of NULL values
#     opts_to_set[key] <<- list(val)

#     cur_val <- getOption(key)
#     # only report values that have changed
#     if (verbose && !identical(val, cur_val)) {
#       info <- list()
#       val_txt <- paste0(deparse(val, width.cutoff = 500L), collapse = "")
#       info$desc <- if (enable) paste0(" - ", enable_msg) else ""
#       info$opt <- paste0("options(", key, " = ", val_txt, ")")
#       messages <<- append(messages, list(info))
#     }
#   }

#   # shiny.devmode = TRUE
#   setup_option(
#     "shiny.devmode", TRUE,
#     "Enables Shiny developer mode",
#   )

#   # shiny.launch.browser = TRUE
#   setup_option(
#     "shiny.launch.browser", TRUE,
#     "Always launch Shiny app",
#   )

#   # shiny.minified = FALSE
#   setup_option(
#     "shiny.minified", FALSE,
#     "Using un-minified `shiny.js`",
#   )

#   # shiny.autoreload = TRUE
#   setup_option(
#     "shiny.autoreload", TRUE,
#     "Auto-reloading on R/ folder source changes",
#   )

#   # shiny.fullstacktrace = TRUE
#   setup_option(
#     "shiny.fullstacktrace", TRUE,
#     "Printing full error stack traces",
#   )

#   # disable sass caching
#   setup_option(
#     "sass.cache", FALSE,
#     "`sass` cache disabled",
#   )

#   if (verbose && length(messages) > 0) {
#     # pretty print all information as a single, formatted message
#     desc_txt <- vapply(messages, `[[`, character(1), "desc")
#     opts_txt <- vapply(messages, `[[`, character(1), "opt")
#     opts_txt <- format(opts_txt, justify = "left")
#     pre_txt <-
#       if (enable)
#         "Using Shiny developer mode\nSetting options:"
#       else
#         "Stopping Shiny developer mode\nResetting options:"
#     message(
#       pre_txt, "\n",
#       paste0(
#         "  ", opts_txt, desc_txt,
#         collapse = "\n"
#       )
#     )
#   }

#   # return the return value when setting the `options()`
#   options(opts_to_set)
# }

# ------------------------------------------


#' Shiny Developer Mode
#'
#' @keywords internal
#' @describeIn shiny_dev_mode A small function to set two options to enable/disable Shiny Developer Mode and whether or not to display Developer messages
#' @param dev_mode Logical value which should be set to TRUE to enable Shiny Developer Mode
#' @param verbose Logical value which should be set to display Shiny Developer messages
#' @examples
#' # Enable Shiny Developer mode
#' shiny_dev_mode()
shiny_dev_mode <- function(
  dev_mode = getOption("shiny.devmode", TRUE),
  verbose = getOption("shiny.devmode.verbose", TRUE)
) {
  options(
    shiny.devmode = dev_mode,
    shiny.devmode.verbose = verbose
  )
}


#' @describeIn shiny_dev_mode Determines if Shiny is in Developer Mode
#' @examples
#' in_shiny_dev_mode() # TRUE/FALSE?
in_shiny_dev_mode <- function() {
  isTRUE(getOption("shiny.devmode", FALSE)) &&
    # !testthat::is_testing()
    !identical(Sys.getenv("TESTTHAT"), "true")
}

# ' @describeIn shiny_dev_mode Determines if Shiny Developer Mode messages should be displayed. Each message is displayed only once every 8 hours
# ' @examples
# ' is_shiny_dev_mode_verbose() # TRUE/FALSE?
### Do not document
is_shiny_dev_mode_verbose <- function() {
  in_shiny_dev_mode() && isTRUE(getOption("shiny.devmode.verbose", TRUE))
}


#' @describeIn shiny_dev_mode Temporarily set Shiny Developer Mode and Developer message verbosity
#' @param code Code to execute with the temporary `options()` set
#' @examples
#' with_shiny_dev_mode(TRUE, in_shiny_dev_mode()) # TRUE
with_shiny_dev_mode <- function(
  dev_mode,
  code,
  verbose = getOption("shiny.devmode.verbose", TRUE)
) {
  with_options(
    list(
      shiny.devmode = dev_mode,
      shiny.devmode.verbose = verbose
    ),
    code
  )
}

#' @describeIn  shiny_dev_mode Helper method to retrieve an `options()` value. If the `option` is not set, `on_value` or `off_value` will be returned depending on the result of `in_shiny_dev_mode()`.  When Shiny Developer Mode is on, `on_msg` will be displayed using `shiny_dev_mode_msg()`
#' @param option Option to look for in `options()`
#' @param on_msg Message to display once every 8 hours if `option` is not set and `in_shiny_dev_mode()` returns `TRUE`.
#' @param on_value Default value to use when `option` is not set and `in_shiny_dev_mode()` returns `TRUE`.
#' @param off_value Default value to use when `option` is not set and `in_shiny_dev_mode()` returns `FALSE`.
shiny_dev_mode_option <- function(
  option,
  on_msg,
  on_value = NULL,
  off_value = NULL
) {
  getOption(
    option,
    {
      if (in_shiny_dev_mode()) {
        shiny_dev_mode_msg(msg = on_msg)
        enabled_value
      } else {
        # typical case
        disabled_value
      }
    }
  )
}

#' @describeIn shiny_dev_mode If Shiny Developer Mode is enabled, displays a message (`msg`) once every 8 hrs
#' @param ... Values passed into [paste0()] to create `msg`
#' @param msg Message to be displayed
#' @inheritParams rlang::inform
shiny_dev_mode_msg <- function(
  ...,
  msg = paste0(...),
  .frequency = "once",
  .frequency_id = msg
) {
  if (is_shiny_dev_mode_verbose()) {
    rlang::inform(
      message = paste0("shiny dev mode - ", msg),
      .frequency = frequency,
      .frequency_id = id
    )
  }
}
