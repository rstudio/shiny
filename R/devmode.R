#' Shiny Developer Mode
#'
#' @description \lifecycle{experimental}
#'
#' The methods described in this help file provide a way to change default shiny behavior when developing shiny applications (as compared to using a shiny application). For example, the following default option values are altered to have different behavior when Shiny Developer Mode is enabled:
#'
#' * `getOption("shiny.autoreload", default = TRUE)`: Reload the Shiny app when a sourced R file changes
#' * `getOption("shiny.minified", default = FALSE`): Use the unminified Shiny JavaScript file, `shiny.js`
#' * `getOption("shiny.fullstacktrace", default = TRUE`): Display the full stack trace when errors occur during Shiny app execution
#'
#' Other known, non-Shiny default option values that are changed when Shiny Developer Mode is enabled:
#' * `getOption("sass.cache", default = FALSE)`: Disable `sass` caching by default. This will force `sass` to **always** compile its input values
#'
#'
#' @keywords internal
#' @describeIn devmode Function to set two options to enable/disable Shiny Developer Mode and Developer messages
#' @param dev_mode Logical value which should be set to `TRUE` to enable Shiny Developer Mode
#' @param verbose Logical value which should be set to `TRUE` display Shiny Developer messages
#' @export
#' @examples
#' # Enable Shiny Developer mode
#' devmode()
#'
devmode <- function(
  dev_mode = getOption("shiny.devmode", TRUE),
  verbose = getOption("shiny.devmode.verbose", TRUE)
) {
  options(
    shiny.devmode = dev_mode,
    shiny.devmode.verbose = verbose
  )
}


#' @describeIn devmode Determines if Shiny is in Developer Mode. If the `getOption("shiny.devmode")` is set to `TRUE` and not in testing inside `testthat`, then Shiny Developer Mode is enabled.
#' @section Avoiding direct dependency on shiny:
#'
#' The methods explained in this documentation file act independently from the rest of Shiny but are included to provide blue prints for your own packages. If your package already has (or willing to take) a dependency on Shiny, we recommend using the exported methods for consistent behavior.
#'
#' If your package should not take a dependency on Shiny, we recommending re-implementing two functions:
#'
#' 1. `in_devmode()`: This function should return `TRUE` if `getOption("shiny.devmode")` is set. In addition, we strongly recommend that it also checks to make sure `testthat` is not testing.  This can be achieved using:
#' ```r
#' in_devmode <- function() {
#'   isTRUE(getOption("shiny.devmode", FALSE)) &&
#'     # !testthat::is_testing()
#'     !identical(Sys.getenv("TESTTHAT"), "true")
#' }
#' ```
#'
#' 2. `get_devmode_option(option, default, on, message)`:
#' This function should return the global `option` when it is set, similar to `getOption(option, default)`. When the global option `option` is not set, this function should return the default value (`default`) when `in_devmode()` is `FALSE` and the default Dev Mode value (`on`) when `in_devmode()` is `TRUE`. We strongly recommend displaying a message (`message`) to the developer once every 8 hours using `rlang::inform(message, .frequency = "regularly", .frequency_id = message)` if returning the `on` default Dev Mode option value. This will keep the author up to date as to which behaviors are being altered. To allow developers a chance to disable Dev Mode messages, the message may be skipped if `getOption("shiny.devmode.verbose", TRUE)` returns `FALSE`.
#'
#' ```r
#' get_devmode_option <- function(option, default = NULL, on = NULL, message = NULL) {
#'   if (!in_devmode()) {
#'     # Dev Mode disabled, act like `getOption()`
#'     return(getOption(option, default = default))
#'   }
#'   # Notify developer
#'   if (getOption("shiny.devmode.verbose", TRUE) && !is.null(message)) {
#'     rlang::inform(message = message, .frequency = "regularly", .frequency_id = message)
#'   }
#'   # Return Dev Mode default value `on`
#'   getOption(option, default = on)
#' }
#' ```
#'
#' The remaining functions in this file are used for author convenience and are not recommended for all situations.
#' @export
#' @examples
#' in_devmode() # TRUE/FALSE?
#'
in_devmode <- function() {
  isTRUE(getOption("shiny.devmode", FALSE)) &&
    # !testthat::is_testing()
    !identical(Sys.getenv("TESTTHAT"), "true")
}

#' @describeIn devmode Temporarily set Shiny Developer Mode and Developer message verbosity
#' @param code Code to execute with the temporary Dev Mode options set
#' @export
#' @examples
#' # Execute code in a temporary shiny dev mode
#' with_devmode(TRUE, in_devmode()) # TRUE
#'
with_devmode <- function(
  dev_mode,
  code,
  verbose = getOption("shiny.devmode.verbose", TRUE)
) {
  withr::with_options(
    list(
      shiny.devmode = dev_mode,
      shiny.devmode.verbose = verbose
    ),
    code
  )
}


#' @describeIn devmode If Shiny Developer Mode and verbosity are enabled, displays a message once every 8 hrs (by default)
#' @inheritParams rlang::inform
#' @param ... Parameters passed to [rlang::inform()]
devmode_inform <- function(
  message,
  .frequency = "regularly",
  .frequency_id = message,
  ...
) {

  if (!(
    in_devmode() &&
      isTRUE(getOption("shiny.devmode.verbose", TRUE))
  )) {
    return()
  }
  if (is.null(message)) {
    return()
  }

  rlang::inform(
    message = paste0("shiny dev mode - ", message),
    .frequency = .frequency,
    .frequency_id = .frequency_id,
    ...
  )
}




#' @include map.R
registered_devmode_options <- Map$new()

#' @describeIn devmode Registers a Shiny Developer Mode option with an updated value and Developer message. This registration method allows package authors to write one message in a single location
#' @param option Option to look for in `options()`
#' @param message Message to display once every 8 hours if `option` is not set and if `in_devmode()` returns `TRUE` and Dev Mode messages can be displayed. For `get_devmode_option()`, if `message = waiver()`, the registered `message` value be attempted to be displayed.
#' @param on Default value to return if `in_devmode()` returns `TRUE`. For `get_devmode_option()`, if `on = waiver()`, the registered default `on` value will be used.
#' @examples
#' # Ex: Within shiny, we register the option "shiny.minified"
#' #   to default to `FALSE` when in Dev Mode
#' \dontrun{register_devmode_option(
#'   "shiny.minified",
#'   message = paste0(
#'     "Using full shiny javascript file. ",
#'     "To use the minified version, call `options(shiny.minified = TRUE)`"
#'   ),
#'   on = FALSE
#' )}
#'
register_devmode_option <- function(
  option,
  message = NULL,
  on = NULL
) {
  if (!is.null(message)) {
    stopifnot(length(message) == 1 && is.character(message))
  }
  registered_devmode_options$set(
    option,
    list(on = on, message = message)
  )
}
registered_devmode_options

#' @describeIn  devmode Return a global option but allow the default value to be different if Shiny Developer Mode is enabled. This method is very similar to [getOption()] where the globally set option takes precedence. However, when the global option is not set, `default` will be returned when `in_devmode()` is `FALSE`, or `on` will be returned if `in_devmode()` is `TRUE`.
#' @export
#' @examples
#' # Used within `shiny::runApp(launch.browser)`
#' get_devmode_option("shiny.minified", TRUE) # TRUE if Dev mode is off
#' is_minified <- with_devmode(TRUE, {
#'   get_devmode_option("shiny.minified", TRUE)
#' })
#' is_minified # FALSE
#'
get_devmode_option <- function(
  option,
  default = NULL,
  on = waiver(),
  message = waiver()
) {
  getOption(
    option,
    local({
      if (!in_devmode()) {
        # typical case
        return(default)
      }

      info <- registered_devmode_options$get(option)
      if (is.null(info)) {
        # Not registered,
        # Warn and return default value
        rlang::warn(
          message = paste0(
            "`get_devmode_option(option)` could not find `option = \"", option, "\"`. ",
            "Returning `default` value"
          )
        )
        return(default)
      }

      # display message
      if (is_waiver(message)) {
        # no custom message found. Display default devmode message
        message <- info$message
      }
      devmode_inform(message = message)

      # return registered on value
      if (is_waiver(on)) {
        # use default devmode value
        return(info$on)
      }

      # use provided `on` value
      on
    })
  )
}

is_waiver <- function(x) {
  inherits(x, "waiver")
}
waiver <- function() {
  structure(
    list(),
    class = "waiver"
  )
}



register_devmode_option(
  "shiny.autoreload",
  "Turning on shiny autoreload. To disable, call `options(shiny.autoreload = FALSE)`",
  TRUE
)

register_devmode_option(
  "shiny.minified",
  "Using full shiny javascript file. To use the minified version, call `options(shiny.minified = TRUE)`",
  FALSE
)

register_devmode_option(
  "shiny.fullstacktrace",
  "Turning on full stack trace. To disable, call `options(shiny.fullstacktrace = FALSE)",
  TRUE
)
