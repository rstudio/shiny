#' Shiny Developer Mode
#'
#' @description `r lifecycle::badge("experimental")`
#'
#'   Developer Mode enables a number of [options()] to make a developer's life
#'   easier, like enabling non-minified JS and printing messages about
#'   deprecated functions and options.
#'
#'   Shiny Developer Mode can be enabled by calling `devmode(TRUE)` and disabled
#'   by calling `devmode(FALSE)`.
#'
#'   Please see the function descriptions for more details.
#'
#' @describeIn devmode Function to set two options to enable/disable Shiny
#'   Developer Mode and Developer messages
#' @param devmode Logical value which should be set to `TRUE` to enable Shiny
#'   Developer Mode
#' @param verbose Logical value which should be set to `TRUE` display Shiny
#'   Developer messages
#' @export
#' @examples
#' # Enable Shiny Developer mode
#' devmode()
#'
devmode <- function(
  devmode = getOption("shiny.devmode", TRUE),
  verbose = getOption("shiny.devmode.verbose", TRUE)
) {
  options(
    shiny.devmode = devmode,
    shiny.devmode.verbose = verbose
  )
}


#' @describeIn devmode Determines if Shiny is in Developer Mode. If the
#'   `getOption("shiny.devmode")` is set to `TRUE` and not in testing inside
#'   `testthat`, then Shiny Developer Mode is enabled.
#' @section Avoiding direct dependency on shiny:
#'
#'   The methods explained in this help file act independently from the rest of
#'   Shiny but are included to provide blue prints for your own packages. If
#'   your package already has (or is willing to take) a dependency on Shiny, we
#'   recommend using the exported Shiny methods for consistent behavior. Note
#'   that if you use exported Shiny methods, it will cause the Shiny package to
#'   load. This may be undesirable if your code will be used in (for example) R
#'   Markdown documents that do not have a Shiny runtime (`runtime: shiny`).
#'
#'   If your package can **not** take a dependency on Shiny, we recommending
#'   re-implementing these two functions:
#'
#' \enumerate{
#' \item `in_devmode()`:
#'
#'   This function should return `TRUE` if `getOption("shiny.devmode")` is set.
#'   In addition, we strongly recommend that it also checks to make sure
#'   `testthat` is not testing.
#'
#' ```r
#' in_devmode <- function() {
#'   isTRUE(getOption("shiny.devmode", FALSE)) &&
#'     !identical(Sys.getenv("TESTTHAT"), "true")
#' }
#' ```
#'
#' \item `get_devmode_option(name, default, devmode_default, devmode_message)`:
#'
#' This function is similar to `getOption(name, default)`, but when the option
#' is not set, the default value changes depending on the Dev Mode.
#' `get_devmode_option()` should be implemented as follows:
#'
#' * If not in Dev Mode:
#'   * Return `getOption(name, default)`.
#' * If in Dev Mode:
#'   * Get the global option `getOption(name)` value.
#'   * If the global option value is set:
#'     * Return the value.
#'   * If the global option value is not set:
#'     * Notify the developer that the Dev Mode default value will be used.
#'     * Return the Dev Mode default value.
#'
#' When notifying the developer that the default value has changed, we strongly
#' recommend displaying a message (`devmode_message`) to `stderr()` once every 8
#' hours using [rlang::inform()]. This will keep the author up to date as to
#' which Dev Mode options are being altered. To allow developers a chance to
#' disable Dev Mode messages, the message should be skipped if
#' `getOption("shiny.devmode.verbose", TRUE)` is not `TRUE`.
#'
#' ```r
#' get_devmode_option <- function(name, default = NULL, devmode_default, devmode_message) {
#'   if (!in_devmode()) {
#'     # Dev Mode disabled, act like `getOption()`
#'     return(getOption(name, default = default))
#'   }
#'
#'   # Dev Mode enabled, update the default value for `getOption()`
#'   getOption(name, default = {
#'     # Notify developer
#'     if (
#'       !missing(devmode_message) &&
#'       !is.null(devmode_message) &&
#'       getOption("shiny.devmode.verbose", TRUE)
#'     ) {
#'       rlang::inform(
#'         message = devmode_message,
#'         .frequency = "regularly",
#'         .frequency_id = devmode_message,
#'         .file = stderr()
#'       )
#'     }
#'
#'     # Return Dev Mode default value `devmode_default`
#'     devmode_default
#'   })
#' }
#' ```
#' }
#'
#' The remaining functions in this file are used for author convenience and are
#' not recommended for all reimplementation situations.
#' @export
#' @examples
#' in_devmode() # TRUE/FALSE?
#'
in_devmode <- function() {
  isTRUE(getOption("shiny.devmode", FALSE)) &&
    # !testthat::is_testing()
    !identical(Sys.getenv("TESTTHAT"), "true")
}

in_client_devmode <- function() {
  # Client-side devmode enables client-side only dev features without local
  # devmode. Currently, the main feature is the client-side error console.
  isTRUE(getOption("shiny.client_devmode", FALSE))
}

#' @describeIn devmode Temporarily set Shiny Developer Mode and Developer
#'   message verbosity
#' @param code Code to execute with the temporary Dev Mode options set
#' @export
#' @examples
#' # Execute code in a temporary shiny dev mode
#' with_devmode(TRUE, in_devmode()) # TRUE
#'
with_devmode <- function(
  devmode,
  code,
  verbose = getOption("shiny.devmode.verbose", TRUE)
) {
  withr::with_options(
    list(
      shiny.devmode = devmode,
      shiny.devmode.verbose = verbose
    ),
    code
  )
}


#' @describeIn devmode If Shiny Developer Mode and verbosity are enabled,
#'   displays a message once every 8 hrs (by default)
#' @param message Developer Mode message to be sent to [rlang::inform()]
#' @param .frequency Frequency of the Developer Mode message used with
#'   [rlang::inform()]. Defaults to once every 8 hours.
#' @param .frequency_id [rlang::inform()] message identifier. Defaults to
#'   `message`.
#' @param .file Output connection for [rlang::inform()]. Defaults to [stderr()]
#' @param ... Parameters passed to [rlang::inform()]
devmode_inform <- function(
  message,
  .frequency = "regularly",
  .frequency_id = message,
  .file = stderr(),
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
    message = paste0("shiny devmode - ", message),
    .frequency = .frequency,
    .frequency_id = .frequency_id,
    .file = .file,
    ...
  )
}




registered_devmode_options <- NULL
on_load({
  registered_devmode_options <- Map$new()
})

#' @describeIn devmode Registers a Shiny Developer Mode option with an updated
#'   value and Developer message. This registration method allows package
#'   authors to write one message in a single location.
#'
#'   For example, the following Shiny Developer Mode options are registered:
#'
#' ```r
#' # Reload the Shiny app when a sourced R file changes
#' register_devmode_option(
#'   "shiny.autoreload",
#'   "Turning on shiny autoreload. To disable, call `options(shiny.autoreload = FALSE)`",
#'   devmode_default = TRUE
#' )
#'
#' # Use the unminified Shiny JavaScript file, `shiny.js`
#' register_devmode_option(
#'   "shiny.minified",
#'   "Using full shiny javascript file. To use the minified version, call `options(shiny.minified = TRUE)`",
#'   devmode_default = FALSE
#' )
#'
#' # Display the full stack trace when errors occur during Shiny app execution
#' register_devmode_option(
#'   "shiny.fullstacktrace",
#'   "Turning on full stack trace. To disable, call `options(shiny.fullstacktrace = FALSE)`",
#'   devmode_default = TRUE
#' )
#' ```
#'
#' Other known, non-Shiny Developer Mode options:
#'
#' * Sass:
#' ```r
#' # Display the full stack trace when errors occur during Shiny app execution
#' register_devmode_option(
#'   "sass.cache",
#'   "Turning off sass cache. To use default caching, call `options(sass.cache = TRUE)`",
#'   devmode_default = FALSE
#' )
#' ```
#'
#' @param name Name of option to look for in `options()`
#' @param default Default value to return if `in_devmode()` returns
#'   `TRUE` and the specified option is not set in [`options()`].
#' @param devmode_message Message to display once every 8 hours when utilizing
#'   the `devmode_default` value.  If `devmode_message` is missing, the
#'   registered `devmode_message` value be used.
#' @param devmode_default Default value to return if `in_devmode()` returns
#'   `TRUE` and the specified option is not set in [`options()`]. For
#'   `get_devmode_option()`, if `devmode_default` is missing, the
#'   registered `devmode_default` value will be used.
#' @export
#' @examples
#' # Ex: Within shiny, we register the option "shiny.minified"
#' #   to default to `FALSE` when in Dev Mode
#' \dontrun{register_devmode_option(
#'   "shiny.minified",
#'   devmode_message = paste0(
#'     "Using full shiny javascript file. ",
#'     "To use the minified version, call `options(shiny.minified = TRUE)`"
#'   ),
#'   devmode_default = FALSE
#' )}
#'
register_devmode_option <- function(
  name,
  devmode_message = NULL,
  devmode_default = NULL
) {
  if (!is.null(devmode_message)) {
    stopifnot(length(devmode_message) == 1 && is.character(devmode_message))
  }
  registered_devmode_options$set(
    name,
    list(devmode_default = devmode_default, devmode_message = devmode_message)
  )
}


#' @describeIn devmode Provides a consistent way to change the expected
#'   [getOption()] behavior when Developer Mode is enabled. This method is very
#'   similar to [getOption()] where the globally set option takes precedence.
#'   See section "Avoiding direct dependency on shiny" for
#'   `get_devmode_option()` implementation details.
#'
#'   **Package developers:** Register your Dev Mode option using
#'   `register_devmode_option()` to avoid supplying the same `devmode_default`
#'   and `devmode_message` values throughout your package. (This requires a
#'   \pkg{shiny} dependency.)
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
  name,
  default = NULL,
  devmode_default = missing_arg(),
  devmode_message = missing_arg()
) {
  getOption(
    name,
    local({
      if (!in_devmode()) {
        # typical case
        return(default)
      }

      info <- registered_devmode_options$get(name)
      if (is.null(info)) {
        # Not registered,
        # Warn and return default value
        rlang::warn(
          message = paste0(
            "`get_devmode_option(name)` could not find `name = \"", name, "\"`. ",
            "Returning `default` value"
          )
        )
        return(default)
      }

      # display message
      devmode_inform(
        maybe_missing(
          # use provided `devmode_message` value
          devmode_message,
          # If `devmode_message` is missing, display registered `devmode_message`
          default = info$devmode_message
        )
      )

      # return value
      maybe_missing(
        # use provided `devmode_default` value
        devmode_default,
        # if `devmode_default` is missing, provide registered `devmode_default`
        default = info$devmode_default
      )
    })
  )
}


on_load({
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
    "Turning on full stack trace. To disable, call `options(shiny.fullstacktrace = FALSE)`",
    TRUE
  )
})
