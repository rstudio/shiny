# Create a Map object for input handlers and register the defaults.
# This is assigned in .onLoad time.
inputHandlers <- NULL
on_load({
  inputHandlers <- Map$new()
})

#' Register an Input Handler
#'
#' Adds an input handler for data of this type. When called, Shiny will use the
#' function provided to refine the data passed back from the client (after being
#' deserialized by jsonlite) before making it available in the `input` variable
#' of the `server.R` file.
#'
#' This function will register the handler for the duration of the R process
#' (unless Shiny is explicitly reloaded). For that reason, the `type` used
#' should be very specific to this package to minimize the risk of colliding
#' with another Shiny package which might use this data type name. We recommend
#' the format of "packageName.widgetName". It should be called from the
#' package's `.onLoad()` function.
#'
#' Currently Shiny registers the following handlers: `shiny.matrix`,
#' `shiny.number`, and `shiny.date`.
#'
#' The `type` of a custom Shiny Input widget will be deduced using the
#' `getType()` JavaScript function on the registered Shiny inputBinding.
#' @param type The type for which the handler should be added --- should be a
#'   single-element character vector.
#' @param fun The handler function. This is the function that will be used to
#'   parse the data delivered from the client before it is available in the
#'   `input` variable. The function will be called with the following three
#'   parameters: \enumerate{ \item{The value of this input as provided by the
#'   client, deserialized using jsonlite.} \item{The `shinysession` in which the
#'   input exists.} \item{The name of the input.} }
#' @param force If `TRUE`, will overwrite any existing handler without warning.
#'   If `FALSE`, will throw an error if this class already has a handler
#'   defined.
#' @examples
#' \dontrun{
#' # Register an input handler which rounds a input number to the nearest integer
#' # In a package, this should be called from the .onLoad function.
#' registerInputHandler("mypackage.validint", function(x, shinysession, name) {
#'   if (is.null(x)) return(NA)
#'   round(x)
#' })
#'
#' ## On the Javascript side, the associated input binding must have a corresponding getType method:
#' # getType: function(el) {
#' #   return "mypackage.validint";
#' # }
#'
#' }
#' @seealso [removeInputHandler()] [applyInputHandlers()]
#' @export
registerInputHandler <- function(type, fun, force=FALSE){
  if (inputHandlers$containsKey(type) && !force){
    stop("There is already an input handler for type: ", type)
  }
  inputHandlers$set(type, fun)
}

#' Deregister an Input Handler
#'
#' Removes an Input Handler. Rather than using the previously specified handler
#' for data of this type, the default jsonlite serialization will be used.
#'
#' @param type The type for which handlers should be removed.
#' @return The handler previously associated with this `type`, if one
#'   existed. Otherwise, `NULL`.
#' @seealso [registerInputHandler()]
#' @export
removeInputHandler <- function(type){
  inputHandlers$remove(type)
}


# Apply input handler to a single input value
applyInputHandler <- function(name, val, shinysession) {
  splitName <- strsplit(name, ':')[[1]]
  if (length(splitName) > 1) {
    if (!inputHandlers$containsKey(splitName[[2]])) {
      # No input handler registered for this type
      stop("No handler registered for type ", name)
    }

    inputName <- splitName[[1]]

    # Get the function for processing this type of input
    inputHandler <- inputHandlers$get(splitName[[2]])

    return(inputHandler(val, shinysession, inputName))

  } else if (is.list(val) && is.null(names(val))) {
    return(unlist(val, recursive = TRUE))
  } else {
    return(val)
  }
}

#' Apply input handlers to raw input values
#'
#' The purpose of this function is to make it possible for external packages to
#' test Shiny inputs. It takes a named list of raw input values, applies input
#' handlers to those values, and then returns a named list of the processed
#' values.
#'
#' The raw input values should be in a named list. Some values may have names
#' like `"x:shiny.date"`. This function would apply the `"shiny.date"`
#' input handler to the value, and then rename the result to `"x"`, in the
#' output.
#'
#' @param inputs A named list of input values.
#' @param shinysession A Shiny session object.
#'
#' @seealso registerInputHandler
#' @keywords internal
applyInputHandlers <- function(inputs, shinysession = getDefaultReactiveDomain()) {
  inputs <- mapply(applyInputHandler, names(inputs), inputs,
                   MoreArgs = list(shinysession = shinysession),
                   SIMPLIFY = FALSE)

  # Convert names like "button1:shiny.action" to "button1"
  names(inputs) <- vapply(
    names(inputs),
    function(name) { strsplit(name, ":")[[1]][1] },
    FUN.VALUE = character(1)
  )

  inputs
}

on_load({
  # Takes a list-of-lists and returns a matrix. The lists
  # must all be the same length. NULL is replaced by NA.
  registerInputHandler("shiny.matrix", function(data, ...) {
    if (length(data) == 0)
      return(matrix(nrow=0, ncol=0))

    m <- matrix(unlist(lapply(data, function(x) {
      sapply(x, function(y) {
        ifelse(is.null(y), NA, y)
      })
    })), nrow = length(data[[1]]), ncol = length(data))
    return(m)
  })


  registerInputHandler("shiny.number", function(val, ...){
    ifelse(is.null(val), NA, val)
  })

  registerInputHandler("shiny.password", function(val, shinysession, name) {
    # Mark passwords as not serializable
    setSerializer(name, serializerUnserializable)
    val
  })

  registerInputHandler("shiny.date", function(val, ...){
    # First replace NULLs with NA, then convert to Date vector
    datelist <- ifelse(lapply(val, is.null), NA, val)

    res <- NULL
    tryCatch({
        res <- as.Date(unlist(datelist))
      },
      error = function(e) {
        # It's possible for client to send a string like "99999-01-01", which
        # as.Date can't handle.
        warning(e$message)
        res <<- as.Date(rep(NA, length(datelist)))
      }
    )

    res
  })

  registerInputHandler("shiny.datetime", function(val, ...){
    # First replace NULLs with NA, then convert to POSIXct vector
    times <- lapply(val, function(x) {
      if (is.null(x)) NA
      else x
    })
    as.POSIXct(unlist(times), origin = "1970-01-01", tz = "UTC")
  })

  registerInputHandler("shiny.action", function(val, shinysession, name) {
    # mark up the action button value with a special class so we can recognize it later
    class(val) <- c("shinyActionButtonValue", class(val))
    val
  })

  registerInputHandler("shiny.file", function(val, shinysession, name) {
    # This function is only used when restoring a Shiny fileInput. When a file is
    # uploaded the usual way, it takes a different code path and won't hit this
    # function.
    if (is.null(val))
      return(NULL)

    # The data will be a named list of lists; convert to a data frame.
    val <- as.data.frame(lapply(val, unlist), stringsAsFactors = FALSE)

    # `val$datapath` should be a filename without a path, for security reasons.
    if (basename(val$datapath) != val$datapath) {
      stop("Invalid '/' found in file input path.")
    }

    # Prepend the persistent dir
    oldfile <- file.path(getCurrentRestoreContext()$dir, val$datapath)

    # Copy the original file to a new temp dir, so that a restored session can't
    # modify the original.
    newdir <- file.path(tempdir(), createUniqueId(12))
    dir.create(newdir)
    val$datapath <- file.path(newdir, val$datapath)
    file.copy(oldfile, val$datapath)

    # Need to mark this input value with the correct serializer. When a file is
    # uploaded the usual way (instead of being restored), this occurs in
    # session$`@uploadEnd`.
    setSerializer(name, serializerFileInput)

    snapshotPreprocessInput(name, snapshotPreprocessorFileInput)

    val
  })


  # to be used with !!!answer
  registerInputHandler("shiny.symbolList", function(val, ...) {
    if (is.null(val)) {
      list()
    } else {
      lapply(val, as.symbol)
    }
  })
  # to be used with !!answer
  registerInputHandler("shiny.symbol", function(val, ...) {
    if (is.null(val) || identical(val, "")) {
      NULL
    } else {
      as.symbol(val)
    }
  })

})
