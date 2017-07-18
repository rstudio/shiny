#' Add a function for serializing an input before bookmarking application state
#'
#' @param inputId Name of the input value.
#' @param fun A function that takes the input value and returns a modified
#'   value. The returned value will be used for the test snapshot.
#' @param session A Shiny session object.
#'
#' @keywords internal
#' @export
setSerializer <- function(inputId, fun, session = getDefaultReactiveDomain()) {
  if (is.null(session)) {
    stop("setSerializer() needs a session object.")
  }

  input_impl <- .subset2(session$input, "impl")
  input_impl$setMeta(inputId, "shiny.serializer", fun)
}


# For most types of values, simply return the value unchanged.
serializerDefault <- function(value, stateDir) {
  value
}


serializerFileInput <- function(value, stateDir = NULL) {
  # File inputs can be serialized only if there's a stateDir
  if (is.null(stateDir)) {
    return(serializerUnserializable())
  }

  # value is a data frame. When persisting files, we need to copy the file to
  # the persistent dir and then strip the original path before saving.
  newpaths <- file.path(stateDir, basename(value$datapath))
  file.copy(value$datapath, newpaths, overwrite = TRUE)
  value$datapath <- basename(newpaths)

  value
}


# Return a sentinel value that represents "unserializable". This is applied to
# for example, passwords and actionButtons.
serializerUnserializable <- function(value, stateDir) {
  structure(
    list(),
    serializable = FALSE
  )
}

# Is this an "unserializable" sentinel value?
isUnserializable <- function(x) {
  identical(
    attr(x, "serializable", exact = TRUE),
    FALSE
  )
}


# Given a reactiveValues object and optional directory for saving state, apply
# serializer function to each of the values, and return a list of the returned
# values. This function passes stateDir to the serializer functions, so if
# stateDir is non-NULL, it can have a side effect of writing values to disk (in
# stateDir).
serializeReactiveValues <- function(values, exclude, stateDir = NULL) {
  impl <- .subset2(values, "impl")

  # Get named list where keys and values are the names of inputs; we'll retrieve
  # actual values later.
  vals <- isolate(impl$names())
  vals <- setdiff(vals, exclude)
  names(vals) <- vals

  # Get values and apply serializer functions
  vals <- lapply(vals, function(name) {
    val <- impl$get(name)

    # Get the serializer function for this input value. If none specified, use
    # the default.
    serializer_fun <- impl$getMeta(name, "shiny.serializer")
    if (is.null(serializer_fun))
      serializer_fun <- serializerDefault

    # Apply serializer function.
    serializer_fun(val, stateDir)
  })

  # Filter out any values that were marked as unserializable.
  vals <- Filter(Negate(isUnserializable), vals)
  vals
}
