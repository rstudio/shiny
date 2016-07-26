#' @include stack.R
NULL

ShinySaveState <- R6Class("ShinySaveState",
  public = list(
    input = NULL,
    exclude = NULL,
    onSave = NULL, # A callback to invoke during the saving process.

    # These are set not in initialize(), but by external functions that modify
    # the ShinySaveState object.
    dir = NULL,

    # An environment for storing arbitrary values. This is an environment
    # (instead of, say, a list) because if the onSave function represents
    # multiple callback functions (when onBookmark is called multiple times),
    # each callback can change `values`, and if we used a list, one of the
    # callbacks could easily obliterate values set by another. This can happen
    # when using modules that have an onBookmark function.
    values = NULL,

    initialize = function(input = NULL, exclude = NULL, onSave = NULL) {
      self$input   <- input
      self$exclude <- exclude
      self$onSave  <- onSave
      self$values  <- new.env(parent = emptyenv())
    }
  )
)


# Save a state to disk. Returns a query string which can be used to restore the
# session.
saveShinySaveState <- function(state) {
  id <- createUniqueId(8)

  # A function for saving the state object to disk, given a directory to save
  # to.
  saveState <- function(stateDir) {
    state$dir <- stateDir

    # Allow user-supplied onSave function to do things like add state$values, or
    # save data to state dir.
    if (!is.null(state$onSave))
      isolate(state$onSave(state))

    # Serialize values, possibly saving some extra data to stateDir
    inputValues <- serializeReactiveValues(state$input, state$exclude, state$dir)
    saveRDS(inputValues, file.path(stateDir, "input.rds"))

    # If values were added, save them also.
    values <- as.list.environment(state$values)
    if (length(values) != 0)
      saveRDS(values, file.path(stateDir, "values.rds"))
  }

  # Pass the saveState function to the save interface function, which will
  # invoke saveState after preparing the directory.
  saveInterface <- getShinyOption("save.interface", default = saveInterfaceLocal)
  saveInterface(id, saveState)

  paste0("_state_id_=", encodeURIComponent(id))
}

# Encode the state to a URL. This does not save to disk.
encodeShinySaveState <- function(state) {
  inputVals <- serializeReactiveValues(state$input, state$exclude, stateDir = NULL)

  # Allow user-supplied onSave function to do things like add state$values.
  if (!is.null(state$onSave))
    isolate(state$onSave(state))

  inputVals <- vapply(inputVals,
    function(x) toJSON(x, strict_atomic = FALSE),
    character(1),
    USE.NAMES = TRUE
  )

  res <- paste0("_inputs_&",
    paste0(
      encodeURIComponent(names(inputVals)),
      "=",
      encodeURIComponent(inputVals),
      collapse = "&"
    )
  )

  # If 'values' is present, add them as well.
  if (length(state$values) != 0) {
    values <- vapply(state$values,
      function(x) toJSON(x, strict_atomic = FALSE),
      character(1),
      USE.NAMES = TRUE
    )

    res <- paste0(res, "&_values_&",
      paste0(
        encodeURIComponent(names(values)),
        "=",
        encodeURIComponent(values),
        collapse = "&"
      )
    )
  }

  res
}

RestoreContext <- R6Class("RestoreContext",
  public = list(
    # This will be set to TRUE if there's actually a state to restore
    active = FALSE,

    # This is set to an error message string in case there was an initialization
    # error. Later, after the app has started on the client, the server can send
    # this message as a notification on the client.
    initErrorMessage = NULL,

    # This is a RestoreInputSet for input values. This is a key-value store with
    # some special handling.
    input = NULL,

    # Directory for extra files, if restoring from state that was saved to disk.
    dir = NULL,

    # For values other than input values. These values don't need the special
    # phandling that's needed for input values, because they're only accessed
    # from the onRestore function.
    values = NULL,

    initialize = function(queryString = NULL) {
      self$reset() # Need this to initialize self$input

      if (!is.null(queryString) && nzchar(queryString)) {
        tryCatch(
          withLogErrors({
            qsValues <- parseQueryString(queryString, nested = TRUE)

            if (!is.null(qsValues[["__subapp__"]]) && qsValues[["__subapp__"]] == 1) {
              # Ignore subapps in shiny docs
              self$reset()

            } else if (!is.null(qsValues[["_state_id_"]]) && nzchar(qsValues[["_state_id_"]])) {
              # If we have a "_state_id_" key, restore from saved state and
              # ignore other key/value pairs. If not, restore from key/value
              # pairs in the query string.
              self$active <- TRUE
              private$loadStateQueryString(queryString)

            } else {
              # The query string contains the saved keys and values
              self$active <- TRUE
              private$decodeStateQueryString(queryString)
            }
          }),
          error = function(e) {
            # If there's an error in restoring problem, just reset these values
            self$reset()
            self$initErrorMessage <- e$message
            warning(e$message)
          }
        )
      }
    },

    reset = function() {
      self$active <- FALSE
      self$initErrorMessage <- NULL
      self$input <- RestoreInputSet$new(list())
      self$values <- list()
      self$dir <- NULL
    },

    # This should be called before a restore context is popped off the stack.
    flushPending = function() {
      self$input$flushPending()
    },


    # Returns a list representation of the RestoreContext object. This is passed
    # to the app author's onRestore function. An important difference between
    # the RestoreContext object and the list is that the former's `input` field
    # is a RestoreInputSet object, while the latter's `input` field is just a
    # list.
    asList = function() {
      list(
        input = self$input$asList(),
        dir = self$dir,
        values = self$values
      )
    }
  ),

  private = list(
    # Given a query string with a _state_id_, load saved state with that ID.
    loadStateQueryString = function(queryString) {
      values <- parseQueryString(queryString, nested = TRUE)
      id <- values[["_state_id_"]]

      # This function is passed to the loadInterface function; given a
      # directory, it will load state from that directory
      loadFun <- function(stateDir) {
        self$dir <- stateDir

        inputValues <- readRDS(file.path(stateDir, "input.rds"))
        self$input <- RestoreInputSet$new(inputValues)

        valuesFile <- file.path(stateDir, "values.rds")
        if (file.exists(valuesFile)) {
          self$values <- readRDS(valuesFile)
        } else {
          self$values <- list()
        }
      }

      loadInterface <- getShinyOption("load.interface", default = loadInterfaceLocal)
      loadInterface(id, loadFun)

      invisible()
    },

    # Given a query string with values encoded in it, restore saved state
    # from those values.
    decodeStateQueryString = function(queryString) {
      # Remove leading '?'
      if (substr(queryString, 1, 1) == '?')
        queryString <- substr(queryString, 2, nchar(queryString))


      # Error if multiple '_inputs_' or '_values_'. This is needed because
      # strsplit won't add an entry if the search pattern is at the end of a
      # string.
      if (length(gregexpr("(^|&)_inputs_(&|$)", queryString)[[1]]) > 1)
        stop("Invalid state string: more than one '_inputs_' found")
      if (length(gregexpr("(^|&)_values_(&|$)", queryString)[[1]]) > 1)
        stop("Invalid state string: more than one '_values_' found")

      # Look for _inputs_ and store following content in inputStr
      splitStr <- strsplit(queryString, "(^|&)_inputs_(&|$)")[[1]]
      if (length(splitStr) == 2) {
        inputStr <- splitStr[2]
        # Remove any _values_ (and content after _values_) that may come after
        # _inputs_
        inputStr <- strsplit(inputStr, "(^|&)_values_(&|$)")[[1]][1]

      } else {
        inputStr <- ""
      }

      # Look for _values_ and store following content in valueStr
      splitStr <- strsplit(queryString, "(^|&)_values_(&|$)")[[1]]
      if (length(splitStr) == 2) {
        valueStr <- splitStr[2]
        # Remove any _inputs_ (and content after _inputs_) that may come after
        # _values_
        valueStr <- strsplit(valueStr, "(^|&)_inputs_(&|$)")[[1]][1]

      } else {
        valueStr <- ""
      }


      inputs <- parseQueryString(inputStr, nested = TRUE)
      values <- parseQueryString(valueStr, nested = TRUE)

      valuesFromJSON <- function(vals) {
        mapply(names(vals), vals, SIMPLIFY = FALSE,
          FUN = function(name, value) {
            tryCatch(
              jsonlite::fromJSON(value),
              error = function(e) {
                stop("Failed to parse URL parameter \"", name, "\"")
              }
            )
          }
        )
      }

      inputs <- valuesFromJSON(inputs)
      self$input <- RestoreInputSet$new(inputs)

      self$values <- valuesFromJSON(values)
    }
  )
)


# Restore input set. This is basically a key-value store, except for one
# important difference: When the user `get()`s a value, the value is marked as
# pending; when `flushPending()` is called, those pending values are marked as
# used. When a value is marked as used, `get()` will not return it, unless
# called with `force=TRUE`. This is to make sure that a particular value can be
# restored only within a single call to `withRestoreContext()`. Without this, if
# a value is restored in a dynamic UI, it could completely prevent any other
# (non- restored) kvalue from being used.
RestoreInputSet <- R6Class("RestoreInputSet",
  private = list(
    values = NULL,
    pending = character(0),
    used = character(0)     # Names of values which have been used
  ),

  public = list(
    initialize = function(values) {
      private$values <- new.env(parent = emptyenv())
      list2env(values, private$values)
    },

    exists = function(name) {
      exists(name, envir = private$values)
    },

    # Return TRUE if the value exists and has not been marked as used.
    available = function(name) {
      self$exists(name) && !self$isUsed(name)
    },

    isPending = function(name) {
      name %in% private$pending
    },

    isUsed = function(name) {
      name %in% private$used
    },

    # Get a value. If `force` is TRUE, get the value without checking whether
    # has been used, and without marking it as pending.
    get = function(name, force = FALSE) {
      if (force)
        return(private$values[[name]])

      if (!self$available(name))
        return(NULL)

      # Mark this name as pending. Use unique so that it's not added twice.
      private$pending <- unique(c(private$pending, name))
      private$values[[name]]
    },

    # Take pending names and mark them as used, then clear pending list.
    flushPending = function() {
      private$used <- unique(c(private$used, private$pending))
      private$pending <- character(0)
    },

    asList = function() {
      as.list.environment(private$values)
    }
  )
)


restoreCtxStack <- Stack$new()

withRestoreContext <- function(ctx, expr) {
  restoreCtxStack$push(ctx)

  on.exit({
    # Mark pending names as used
    restoreCtxStack$peek()$flushPending()
    restoreCtxStack$pop()
  }, add = TRUE)

  force(expr)
}

# Is there a current restore context?
hasCurrentRestoreContext <- function() {
  restoreCtxStack$size() > 0
}

# Call to access the current restore context
getCurrentRestoreContext <- function() {
  ctx <- restoreCtxStack$peek()
  if (is.null(ctx)) {
    stop("No restore context found")
  }
  ctx
}

#' Restore an input value
#'
#' This restores an input value from the current restore context. It should be
#' called early on inside of input functions (like \code{\link{textInput}}).
#'
#' @param id Name of the input value to restore.
#' @param default A default value to use, if there's no value to restore.
#'
#' @export
restoreInput <- function(id, default) {
  # Need to evaluate `default` in case it contains reactives like input$x. If we
  # don't, then the calling code won't take a reactive dependency on input$x
  # when restoring a value.
  force(default)

  if (!hasCurrentRestoreContext()) {
    return(default)
  }

  oldInputs <- getCurrentRestoreContext()$input
  if (oldInputs$available(id)) {
    oldInputs$get(id)
  } else {
    default
  }
}

#' Update URL in browser's location bar
#'
#' This function updates the URL in the client browser's location bar. It
#' typically is called from an observer.
#'
#' @param queryString The new query string to show in the location bar.
#' @param session A Shiny session object.
#' @export
updateLocationBar <- function(queryString, session = getDefaultReactiveDomain()) {
  session$updateLocationBar(queryString)
}

#' Create a button for bookmarking/sharing
#'
#' A \code{bookmarkButton} is a \code{\link{actionButton}} with a default label
#' that consists of a link icon and the text "Share...". It is meant to be used
#' for bookmarking state.
#'
#' @param title A tooltip that is shown when the mouse cursor hovers over the
#'   button.
#'
#' @seealso configureBookmarking
#' @inheritParams actionButton
#' @export
bookmarkButton <- function(label = "Bookmark...",
  icon = shiny::icon("link", lib = "glyphicon"),
  title = "Bookmark this application's state and get a URL for sharing.",
  ...)
{
  actionButton("__bookmark__", label, icon, title = title, ...)
}


#' Generate a modal dialog that displays a URL
#'
#' The modal dialog generated by \code{urlModal} will display the URL in a
#' textarea input, and the URL text will be selected so that it can be easily
#' copied. The result from \code{urlModal} should be passed to the
#' \code{\link{showModal}} function to display it in the browser.
#'
#' @param url A URL to display in the dialog box.
#' @param title A title for the dialog box.
#' @param subtitle Text to display underneath URL.
#' @export
urlModal <- function(url, title = "Bookmarked application link", subtitle = NULL) {

  subtitleTag <- NULL
  if (!is.null(subtitle)) {
    subtitleTag <- tagList(
      br(),
      span(class = "text-muted", subtitle),
      span(id = "shiny-bookmark-copy-text", class = "text-muted")
    )
  }

  modalDialog(
    title = title,
    easyClose = TRUE,
    footer = NULL,
    tags$textarea(class = "form-control", rows = "1", style = "resize: none;",
      readonly = "readonly",
      url
    ),
    subtitleTag,
    # Need separate show and shown listeners. The show listener sizes the
    # textarea just as the modal starts to fade in. The 200ms delay is needed
    # because if we try to resize earlier, it can't calculate the text height
    # (scrollHeight will be reported as zero). The shown listener selects the
    # text; it's needed because because selection has to be done after the fade-
    # in is completed.
    tags$script(
      "$('#shiny-modal').
        one('show.bs.modal', function() {
          setTimeout(function() {
            var $textarea = $('#shiny-modal textarea');
            $textarea.innerHeight($textarea[0].scrollHeight);
          }, 200);
        });
      $('#shiny-modal')
        .one('shown.bs.modal', function() {
          $('#shiny-modal textarea').select().focus();
        });
      $('#shiny-bookmark-copy-text')
        .text(function() {
          if (/Mac/i.test(navigator.userAgent)) {
            return 'Press \u2318-C to copy.';
          } else {
            return 'Press Ctrl-C to copy.';
          }
        });
      "
    )
  )
}

#' Enable bookmarking for a Shiny application
#'
#' @param store Either \code{"url"}, which encodes all of the relevant values in
#'   a URL, \code{"server"}, which saves to disk on the server, or
#'   \code{"disable"}, which disables any previously-enabled bookmarking.
#' @export
enableBookmarking <- function(store = c("url", "server", "disable")) {
  store <- match.arg(store)
  shinyOptions(bookmarkStore = store)
}


#' Exclude inputs from bookmarking
#'
#' @param names A character vector containing names of inputs to exclude from
#'   bookmarking.
#' @param session A shiny session object.
#' @export
setBookmarkExclude <- function(names = character(0), session = getDefaultReactiveDomain()) {
  session$setBookmarkExclude(names)
}


#' Add callbacks for Shiny session bookmarkingevents
#'
#' These functions are for registering callbacks on Shiny session events.
#' \code{onBookmark} registers a function that will be called before Shiny flushes
#' the reactive system. \code{onFlushed} registers a function that will be
#' called after Shiny flushes the reactive system. \code{onSessionEnded}
#' registers a function to be called after the client has disconnected.
#'
#' These functions should be called within the application's server function.
#'
#' All of these functions return a function which can be called with no
#' arguments to cancel the registration.
#'
#' @param fun A callback function.
#' @param session A shiny session object.
#'
#' @export
onBookmark <- function(fun, session = getDefaultReactiveDomain()) {
  session$onBookmark(fun)
}

#' @rdname onBookmark
#' @export
onBookmarked <- function(fun, session = getDefaultReactiveDomain()) {
  session$onBookmarked(fun)
}

#' @rdname onBookmark
#' @export
onRestore <- function(fun, session = getDefaultReactiveDomain()) {
  session$onRestore(fun)
}

#' @rdname onBookmark
#' @export
onRestored <- function(fun, session = getDefaultReactiveDomain()) {
  session$onRestored(fun)
}


# Get shiny options related to bookmarking and put them in a list, reset those
# shiny options, and then return the options list. This should be during the
# creation of a shiny app object, which happens before another option frame is
# added to the options stack (the new option frame is added when the app is
# run). This function "consumes" the options when the shinyApp object is
# created, so the options won't affect another app that is created later.
consumeBookmarkOptions <- function() {
  # Get options from configureBookmarking
  options <- list(
    bookmarkStore = getShinyOption("bookmarkStore")
  )

  shinyOptions(bookmarkStore = NULL)

  options
}
