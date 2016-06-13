#' Persist (save) or encode state of Shiny session
#'
#' Shiny applications can have their state \emph{encoded} in a URL or
#' \emph{persisted}. If the state is encoded, all of the input values are stored in
#' the URL. If the state is persisted, the input values and any uploaded files are
#' stored on disk.
#'
#' @param input The session's input object.
#' @param exclude A character vector of input names that should not be
#'   bookmarked.
#' @param values Any additional values that should be persisted or encoded. This
#'   must be either NULL or a list.
#' @export
persistStateQueryString <- function(input, exclude = NULL, values = NULL) {
  if (!is.null(values) && !is.list(values)) {
    stop("'values' must be either NULL or a list.")
  }

  id <- createUniqueId(8)

  persistInterface <- getShinyOption("persist.interface", default = persistInterfaceLocal)

  persistInterface(id, function(stateDir) {
    # Serialize values, possibly saving some extra data to stateDir
    inputValues <- serializeReactiveValues(input, exclude, stateDir)

    saveRDS(inputValues, file.path(stateDir, "input.rds"))

    # If there values passed in, save them also
    if (!is.null(values))
      saveRDS(values, file.path(stateDir, "values.rds"))

  })

  paste0("_state_id=", encodeURIComponent(id))
}

# Counterpart to persistStateQueryString
loadStateQueryString <- function(queryString) {
  values <- parseQueryString(queryString, nested = TRUE)
  id <- values[["_state_id"]]

  restoreInterface <- getShinyOption("restore.interface", default = restoreInterfaceLocal)

  res <- NULL

  restoreInterface(id, function(stateDir) {
    res <<- list(
      input = readRDS(file.path(stateDir, "input.rds")),
      dir = stateDir
    )

    valuesFile <- file.path(stateDir, "values.rds")
    if (file.exists(valuesFile)) {
      res$values <<- readRDS(valuesFile)
    } else {
      res$values <<- list()
    }
  })

  res
}


#' @rdname persistStateQueryString
#' @export
encodeStateQueryString <- function(input, exclude = NULL, values = NULL) {
  if (!is.null(values) && !is.list(values)) {
    stop("'values' must be either NULL or a list.")
  }

  inputVals <- serializeReactiveValues(input, exclude, stateDir = NULL)

  inputVals <- vapply(inputVals,
    function(x) toJSON(x, strict_atomic = FALSE),
    character(1),
    USE.NAMES = TRUE
  )

  res <- paste0(
    encodeURIComponent(names(inputVals)),
    "=",
    encodeURIComponent(inputVals),
    collapse = "&"
  )

  # If 'values' is present, add them as well.
  if (length(values) != 0) {
    values <- vapply(values,
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

# Counterpart to encodeStateQueryString
decodeStateQueryString <- function(queryString) {
  if (grepl("_values_", queryString)) {
    splitStr <- strsplit(queryString, "_values_", fixed = TRUE)[[1]]
    inputValueStr <- splitStr[1]
    valueStr <- splitStr[2]

  } else {
    inputValueStr <- queryString
    valueStr <- ""
  }

  inputValues <- parseQueryString(inputValueStr, nested = TRUE)
  values <- parseQueryString(valueStr, nested = TRUE)

  inputValues <- mapply(names(inputValues), inputValues, SIMPLIFY = FALSE,
    FUN = function(name, value) {
      tryCatch(
        jsonlite::fromJSON(value),
        error = function(e) {
          stop("Failed to parse URL parameter \"", name, "\"")
        }
      )
    }
  )

  values <- mapply(names(values), values, SIMPLIFY = FALSE,
    FUN = function(name, value) {
      tryCatch(
        jsonlite::fromJSON(value),
        error = function(e) {
          stop("Failed to parse URL parameter \"", name, "\"")
        }
      )
    }
  )

  list(input = inputValues, values = values)
}


RestoreContext <- R6Class("RestoreContext",
  public = list(
    # This is a RestoreInputSet for input values. This is a key-value store with
    # some special handling.
    input = NULL,

    # Directory for extra files, if restoring from persisted state
    dir = NULL,

    # For values other than input values. These values don't need the special
    # phandling that's needed for input values, because they're only accessed
    # from the onRestore function.
    values = list(),

    initialize = function(queryString = NULL) {
      if (!is.null(queryString)) {
        qsValues <- parseQueryString(queryString, nested = TRUE)

        # If we have a "_state_id" key, restore from persisted state and ignore
        # other key/value pairs. If not, restore from key/value pairs in the
        # query string.
        if (!is.null(qsValues[["_state_id"]]) && nzchar(qsValues[["_state_id"]])) {

          allValues <- loadStateQueryString(queryString)
          self$dir <- allValues$dir

        } else {
          # The query string contains the saved keys and values
          allValues <- decodeStateQueryString(queryString)
        }

        self$input <- RestoreInputSet$new(allValues$input)
        self$values <- allValues$values
      }
    },

    # This should be called before a restore context is popped off the stack.
    flushPending = function() {
      self$input$flushPending()
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
#' This restores an input value from the current restore context..
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

  if (identical(getShinyOption("restorable"), FALSE) || !hasCurrentRestoreContext())
    return(default)

  oldInputs <- getCurrentRestoreContext()$input
  if (oldInputs$available(id)) {
    oldInputs$get(id)
  } else {
    default
  }
}

#' Update URL query string in browser's location bar
#'
#' @param queryString The new query string to show in the location bar.
#' @param session A Shiny session object.
#' @export
updateQueryString <- function(queryString, session = getDefaultReactiveDomain()) {
  session$updateQueryString(queryString)
}

#' Create a button for bookmarking/sharing
#'
#' A \code{bookmarkButton} is a \code{\link{actionButton}} with a default label
#' that consists of a link icon and the text "Share...". It is meant to be used
#' for bookmarking state.
#'
#' @seealso configureBookmarking
#' @inheritParams actionButton
#' @export
bookmarkButton <- function(inputId,
  label = tagList(icon("link", lib = "glyphicon"), "Share..."), ...)
{
  actionButton(inputId, label, ...)
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
urlModal <- function(url, title = "Share link", subtitle = NULL) {

  subtitleTag <- NULL
  if (!is.null(subtitle)) {
    subtitleTag <- tagList(
      br(),
      span(class = "text-muted", subtitle)
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
        });"
    )
  )
}


#' Configure bookmarking for the current session
#'
#' There are two types of bookmarking: saving state, and encoding state.
#'
#' @param eventExpr An expression to listen for, similar to
#'   \code{\link{observeEvent}}.
#' @param type Either \code{"encode"}, which encodes all of the relevant values
#'   in a URL, \code{"persist"}, which saves to disk, or \code{"disable"}, which
#'   disables any previously-enabled bookmarking.
#' @param exclude Input values to exclude from bookmarking.
#' @param onBookmark A function to call before saving state. This function
#'   should return a list, which will be saved as \code{values}.
#' @param onRestore A function to call when a session is restored. It will be
#'   passed one argument, a restoreContext object.
#' @param onBookmarked A callback function to invoke after the bookmarking has
#'   been done.
#' @param session A Shiny session object.
#' @export
configureBookmarking <- function(eventExpr,
  type = c("encode", "persist", "disable"), exclude = NULL,
  onBookmark = NULL, onRestore = NULL, onBookmarked = NULL,
  session = getDefaultReactiveDomain())
{

  eventExpr <- substitute(eventExpr)
  type <- match.arg(type)

  # If there's an existing onBookmarked observer, destroy it before creating a
  # new one.
  if (!is.null(session$bookmarkConfig$onBookmarkedObserver)) {
    session$bookmarkConfig$onBookmarkedObserver$destroy()
    session$bookmarkConfig$onBookmarkedObserver <- NULL
  }

  if (type == "disable") {
    return(invisible())
  }

  # If no onBookmarked function is provided, use one of these defaults.
  if (is.null(onBookmarked)) {
    if (type == "persist") {
      onBookmarked <- function(url) {
        showModal(urlModal(url, subtitle = "The state of this application has been persisted."))
      }
    } else if (type == "encode") {
      onBookmarked <- function(url) {
        showModal(urlModal(url))
      }
    }
  } else if (!is.function(onBookmarked)) {
    stop("onBookmarked must be a function.")
  }

  session$bookmarkConfig$onBookmarkedObserver <- observeEvent(
    eventExpr,
    event.env = parent.frame(),
    event.quoted = TRUE,
    {
      values <- NULL
      if (!is.null(onBookmark))
        values <- onBookmark()
      if (!is.null(values) && !is.list(values))
        stop("The value returned by onBookmark() must be NULL or a list.")

      if (type == "persist") {
        url <- persistStateQueryString(session$input, exclude, values)
      } else {
        url <- encodeStateQueryString(session$input, exclude, values)
      }

      clientData <- session$clientData
      url <- paste0(
        clientData$url_protocol, "//",
        clientData$url_hostname,
        if (nzchar(clientData$url_port)) paste0(":", clientData$url_port),
        clientData$url_pathname,
        "?", url
      )

      onBookmarked(url)
    }
  )

  # Run the onRestore function immediately
  if (!is.null(onRestore))
    onRestore(getCurrentRestoreContext())

  invisible()
}
