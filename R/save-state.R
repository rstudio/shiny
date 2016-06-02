#' Save or encode state of Shiny session
#'
#' Shiny applications can have their state \emph{encoded} in a URL or
#' \emph{saved}. If the state is encoded, all of the input values are stored in
#' the URL. If the state is saved, the input values and any uploaded files are
#' stored on disk.
#'
#' @param input The session's input object.
#' @param exclude A character vector of input names that should not be
#'   bookmarked.
#' @export
saveStateURL <- function(input, exclude) {
  id <- createUniqueId(8)

  saveInterface <- getShinyOption("save.interface", default = saveInterfaceLocal)

  saveInterface(id, function(stateDir) {
    # Serialize values, possibly saving some extra data to stateDir
    values <- serializeReactiveValues(stateDir, input, exclude)

    stateFile <- file.path(stateDir, "state.rds")
    saveRDS(values, stateFile)
  })

  paste0("_state_id=", encodeURIComponent(id))
}


restoreStateURL <- function(queryString) {
  values <- parseQueryString(queryString, nested = TRUE)
  id <- values[["_state_id"]]

  restoreInterface <- getShinyOption("restore.interface", default = restoreInterfaceLocal)

  res <- NULL

  restoreInterface(id, function(stateDir) {
    stateFile <- file.path(stateDir, "state.rds")

    res <<- list(
      values = readRDS(stateFile),
      dir = stateDir
    )
  })

  res
}

#' @rdname saveStateURL
#' @export
encodeStateURL <- function(input, exclude) {
  vals <- serializeReactiveValues(input, exclude, stateDir = NULL)

  vals <- vapply(vals,
    function(x) toJSON(x, strict_atomic = FALSE),
    character(1),
    USE.NAMES = TRUE
  )

  paste0(
    encodeURIComponent(names(vals)),
    "=",
    encodeURIComponent(vals),
    collapse = "&"
  )
}


#' @export
decodeStateURL <- function(url) {
  values <- parseQueryString(url, nested = TRUE)

  mapply(names(values), values, SIMPLIFY = FALSE,
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

# Restore context. This is basically a key-value store, except for one important
# difference: When the user `get()`s a value, the value is marked as pending;
# when `flushPending()` is called, those pending values are marked as used. When
# a value is marked as used, `get()` will not return it, unless called with
# `force=TRUE`. This is to make sure that a particular value can be restored
# only within a single call to `withRestoreContext()`.
RestoreContext <- R6Class("RestoreContext",
  private = list(
    values = NULL,
    dir = NULL,  # Directory for extra files, if restoring from saved state
    pending = character(0),
    used = character(0)     # Names of values which have been used
  ),

  public = list(
    initialize = function(queryString = NULL) {
      private$values <- new.env(parent = emptyenv())

      if (!is.null(queryString)) {
        values <- parseQueryString(queryString, nested = TRUE)

        # If we have a "_state_id" key, restore from persisted state and ignore
        # other key/value pairs. If not, restore from key/value pairs in the
        # query string.
        if (!is.null(values[["_state_id"]]) && nzchar(values[["_state_id"]])) {

          res <- restoreStateURL(queryString)

          values <- res$values
          private$dir <- res$dir

        } else {
          # The query string contains the saved keys and values
          values <- decodeStateURL(queryString)
        }

        list2env(values, private$values)
      }
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

    getDir = function() {
      private$dir
    }
  )
)


restoreCtxStack <- Stack$new()

# Equivalent to
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

#' @export
restoreInput <- function(id, default) {
  if (!isTRUE(getShinyOption("restorable")) || !hasCurrentRestoreContext())
    return(default)

  ctx <- getCurrentRestoreContext()
  if (ctx$available(id)) {
    ctx$get(id)
  } else {
    default
  }
}

#' @export
restoreValue <- function(id, default) {
  ctx <- getCurrentRestoreContext()
  if (id %in% names(ctx$values)) {
    ctx$values[[id]]
  } else {
    default
  }
}

#' @param id ID for the output.
#' @param label Text label, displayed above the bookmark output.
#' @param updateId An optional input ID. If non-NULL, a special actionButton
#'   with this ID will be added to the input group. The purpose of this button
#'   is that, when clicked, the \code{bookmarkOutput}'s value will be updated.
#'   See examples below.
#' @export
bookmarkOutput <- function(id, label = NULL, updateId = NULL) {
  textId <- paste0("shiny-bookmark-", id)

  tagList(
    if (!is.null(label)) tags$label(label, `for` = textId),

    div(class="input-group shiny-bookmark-output", id = id,
      tags$input(id = textId,
        readonly = "readonly",
        class = "form-control",
        placeholder = "Bookmark URL"
      ),
      span(class = "input-group-btn",
        if (!is.null(updateId)) tags$button(id = updateId,
          class = "btn btn-default action-button",
          icon("repeat", lib = "glyphicon")
        ),
        tags$button(class = "btn btn-default",
          `data-clipboard-target` = paste0("#", textId),
          icon("copy", lib = "glyphicon")
        )
      )
    ),
    htmlDependency(
      "clipboardjs", "1.5.10", c(href = "shared/clipboardjs"),
      script = "clipboard.min.js"
    )
  )
}

#' @export
updateQueryString <- function(queryString, session = getDefaultReactiveDomain()) {
  session$updateQueryString(queryString)
}
