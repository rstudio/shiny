#' @export
decodeBookmarkDataURL <- function(url) {
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


#' @param input The session's input object.
#' @param exclude A character vector of input names that should not be
#'   bookmarked.
#' @export
encodeBookmarkDataURL <- function(input, exclude = NULL) {
  vals <- reactiveValuesToList(input)
  vals <- vals[setdiff(names(vals), exclude)]

  vals <- vapply(vals, function(x) {
    toJSON(x, strict_atomic = FALSE)
  }, character(1), USE.NAMES = TRUE)

  paste0(
    encodeURIComponent(names(vals)),
    "=",
    encodeURIComponent(vals),
    collapse = "&"
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
    pending = character(0),
    used = character(0)     # Names of values which have been used
  ),

  public = list(
    initialize = function(queryString = NULL) {
      private$values <- new.env(parent = emptyenv())

      if (!is.null(queryString)) {
        vals <- decodeBookmarkDataURL(queryString)
        list2env(vals, private$values)
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
    }
  )
)


restoreCtxStack <- Stack$new()

withRestoreContext <- function(ctx, expr) {
  restoreCtxStack$push(ctx)

  on.exit({
    # Mark pending names as used
    ctx$flushPending()
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
  if (!hasCurrentRestoreContext())
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

#' @export
bookmarkOutput <- function(id, label = NULL) {
  textId <- paste0("shiny-bookmark-", id)

  tagList(
    if (!is.null(label)) tags$label(label, `for` = textId),

    div(class="input-group shiny-bookmark-output", id = id,
      tags$input(id = textId,
        readonly = "readonly",
        class = "form-control",
        placeholder = "Click button"
      ),
      span(class = "input-group-btn",
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
