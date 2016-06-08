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
saveStateQueryString <- function(input, exclude = NULL) {
  id <- createUniqueId(8)

  saveInterface <- getShinyOption("save.interface", default = saveInterfaceLocal)

  saveInterface(id, function(stateDir) {
    # Serialize values, possibly saving some extra data to stateDir
    values <- serializeReactiveValues(input, exclude, stateDir)

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

#' @rdname saveStateQueryString
#' @export
encodeStateQueryString <- function(input, exclude = NULL) {
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
updateQueryString <- function(queryString, session = getDefaultReactiveDomain()) {
  session$updateQueryString(queryString)
}


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
#' @param enable If \code{TRUE} (the default), enable bookmarking for this app.
#' @param type Either \code{"save"}, which saves to disk, or \code{"encode"},
#'   which encodes all of the relevant values in a URL.
#' @param exclude Input values to exclude from bookmarking.
#' @param onBookmarked A callback function to invoke after the bookmarking has
#'   been done.
#' @param session A Shiny session object.
#' @export
configureBookmarking <- function(eventExpr, enable = TRUE,
  type = c("save", "encode"), exclude = NULL,
  onBookmarked = NULL, session = getDefaultReactiveDomain())
{

  eventExpr <- substitute(eventExpr)
  type <- match.arg(type)

  # If no onBookmarked function is provided, use one of these defaults.
  if (is.null(onBookmarked)) {
    if (!is.function(onBookmarked))
      stop("onBookmarked must be a function")

    if (type == "save") {
      onBookmarked <- function(url) {
        showModal(urlModal(url, subtitle = "The state of this application has been saved."))
      }
    } else {
      onBookmarked <- function(url) {
        showModal(urlModal(url))
      }
    }
  }

  # If there's an existing onBookmarked observer, destroy it before creating a
  # new one.
  if (!is.null(session$onBookmarkedObserver)) {
    session$onBookmarkedObserver$destroy()
    session$onBookmarkedObserver <- NULL
  }

  if (enable) {
    session$onBookmarkedObserver <- observeEvent(
      eventExpr,
      event.env = parent.frame(),
      event.quoted = TRUE,
      {
        if (type == "save") {
          url <- saveStateQueryString(session$input, exclude)
        } else {
          url <- encodeStateQueryString(session$input, exclude)
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
  }

  invisible()
}
