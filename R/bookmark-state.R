ShinySaveState <- R6Class("ShinySaveState",
  public = list(
    input = NULL,
    exclude = NULL,
    onSave = NULL, # A callback to invoke during the saving process.

    # These are set not in initialize(), but by external functions that modify
    # the ShinySaveState object.
    dir = NULL,


    initialize = function(input = NULL, exclude = NULL, onSave = NULL) {
      self$input   <- input
      self$exclude <- exclude
      self$onSave  <- onSave
      private$values_  <- new.env(parent = emptyenv())
    }
  ),

  active = list(
    # `values` looks to the outside world like an environment for storing
    # arbitrary values. Two things to note: (1) This is an environment (instead
    # of, say, a list) because if the onSave function represents multiple
    # callback functions (when onBookmark is called multiple times), each
    # callback can change `values`, and if we used a list, one of the callbacks
    # could easily obliterate values set by another. This can happen when using
    # modules that have an onBookmark function. (2) The purpose of the active
    # binding is to prevent replacing state$values with another arbitrary
    # object. (Simply locking the binding would prevent all changes to
    # state$values.)
    values = function(value) {
      if (missing(value))
        return(private$values_)

      if (identical(value, private$values_)) {
        return(value)
      } else {
        stop("Items in `values` can be changed, but `values` itself cannot be replaced.")
      }
    }
  ),

  private = list(
    values_ = NULL
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
    exclude <- c(state$exclude, "._bookmark_")
    inputValues <- serializeReactiveValues(state$input, exclude, state$dir)
    saveRDS(inputValues, file.path(stateDir, "input.rds"))

    # If values were added, save them also.
    if (length(state$values) != 0)
      saveRDS(state$values, file.path(stateDir, "values.rds"))
  }

  # Pass the saveState function to the save interface function, which will
  # invoke saveState after preparing the directory.

  # Look for a save.interface function. This will be defined by the hosting
  # environment if it supports bookmarking.
  saveInterface <- getShinyOption("save.interface", default = NULL)

  if (is.null(saveInterface)) {
    if (inShinyServer()) {
      # We're in a version of Shiny Server/Connect that doesn't have
      # bookmarking support.
      saveInterface <- function(id, callback) {
        stop("The hosting environment does not support saved-to-server bookmarking.")
      }

    } else {
      # We're running Shiny locally.
      saveInterface <- saveInterfaceLocal
    }
  }

  saveInterface(id, saveState)

  paste0("_state_id_=", encodeURIComponent(id))
}

# Encode the state to a URL. This does not save to disk.
encodeShinySaveState <- function(state) {
  # Allow user-supplied onSave function to do things like add state$values.
  if (!is.null(state$onSave))
    isolate(state$onSave(state))

  exclude <- c(state$exclude, "._bookmark_")
  inputVals <- serializeReactiveValues(state$input, exclude, stateDir = NULL)

  inputVals <- vapply(inputVals,
    function(x) toJSON(x, strict_atomic = FALSE),
    character(1),
    USE.NAMES = TRUE
  )

  res <- ""

  # If any input values are present, add them.
  if (length(inputVals) != 0) {
    res <- paste0(res, "_inputs_&",
      paste0(
        encodeURIComponent(names(inputVals)),
        "=",
        encodeURIComponent(inputVals),
        collapse = "&"
      )
    )
  }

  # If 'values' is present, add them as well.
  if (length(state$values) != 0) {
    values <- vapply(state$values,
      function(x) toJSON(x, strict_atomic = FALSE),
      character(1),
      USE.NAMES = TRUE
    )

    res <- paste0(res,
      if (length(inputVals != 0)) "&",  # Add separator if there were inputs
      "_values_&",
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
      self$values <- new.env(parent = emptyenv())
      self$dir <- NULL
    },

    # Completely replace the state
    set = function(active = FALSE, initErrorMessage = NULL, input = list(), values = list(), dir = NULL) {
      # Validate all inputs
      stopifnot(is.logical(active))
      stopifnot(is.null(initErrorMessage) || is.character(initErrorMessage))
      stopifnot(is.list(input))
      stopifnot(is.list(values))
      stopifnot(is.null(dir) || is.character(dir))

      self$active <- active
      self$initErrorMessage <- initErrorMessage
      self$input <- RestoreInputSet$new(input)
      self$values <- list2env2(values, parent = emptyenv())
      self$dir <- dir
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

      # Check that id has only alphanumeric chars
      if (grepl("[^a-zA-Z0-9]", id)) {
        stop("Invalid state id: ", id)
      }

      # This function is passed to the loadInterface function; given a
      # directory, it will load state from that directory
      loadFun <- function(stateDir) {
        self$dir <- stateDir

        if (!dirExists(stateDir)) {
          stop("Bookmarked state directory does not exist.")
        }

        tryCatch({
            inputValues <- readRDS(file.path(stateDir, "input.rds"))
            self$input <- RestoreInputSet$new(inputValues)
          },
          error = function(e) {
            stop("Error reading input values file.")
          }
        )

        valuesFile <- file.path(stateDir, "values.rds")
        if (file.exists(valuesFile)) {
          tryCatch({
              self$values <- readRDS(valuesFile)
            },
            error = function(e) {
              stop("Error reading values file.")
            }
          )
        }
      }

      # Look for a load.interface function. This will be defined by the hosting
      # environment if it supports bookmarking.
      loadInterface <- getShinyOption("load.interface", default = NULL)

      if (is.null(loadInterface)) {
        if (inShinyServer()) {
          # We're in a version of Shiny Server/Connect that doesn't have
          # bookmarking support.
          loadInterface <- function(id, callback) {
            stop("The hosting environment does not support saved-to-server bookmarking.")
          }

        } else {
          # We're running Shiny locally.
          loadInterface <- loadInterfaceLocal
        }
      }

      loadInterface(id, loadFun)

      invisible()
    },

    # Given a query string with values encoded in it, restore saved state
    # from those values.
    decodeStateQueryString = function(queryString) {
      # Remove leading '?'
      if (substr(queryString, 1, 1) == '?')
        queryString <- substr(queryString, 2, nchar(queryString))

      # The "=" after "_inputs_" is optional. Shiny doesn't generate URLs with
      # "=", but httr always adds "=".
      inputs_reg <- "(^|&)_inputs_=?(&|$)"
      values_reg <- "(^|&)_values_=?(&|$)"

      # Error if multiple '_inputs_' or '_values_'. This is needed because
      # strsplit won't add an entry if the search pattern is at the end of a
      # string.
      if (length(gregexpr(inputs_reg, queryString)[[1]]) > 1)
        stop("Invalid state string: more than one '_inputs_' found")
      if (length(gregexpr(values_reg, queryString)[[1]]) > 1)
        stop("Invalid state string: more than one '_values_' found")

      # Look for _inputs_ and store following content in inputStr
      splitStr <- strsplit(queryString, inputs_reg)[[1]]
      if (length(splitStr) == 2) {
        inputStr <- splitStr[2]
        # Remove any _values_ (and content after _values_) that may come after
        # _inputs_
        inputStr <- strsplit(inputStr, values_reg)[[1]][1]

      } else {
        inputStr <- ""
      }

      # Look for _values_ and store following content in valueStr
      splitStr <- strsplit(queryString, values_reg)[[1]]
      if (length(splitStr) == 2) {
        valueStr <- splitStr[2]
        # Remove any _inputs_ (and content after _inputs_) that may come after
        # _values_
        valueStr <- strsplit(valueStr, inputs_reg)[[1]][1]

      } else {
        valueStr <- ""
      }


      inputs <- parseQueryString(inputStr, nested = TRUE)
      values <- parseQueryString(valueStr, nested = TRUE)

      valuesFromJSON <- function(vals) {
        varsUnparsed <- c()
        valsParsed <- mapply(names(vals), vals, SIMPLIFY = FALSE,
          FUN = function(name, value) {
            tryCatch(
              safeFromJSON(value),
              error = function(e) {
                varsUnparsed <<- c(varsUnparsed, name)
                warning("Failed to parse URL parameter \"", name, "\"")
              }
            )
          }
        )
        valsParsed[varsUnparsed] <- NULL
        valsParsed
      }

      inputs <- valuesFromJSON(inputs)
      self$input <- RestoreInputSet$new(inputs)

      values <- valuesFromJSON(values)
      self$values <- list2env2(values, self$values)
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
      private$values <- list2env2(values, parent = emptyenv())
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
      as.list.environment(private$values, all.names = TRUE)
    }
  )
)

restoreCtxStack <- NULL
on_load({
    restoreCtxStack <- fastmap::faststack()
})

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
  if (restoreCtxStack$size() > 0)
    return(TRUE)
  domain <- getDefaultReactiveDomain()
  if (!is.null(domain) && !is.null(domain$restoreContext))
    return(TRUE)

  return(FALSE)
}

# Call to access the current restore context. First look on the restore
# context stack, and if not found, then see if there's one on the current
# reactive domain. In practice, the only time there will be a restore context
# on the stack is when executing the UI function; when executing server code,
# the restore context will be attached to the domain/session.
getCurrentRestoreContext <- function() {
  ctx <- restoreCtxStack$peek()
  if (is.null(ctx)) {
    domain <- getDefaultReactiveDomain()

    if (is.null(domain) || is.null(domain$restoreContext)) {
      stop("No restore context found")
    }

    ctx <- domain$restoreContext
  }
  ctx
}

#' Restore an input value
#'
#' This restores an input value from the current restore context. It should be
#' called early on inside of input functions (like [textInput()]).
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
#' This function updates the client browser's query string in the location bar.
#' It typically is called from an observer. Note that this will not work in
#' Internet Explorer 9 and below.
#'
#' For `mode = "push"`, only three updates are currently allowed:
#' \enumerate{
#'   \item the query string (format: `?param1=val1&param2=val2`)
#'   \item the hash (format: `#hash`)
#'   \item both the query string and the hash
#'     (format: `?param1=val1&param2=val2#hash`)
#' }
#'
#' In other words, if `mode = "push"`, the `queryString` must start
#' with either `?` or with `#`.
#'
#' A technical curiosity: under the hood, this function is calling the HTML5
#' history API (which is where the names for the `mode` argument come from).
#' When `mode = "replace"`, the function called is
#' `window.history.replaceState(null, null, queryString)`.
#' When `mode = "push"`, the function called is
#' `window.history.pushState(null, null, queryString)`.
#'
#' @param queryString The new query string to show in the location bar.
#' @param mode When the query string is updated, should the current history
#'   entry be replaced (default), or should a new history entry be pushed onto
#'   the history stack? The former should only be used in a live bookmarking
#'   context. The latter is useful if you want to navigate between states using
#'   the browser's back and forward buttons. See Examples.
#' @param session A Shiny session object.
#' @seealso [enableBookmarking()], [getQueryString()]
#' @examples
#' ## Only run these examples in interactive sessions
#' if (interactive()) {
#'
#'   ## App 1: Doing "live" bookmarking
#'   ## Update the browser's location bar every time an input changes.
#'   ## This should not be used with enableBookmarking("server"),
#'   ## because that would create a new saved state on disk every time
#'   ## the user changes an input.
#'   enableBookmarking("url")
#'   shinyApp(
#'     ui = function(req) {
#'       fluidPage(
#'         textInput("txt", "Text"),
#'         checkboxInput("chk", "Checkbox")
#'       )
#'     },
#'     server = function(input, output, session) {
#'       observe({
#'         # Trigger this observer every time an input changes
#'         reactiveValuesToList(input)
#'         session$doBookmark()
#'       })
#'       onBookmarked(function(url) {
#'         updateQueryString(url)
#'       })
#'     }
#'   )
#'
#'   ## App 2: Printing the value of the query string
#'   ## (Use the back and forward buttons to see how the browser
#'   ## keeps a record of each state)
#'   shinyApp(
#'     ui = fluidPage(
#'       textInput("txt", "Enter new query string"),
#'       helpText("Format: ?param1=val1&param2=val2"),
#'       actionButton("go", "Update"),
#'       hr(),
#'       verbatimTextOutput("query")
#'     ),
#'     server = function(input, output, session) {
#'       observeEvent(input$go, {
#'         updateQueryString(input$txt, mode = "push")
#'       })
#'       output$query <- renderText({
#'         query <- getQueryString()
#'         queryText <- paste(names(query), query,
#'                        sep = "=", collapse=", ")
#'         paste("Your query string is:\n", queryText)
#'       })
#'     }
#'   )
#' }
#' @export
updateQueryString <- function(queryString, mode = c("replace", "push"),
                              session = getDefaultReactiveDomain()) {
  mode <- match.arg(mode)
  session$updateQueryString(queryString, mode)
}

#' Create a button for bookmarking/sharing
#'
#' A `bookmarkButton` is a [actionButton()] with a default label
#' that consists of a link icon and the text "Bookmark...". It is meant to be
#' used for bookmarking state.
#'
#' @inheritParams actionButton
#' @param title A tooltip that is shown when the mouse cursor hovers over the
#'   button.
#' @param id An ID for the bookmark button. The only time it is necessary to set
#'   the ID unless you have more than one bookmark button in your application.
#'   If you specify an input ID, it should be excluded from bookmarking with
#'   [setBookmarkExclude()], and you must create an observer that
#'   does the bookmarking when the button is pressed. See the examples below.
#'
#' @seealso [enableBookmarking()] for more examples.
#'
#' @examples
#' ## Only run these examples in interactive sessions
#' if (interactive()) {
#'
#' # This example shows how to use multiple bookmark buttons. If you only need
#' # a single bookmark button, see examples in ?enableBookmarking.
#' ui <- function(request) {
#'   fluidPage(
#'     tabsetPanel(id = "tabs",
#'       tabPanel("One",
#'         checkboxInput("chk1", "Checkbox 1"),
#'         bookmarkButton(id = "bookmark1")
#'       ),
#'       tabPanel("Two",
#'         checkboxInput("chk2", "Checkbox 2"),
#'         bookmarkButton(id = "bookmark2")
#'       )
#'     )
#'   )
#' }
#' server <- function(input, output, session) {
#'   # Need to exclude the buttons from themselves being bookmarked
#'   setBookmarkExclude(c("bookmark1", "bookmark2"))
#'
#'   # Trigger bookmarking with either button
#'   observeEvent(input$bookmark1, {
#'     session$doBookmark()
#'   })
#'   observeEvent(input$bookmark2, {
#'     session$doBookmark()
#'   })
#' }
#' enableBookmarking(store = "url")
#' shinyApp(ui, server)
#' }
#' @export
bookmarkButton <- function(label = "Bookmark...",
  icon = shiny::icon("link", lib = "glyphicon"),
  title = "Bookmark this application's state and get a URL for sharing.",
  ...,
  id = "._bookmark_")
{
  actionButton(id, label, icon, title = title, ...)
}


#' Generate a modal dialog that displays a URL
#'
#' The modal dialog generated by `urlModal` will display the URL in a
#' textarea input, and the URL text will be selected so that it can be easily
#' copied. The result from `urlModal` should be passed to the
#' [showModal()] function to display it in the browser.
#'
#' @param url A URL to display in the dialog box.
#' @param title A title for the dialog box.
#' @param subtitle Text to display underneath URL.
#' @export
urlModal <- function(url, title = "Bookmarked application link", subtitle = NULL) {

  subtitleTag <- tagList(
    br(),
    span(class = "text-muted", subtitle),
    span(id = "shiny-bookmark-copy-text", class = "text-muted")
  )

  modalDialog(
    title = title,
    easyClose = TRUE,
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


#' Display a modal dialog for bookmarking
#'
#' This is a wrapper function for [urlModal()] that is automatically
#' called if an application is bookmarked but no other [onBookmark()]
#' callback was set. It displays a modal dialog with the bookmark URL, along
#' with a subtitle that is appropriate for the type of bookmarking used ("url"
#' or "server").
#'
#' @param url A URL to show in the modal dialog.
#' @export
showBookmarkUrlModal <- function(url) {
  store <- getShinyOption("bookmarkStore", default = "")
  if (store == "url") {
    subtitle <- "This link stores the current state of this application."
  } else if (store == "server") {
    subtitle <- "The current state of this application has been stored on the server."
  } else {
    subtitle <- NULL
  }

  showModal(urlModal(url, subtitle = subtitle))
}

#' Enable bookmarking for a Shiny application
#'
#' @description
#'
#' There are two types of bookmarking: saving an application's state to disk on
#' the server, and encoding the application's state in a URL. For state that has
#' been saved to disk, the state can be restored with the corresponding state
#' ID. For URL-encoded state, the state of the application is encoded in the
#' URL, and no server-side storage is needed.
#'
#' URL-encoded bookmarking is appropriate for applications where there not many
#' input values that need to be recorded. Some browsers have a length limit for
#' URLs of about 2000 characters, and if there are many inputs, the length of
#' the URL can exceed that limit.
#'
#' Saved-on-server bookmarking is appropriate when there are many inputs, or
#' when the bookmarked state requires storing files.
#'
#' @details
#'
#' For restoring state to work properly, the UI must be a function that takes
#' one argument, `request`. In most Shiny applications, the UI is not a
#' function; it might have the form `fluidPage(....)`. Converting it to a
#' function is as simple as wrapping it in a function, as in
#' \code{function(request) \{ fluidPage(....) \}}.
#'
#' By default, all input values will be bookmarked, except for the values of
#' passwordInputs. fileInputs will be saved if the state is saved on a server,
#' but not if the state is encoded in a URL.
#'
#' When bookmarking state, arbitrary values can be stored, by passing a function
#' as the `onBookmark` argument. That function will be passed a
#' `ShinySaveState` object. The `values` field of the object is a list
#' which can be manipulated to save extra information. Additionally, if the
#' state is being saved on the server, and the `dir` field of that object
#' can be used to save extra information to files in that directory.
#'
#' For saved-to-server state, this is how the state directory is chosen:
#' \itemize{
#'   \item If running in a hosting environment such as Shiny Server or
#'     Connect, the hosting environment will choose the directory.
#'   \item If running an app in a directory with [runApp()], the
#'     saved states will be saved in a subdirectory of the app called
#'    shiny_bookmarks.
#'   \item If running a Shiny app object that is generated from code (not run
#'     from a directory), the saved states will be saved in a subdirectory of
#'     the current working directory called shiny_bookmarks.
#' }
#'
#' When used with [shinyApp()], this function must be called before
#' `shinyApp()`, or in the `shinyApp()`'s `onStart` function. An
#' alternative to calling the `enableBookmarking()` function is to use the
#' `enableBookmarking` *argument* for `shinyApp()`. See examples
#' below.
#'
#' @param store Either `"url"`, which encodes all of the relevant values in
#'   a URL, `"server"`, which saves to disk on the server, or
#'   `"disable"`, which disables any previously-enabled bookmarking.
#'
#' @seealso [onBookmark()], [onBookmarked()],
#'   [onRestore()], and [onRestored()] for registering
#'   callback functions that are invoked when the state is bookmarked or
#'   restored.
#'
#'   Also see [updateQueryString()].
#'
#' @export
#' @examples
#' ## Only run these examples in interactive R sessions
#' if (interactive()) {
#'
#' # Basic example with state encoded in URL
#' ui <- function(request) {
#'   fluidPage(
#'     textInput("txt", "Text"),
#'     checkboxInput("chk", "Checkbox"),
#'     bookmarkButton()
#'   )
#' }
#' server <- function(input, output, session) { }
#' enableBookmarking("url")
#' shinyApp(ui, server)
#'
#'
#' # An alternative to calling enableBookmarking(): use shinyApp's
#' # enableBookmarking argument
#' shinyApp(ui, server, enableBookmarking = "url")
#'
#'
#' # Same basic example with state saved to disk
#' enableBookmarking("server")
#' shinyApp(ui, server)
#'
#'
#' # Save/restore arbitrary values
#' ui <- function(req) {
#'   fluidPage(
#'     textInput("txt", "Text"),
#'     checkboxInput("chk", "Checkbox"),
#'     bookmarkButton(),
#'     br(),
#'     textOutput("lastSaved")
#'   )
#' }
#' server <- function(input, output, session) {
#'   vals <- reactiveValues(savedTime = NULL)
#'   output$lastSaved <- renderText({
#'     if (!is.null(vals$savedTime))
#'       paste("Last saved at", vals$savedTime)
#'     else
#'       ""
#'   })
#'
#'   onBookmark(function(state) {
#'     vals$savedTime <- Sys.time()
#'     # state is a mutable reference object, and we can add arbitrary values
#'     # to it.
#'     state$values$time <- vals$savedTime
#'   })
#'   onRestore(function(state) {
#'     vals$savedTime <- state$values$time
#'   })
#' }
#' enableBookmarking(store = "url")
#' shinyApp(ui, server)
#'
#'
#' # Usable with dynamic UI (set the slider, then change the text input,
#' # click the bookmark button)
#' ui <- function(request) {
#'   fluidPage(
#'     sliderInput("slider", "Slider", 1, 100, 50),
#'     uiOutput("ui"),
#'     bookmarkButton()
#'   )
#' }
#' server <- function(input, output, session) {
#'   output$ui <- renderUI({
#'     textInput("txt", "Text", input$slider)
#'   })
#' }
#' enableBookmarking("url")
#' shinyApp(ui, server)
#'
#'
#' # Exclude specific inputs (The only input that will be saved in this
#' # example is chk)
#' ui <- function(request) {
#'   fluidPage(
#'     passwordInput("pw", "Password"), # Passwords are never saved
#'     sliderInput("slider", "Slider", 1, 100, 50), # Manually excluded below
#'     checkboxInput("chk", "Checkbox"),
#'     bookmarkButton()
#'   )
#' }
#' server <- function(input, output, session) {
#'   setBookmarkExclude("slider")
#' }
#' enableBookmarking("url")
#' shinyApp(ui, server)
#'
#'
#' # Update the browser's location bar every time an input changes. This should
#' # not be used with enableBookmarking("server"), because that would create a
#' # new saved state on disk every time the user changes an input.
#' ui <- function(req) {
#'   fluidPage(
#'     textInput("txt", "Text"),
#'     checkboxInput("chk", "Checkbox")
#'   )
#' }
#' server <- function(input, output, session) {
#'   observe({
#'     # Trigger this observer every time an input changes
#'     reactiveValuesToList(input)
#'     session$doBookmark()
#'   })
#'   onBookmarked(function(url) {
#'     updateQueryString(url)
#'   })
#' }
#' enableBookmarking("url")
#' shinyApp(ui, server)
#'
#'
#' # Save/restore uploaded files
#' ui <- function(request) {
#'   fluidPage(
#'     sidebarLayout(
#'       sidebarPanel(
#'         fileInput("file1", "Choose CSV File", multiple = TRUE,
#'           accept = c(
#'             "text/csv",
#'             "text/comma-separated-values,text/plain",
#'             ".csv"
#'           )
#'         ),
#'         tags$hr(),
#'         checkboxInput("header", "Header", TRUE),
#'         bookmarkButton()
#'       ),
#'       mainPanel(
#'         tableOutput("contents")
#'       )
#'     )
#'   )
#' }
#' server <- function(input, output) {
#'   output$contents <- renderTable({
#'     inFile <- input$file1
#'     if (is.null(inFile))
#'       return(NULL)
#'
#'     if (nrow(inFile) == 1) {
#'       read.csv(inFile$datapath, header = input$header)
#'     } else {
#'       data.frame(x = "multiple files")
#'     }
#'   })
#' }
#' enableBookmarking("server")
#' shinyApp(ui, server)
#'
#' }
enableBookmarking <- function(store = c("url", "server", "disable")) {
  store <- match.arg(store)
  shinyOptions(bookmarkStore = store)
}


#' Exclude inputs from bookmarking
#'
#' This function tells Shiny which inputs should be excluded from bookmarking.
#' It should be called from inside the application's server function.
#'
#' This function can also be called from a module's server function, in which
#' case it will exclude inputs with the specified names, from that module. It
#' will not affect inputs from other modules or from the top level of the Shiny
#' application.
#'
#' @param names A character vector containing names of inputs to exclude from
#'   bookmarking.
#' @param session A shiny session object.
#' @seealso [enableBookmarking()] for examples.
#' @export
setBookmarkExclude <- function(names = character(0), session = getDefaultReactiveDomain()) {
  session$setBookmarkExclude(names)
}


#' Add callbacks for Shiny session bookmarking events
#'
#' @description
#'
#' These functions are for registering callbacks on Shiny session events. They
#' should be called within an application's server function.
#'
#' \itemize{
#'   \item `onBookmark` registers a function that will be called just
#'     before Shiny bookmarks state.
#'   \item `onBookmarked` registers a function that will be called just
#'     after Shiny bookmarks state.
#'   \item `onRestore` registers a function that will be called when a
#'     session is restored, after the server function executes, but before all
#'     other reactives, observers and render functions are run.
#'   \item `onRestored` registers a function that will be called after a
#'     session is restored. This is similar to `onRestore`, but it will be
#'     called after all reactives, observers, and render functions run, and
#'     after results are sent to the client browser. `onRestored`
#'     callbacks can be useful for sending update messages to the client
#'     browser.
#' }
#'
#' @details
#'
#' All of these functions return a function which can be called with no
#' arguments to cancel the registration.
#'
#' The callback function that is passed to these functions should take one
#' argument, typically named "state" (for `onBookmark`, `onRestore`,
#' and `onRestored`) or "url" (for `onBookmarked`).
#'
#' For `onBookmark`, the state object has three relevant fields. The
#' `values` field is an environment which can be used to save arbitrary
#' values (see examples). If the state is being saved to disk (as opposed to
#' being encoded in a URL), the `dir` field contains the name of a
#' directory which can be used to store extra files. Finally, the state object
#' has an `input` field, which is simply the application's `input`
#' object. It can be read, but not modified.
#'
#' For `onRestore` and `onRestored`, the state object is a list. This
#' list contains `input`, which is a named list of input values to restore,
#' `values`, which is an environment containing arbitrary values that were
#' saved in `onBookmark`, and `dir`, the name of the directory that
#' the state is being restored from, and which could have been used to save
#' extra files.
#'
#' For `onBookmarked`, the callback function receives a string with the
#' bookmark URL. This callback function should be used to display UI in the
#' client browser with the bookmark URL. If no callback function is registered,
#' then Shiny will by default display a modal dialog with the bookmark URL.
#'
#' @section Modules:
#'
#'   These callbacks may also be used in Shiny modules. When used this way, the
#'   inputs and values will automatically be namespaced for the module, and the
#'   callback functions registered for the module will only be able to see the
#'   module's inputs and values.
#'
#' @param fun A callback function which takes one argument.
#' @param session A shiny session object.
#' @seealso enableBookmarking for general information on bookmarking.
#'
#' @examples
#' ## Only run these examples in interactive sessions
#' if (interactive()) {
#'
#' # Basic use of onBookmark and onRestore: This app saves the time in its
#' # arbitrary values, and restores that time when the app is restored.
#' ui <- function(req) {
#'   fluidPage(
#'     textInput("txt", "Input text"),
#'     bookmarkButton()
#'   )
#' }
#' server <- function(input, output) {
#'   onBookmark(function(state) {
#'     savedTime <- as.character(Sys.time())
#'     cat("Last saved at", savedTime, "\n")
#'     # state is a mutable reference object, and we can add arbitrary values to
#'     # it.
#'     state$values$time <- savedTime
#'   })
#'
#'   onRestore(function(state) {
#'     cat("Restoring from state bookmarked at", state$values$time, "\n")
#'   })
#' }
#' enableBookmarking("url")
#' shinyApp(ui, server)
#'
#'
#'
# This app illustrates two things: saving values in a file using state$dir, and
# using an onRestored callback to call an input updater function. (In real use
# cases, it probably makes sense to save content to a file only if it's much
# larger.)
#' ui <- function(req) {
#'   fluidPage(
#'     textInput("txt", "Input text"),
#'     bookmarkButton()
#'   )
#' }
#' server <- function(input, output, session) {
#'   lastUpdateTime <- NULL
#'
#'   observeEvent(input$txt, {
#'     updateTextInput(session, "txt",
#'       label = paste0("Input text (Changed ", as.character(Sys.time()), ")")
#'     )
#'   })
#'
#'   onBookmark(function(state) {
#'     # Save content to a file
#'     messageFile <- file.path(state$dir, "message.txt")
#'     cat(as.character(Sys.time()), file = messageFile)
#'   })
#'
#'   onRestored(function(state) {
#'     # Read the file
#'     messageFile <- file.path(state$dir, "message.txt")
#'     timeText <- readChar(messageFile, 1000)
#'
#'     # updateTextInput must be called in onRestored, as opposed to onRestore,
#'     # because onRestored happens after the client browser is ready.
#'     updateTextInput(session, "txt",
#'       label = paste0("Input text (Changed ", timeText, ")")
#'     )
#'   })
#' }
#' # "server" bookmarking is needed for writing to disk.
#' enableBookmarking("server")
#' shinyApp(ui, server)
#'
#'
#' # This app has a module, and both the module and the main app code have
#' # onBookmark and onRestore functions which write and read state$values$hash. The
#' # module's version of state$values$hash does not conflict with the app's version
#' # of state$values$hash.
#' #
#' # A basic module that captializes text.
#' capitalizerUI <- function(id) {
#'   ns <- NS(id)
#'   wellPanel(
#'     h4("Text captializer module"),
#'     textInput(ns("text"), "Enter text:"),
#'     verbatimTextOutput(ns("out"))
#'   )
#' }
#' capitalizerServer <- function(input, output, session) {
#'   output$out <- renderText({
#'     toupper(input$text)
#'   })
#'   onBookmark(function(state) {
#'     state$values$hash <- rlang::hash(input$text)
#'   })
#'   onRestore(function(state) {
#'     if (identical(rlang::hash(input$text), state$values$hash)) {
#'       message("Module's input text matches hash ", state$values$hash)
#'     } else {
#'       message("Module's input text does not match hash ", state$values$hash)
#'     }
#'   })
#' }
#' # Main app code
#' ui <- function(request) {
#'   fluidPage(
#'     sidebarLayout(
#'       sidebarPanel(
#'         capitalizerUI("tc"),
#'         textInput("text", "Enter text (not in module):"),
#'         bookmarkButton()
#'       ),
#'       mainPanel()
#'     )
#'   )
#' }
#' server <- function(input, output, session) {
#'   callModule(capitalizerServer, "tc")
#'   onBookmark(function(state) {
#'     state$values$hash <- rlang::hash(input$text)
#'   })
#'   onRestore(function(state) {
#'     if (identical(rlang::hash(input$text), state$values$hash)) {
#'       message("App's input text matches hash ", state$values$hash)
#'     } else {
#'       message("App's input text does not match hash ", state$values$hash)
#'     }
#'   })
#' }
#' enableBookmarking(store = "url")
#' shinyApp(ui, server)
#' }
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
