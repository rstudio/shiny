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
    values = NULL,

    initialize = function(input = NULL, exclude = NULL, onSave = NULL)
    {
      self$input   <- input
      self$exclude <- exclude
      self$onSave  <- onSave
    },

    # Persist this state object to disk. Returns a query string which can be
    # used to restore the session.
    persist = function() {
      id <- createUniqueId(8)

      persistInterface <- getShinyOption("persist.interface",
                                         default = persistInterfaceLocal)

      persistInterface(id, function(stateDir) {
        # Directory is provided by the persistInterface function.
        self$dir <- stateDir

        # Allow user-supplied onSave function to do things like add self$values, or
        # save data to state dir.
        if (!is.null(self$onSave))
          isolate(self$onSave(self))

        # Serialize values, possibly saving some extra data to stateDir
        inputValues <- serializeReactiveValues(self$input, self$exclude, self$dir)
        saveRDS(inputValues, file.path(stateDir, "input.rds"))

        # If there values passed in, save them also
        if (!is.null(self$values))
          saveRDS(self$values, file.path(stateDir, "values.rds"))
      })

      paste0("__state_id__=", encodeURIComponent(id))
    },

    # Encode the state to a URL. This does not save to disk.
    encode = function() {
      inputVals <- serializeReactiveValues(self$input, self$exclude, stateDir = NULL)

      # Allow user-supplied onSave function to do things like add self$values.
      if (!is.null(self$onSave))
        self$onSave(self)

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
      if (length(self$values) != 0) {
        values <- vapply(self$values,
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
  )
)


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

    # Directory for extra files, if restoring from persisted state
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

            } else if (!is.null(qsValues[["__state_id__"]]) && nzchar(qsValues[["__state_id__"]])) {
              # If we have a "__state_id__" key, restore from persisted state and ignore
              # other key/value pairs. If not, restore from key/value pairs in the
              # query string.
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
    # Given a query string with a __state_id__, load persisted state with that ID.
    loadStateQueryString = function(queryString) {
      values <- parseQueryString(queryString, nested = TRUE)
      id <- values[["__state_id__"]]

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

    # Given a query string with values encoded in it, restore persisted state
    # from those values.
    decodeStateQueryString = function(queryString) {
      # Remove leading '?'
      if (substr(queryString, 1, 1) == '?')
        queryString <- substr(queryString, 2, nchar(queryString))

      if (grepl("(^|&)_values_(&|$)", queryString)) {
        splitStr <- strsplit(queryString, "(^|&)_values_(&|$)")[[1]]
        inputValueStr <- splitStr[1]
        valueStr <- splitStr[2]
        if (is.na(valueStr))
          valueStr <- ""

      } else {
        inputValueStr <- queryString
        valueStr <- ""
      }

      inputValues <- parseQueryString(inputValueStr, nested = TRUE)
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

      inputValues <- valuesFromJSON(inputValues)
      self$input <- RestoreInputSet$new(inputValues)

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

  if (identical(getShinyOption("restorable"), FALSE) || !hasCurrentRestoreContext())
    return(default)

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
bookmarkButton <- function(inputId, label = "Bookmark...",
  icon = shiny::icon("link", lib = "glyphicon"),
  title = "Bookmark this application's state and get a URL for sharing.",
  ...)
{
  actionButton(inputId, label, icon, title = title, ...)
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
#' There are two types of bookmarking: persisting state, and encoding state. For
#' persisting state, the state of the application will be saved on disk, and can
#' be restored with the corresponding state ID. For encoding state, the state of
#' the application will be encoded in a URL.
#'
#' For restoring state to work properly, the UI must be a function that takes
#' one argument, \code{req}. In most Shiny applications, the UI is not a
#' function; it might have the form \code{fluidPage(....)}. Converting it to a
#' function is as simple as wrapping it in a function, as in \code{function(req)
#' \{ fluidPage(....) \}}.
#'
#' By default, all input values will be bookmarked, except for the values of
#' actionButtons and passwordInputs. FileInputs will be saved if the state is
#' persisted, but not if if the state is encoded.
#'
#' When persisting state, arbitrary values can be saved to disk, by passing a
#' function as Extra values can be stored, by passing a function as the
#' \code{onBookmark} argument. That function will be passed a
#' \code{\link{ShinySaveState}} object. The \code{values} field of the object
#' can be manipulated to save extra information. Additionally, if the state is
#' being persisted, and the \code{dir} field of that object can be used to save
#' extra information to files in that directory.
#'
#' For persisted state, this is how the persisted state directory is chosen:
#' \itemize{
#'   \item If running in a hosting environment such as Shiny Server or Connect,
#'     the hosting environment will choose the directory.
#'   \item If running an app in a directory with \code{\link{runApp}()}, the
#'     persisted states will be saved in a subdirectory of the app called
#'     shiny_persist.
#'   \item If running a Shiny app object that is generated from code (not run
#'     from a directory), the persisted states will be saved in a subdirectory
#'     of the current working directory called shiny_persist.
#' }
#'
#' @param eventExpr An expression to listen for, similar to
#'   \code{\link{observeEvent}}.
#' @param type Either \code{"encode"}, which encodes all of the relevant values
#'   in a URL, \code{"persist"}, which saves to disk, or \code{"disable"}, which
#'   disables any previously-enabled bookmarking.
#' @param exclude Input values to exclude from bookmarking.
#' @param onBookmark A function to call just before saving state. It will be
#'   passed a \code{\link{ShinySaveState}} object. The \code{values} field of
#'   the object can be manipulated to save extra information, and if the state
#'   is being persisted, the \code{dir} field can be used to save extra
#'   information to files in that directory.
#' @param onRestore A function to call when a session is restored. It will be
#'   called after the server function executes, but before all other reactives,
#'   observers and render functions are run. This function will be passed a list
#'   with three items: \code{input}, a named list with input values; \code{dir},
#'   the path to a directory with other persisted content (only if the state was
#'   persisted and not encoded); and \code{values}, extra values that were saved
#'   with the \code{onBookmark} function.
#' @param onRestored A function to call after a session is restored. This is
#'   similar to \code{onRestore}, but it will be called after all reactives,
#'   observers, and render functions run, and after results are sent to the
#'   client browser. This makes it appropriate for setup code that must be run
#'   only after the client receives its first update -- for example, sending an
#'   update an input that was dynamically generated. This function will be
#'   passed the same object as \code{onRestore}.
#' @param onBookmarked A callback function to invoke after the bookmarking has
#'   been done. The default behavior is to show a modal dialog in the client
#'   browser, with the bookmark URL.
#' @param session A Shiny session object.
#'
#' @examples
#' ## Only run these examples in interactive R sessions
#' if (interactive()) {
#'
#' # Basic example with encoded state
#' ui <- function(req) {
#'   fluidPage(
#'     textInput("txt", "Text"),
#'     checkboxInput("chk", "Checkbox"),
#'     bookmarkButton("bookmark")
#'   )
#' }
#' server <- function(input, output, session) {
#'   configureBookmarking(input$bookmark, type = "encode")
#' }
#' shinyApp(ui, server)
#'
#'
#' # Basic example with persisted state
#' ui <- function(req) {
#'   fluidPage(
#'     textInput("txt", "Text"),
#'     checkboxInput("chk", "Checkbox"),
#'     bookmarkButton("bookmark")
#'   )
#' }
#' server <- function(input, output, session) {
#'   configureBookmarking(input$bookmark, type = "persist")
#' }
#' shinyApp(ui, server)
#'
#'
#' # Update browser's location bar automatically when inputs change
#' ui <- function(req) {
#'   fluidPage(
#'     textInput("txt", "Text"),
#'     checkboxInput("chk", "Checkbox")
#'   )
#' }
#' server <- function(input, output, session) {
#'   configureBookmarking(reactiveValuesToList(input),
#'     type = "encode",
#'     onBookmarked = function(url) {
#'       updateLocationBar(url)
#'     }
#'   )
#' }
#' shinyApp(ui, server)
#'
#'
#' # Save/restore arbitrary values
#' ui <- function(req) {
#'   fluidPage(
#'     textInput("txt", "Text"),
#'     checkboxInput("chk", "Checkbox"),
#'     bookmarkButton("bookmark"),
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
#'   configureBookmarking(input$bookmark,
#'     type = "encode",
#'     onBookmark = function(state) {
#'       vals$savedTime <- as.character(Sys.time())
#'       # state is a mutable reference object, and we can add arbitrary values
#'       # to it.
#'       state$values <- list(
#'         time = vals$savedTime
#'       )
#'     },
#'     onRestore = function(state) {
#'       vals$savedTime <- state$values$time
#'     }
#'   )
#' }
#' shinyApp(ui, server)
#'
#'
#' # Usable with dynamic UI
#' ui <- function(req) {
#'   fluidPage(
#'     sliderInput("slider", "Slider", 1, 100, 50),
#'     uiOutput("ui"),
#'     bookmarkButton("bookmark")
#'   )
#' }
#' server <- function(input, output, session) {
#'   output$ui <- renderUI({
#'     textInput("txt", "Text", input$slider)
#'   })
#'   configureBookmarking(input$bookmark, type = "encode")
#' }
#' shinyApp(ui, server)
#'
#'
#' # Exclude specific inputs
#' # The only input that will be saved in this example is chk
#' ui <- function(req) {
#'   fluidPage(
#'     passwordInput("pw", "Password"),   # Passwords are never saved
#'     sliderInput("slider", "Slider", 1, 100, 50),
#'     checkboxInput("chk", "Checkbox"),
#'     bookmarkButton("bookmark")
#'   )
#' }
#' server <- function(input, output, session) {
#'   configureBookmarking(input$bookmark,
#'     exclude = "slider",
#'     type = "encode"
#'   )
#' }
#' shinyApp(ui, server)
#'
#'
#' # Save/restore uploaded files
#' ui <- function(req){
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
#'         bookmarkButton("bookmark")
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
#'
#'   configureBookmarking(input$bookmark, type = "persist")
#' }
#' shinyApp(ui, server)
#'
#' }
#' @export
configureBookmarking <- function(eventExpr,
  type = c("encode", "persist", "disable"), exclude = NULL,
  onBookmark = NULL, onBookmarked = NULL, onRestore = NULL, onRestored = NULL,
  session = getDefaultReactiveDomain())
{

  eventExpr <- substitute(eventExpr)
  type <- match.arg(type)

  # If there's an existing onBookmarked observer, destroy it before creating a
  # new one.
  if (!is.null(session$bookmarkObserver)) {
    session$bookmarkObserver$destroy()
    session$bookmarkObserver <- NULL
  }

  if (type == "disable") {
    return(invisible())
  }

  # If no onBookmarked function is provided, use one of these defaults.
  if (is.null(onBookmarked)) {
    if (type == "persist") {
      onBookmarked <- function(url) {
        showModal(urlModal(
          url,
          subtitle = "The current state of this application has been persisted."
        ))
      }
    } else if (type == "encode") {
      onBookmarked <- function(url) {
        showModal(urlModal(
          url,
          subtitle = "This link encodes the current state of this application."
        ))
      }
    }
  } else if (!is.function(onBookmarked)) {
    stop("onBookmarked must be a function.")
  }

  session$bookmarkObserver <- observeEvent(
    eventExpr,
    event.env = parent.frame(),
    event.quoted = TRUE,
    {
      tryCatch(
        withLogErrors({
          saveState <- ShinySaveState$new(session$input, exclude, onBookmark)

          if (type == "persist") {
            url <- saveState$persist()
          } else {
            url <- saveState$encode()
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
        }),
        error = function(e) {
          msg <- paste0("Error bookmarking state: ", e$message)
          showNotification(msg, duration = NULL, type = "error")
        }
      )
    }
  )

  # If there was an error initializing the current restore context, show
  # notification in the client.
  observe({
    rc <- getCurrentRestoreContext()
    if (!is.null(rc$initErrorMessage)) {
      showNotification(
        paste("Error in RestoreContext initialization:", rc$initErrorMessage),
        duration = NULL, type = "error"
      )
    }
  })

  # Run the onRestore function at the beginning of the flush cycle, but after
  # the server function has been executed.
  if (!is.null(onRestore)) {
    observe({
      tryCatch(
        withLogErrors(
          isolate({
            rc <- getCurrentRestoreContext()
            if (rc$active) {
              restoreState <- getCurrentRestoreContext()$asList()
              onRestore(restoreState)
            }
          })
        ),
        error = function(e) {
          showNotification(
            paste0("Error calling onRestore(): ", e$message),
            duration = NULL, type = "error"
          )
        }
      )
    }, priority = -1000000)
  }

  # Run the onRestored function after the flush cycle completes and information
  # is sent to the client.
  if (!is.null(onRestored)) {
    session$onFlushed(function() {
      tryCatch(
        withLogErrors(
          isolate({
            rc <- getCurrentRestoreContext()
            if (rc$active) {
              restoreState <- getCurrentRestoreContext()$asList()
              onRestored(restoreState)
            }
          })
        ),
        error = function(e) {
          msg <- paste0("Error calling onRestored(): ", e$message)
          showNotification(msg, duration = NULL, type = "error")
        }
      )
    })
  }

  invisible()
}
