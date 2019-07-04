#' Reporting progress (object-oriented API)
#'
#' Reports progress to the user during long-running operations.
#'
#' This package exposes two distinct programming APIs for working with
#' progress. [withProgress()] and [setProgress()]
#' together provide a simple function-based interface, while the
#' `Progress` reference class provides an object-oriented API.
#'
#' Instantiating a `Progress` object causes a progress panel to be
#' created, and it will be displayed the first time the `set`
#' method is called. Calling `close` will cause the progress panel
#' to be removed.
#'
#' As of version 0.14, the progress indicators use Shiny's new notification API.
#' If you want to use the old styling (for example, you may have used customized
#' CSS), you can use `style="old"` each time you call
#' `Progress$new()`. If you don't want to set the style each time
#' `Progress$new` is called, you can instead call
#' [`shinyOptions(progress.style="old")`][shinyOptions] just once, inside the server
#' function.
#'
#' **Methods**
#'   \describe{
#'     \item{`initialize(session, min = 0, max = 1)`}{
#'       Creates a new progress panel (but does not display it).
#'     }
#'     \item{`set(value = NULL, message = NULL, detail = NULL)`}{
#'       Updates the progress panel. When called the first time, the
#'       progress panel is displayed.
#'     }
#'     \item{`inc(amount = 0.1, message = NULL, detail = NULL)`}{
#'       Like `set`, this updates the progress panel. The difference is
#'       that `inc` increases the progress bar by `amount`, instead
#'       of setting it to a specific value.
#'     }
#'     \item{`close()`}{
#'       Removes the progress panel. Future calls to `set` and
#'       `close` will be ignored.
#'     }
#'   }
#'
#' @param session The Shiny session object, as provided by
#'   `shinyServer` to the server function.
#' @param min The value that represents the starting point of the
#'   progress bar. Must be less than `max`.
#' @param max The value that represents the end of the progress bar.
#'   Must be greater than `min`.
#' @param message A single-element character vector; the message to be
#'   displayed to the user, or `NULL` to hide the current message
#'   (if any).
#' @param detail A single-element character vector; the detail message
#'   to be displayed to the user, or `NULL` to hide the current
#'   detail message (if any). The detail message will be shown with a
#'   de-emphasized appearance relative to `message`.
#' @param value A numeric value at which to set
#'   the progress bar, relative to `min` and `max`.
#' @param style Progress display style. If `"notification"` (the default),
#'   the progress indicator will show using Shiny's notification API. If
#'   `"old"`, use the same HTML and CSS used in Shiny 0.13.2 and below
#'   (this is for backward-compatibility).
#' @param amount Single-element numeric vector; the value at which to set
#'   the progress bar, relative to `min` and `max`.
#'   `NULL` hides the progress bar, if it is currently visible.
#' @param amount For the `inc()` method, a numeric value to increment the
#'   progress bar.
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' ui <- fluidPage(
#'   plotOutput("plot")
#' )
#'
#' server <- function(input, output, session) {
#'   output$plot <- renderPlot({
#'     progress <- Progress$new(session, min=1, max=15)
#'     on.exit(progress$close())
#'
#'     progress$set(message = 'Calculation in progress',
#'                  detail = 'This may take a while...')
#'
#'     for (i in 1:15) {
#'       progress$set(value = i)
#'       Sys.sleep(0.5)
#'     }
#'     plot(cars)
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#' @seealso [withProgress()]
#' @format NULL
#' @usage NULL
#' @export
Progress <- R6Class(
  'Progress',
  public = list(

    initialize = function(session = getDefaultReactiveDomain(),
      min = 0, max = 1,
      style = getShinyOption("progress.style", default = "notification"))
    {
      if (is.null(session$progressStack))
        stop("'session' is not a ShinySession object.")

      private$session <- session
      private$id <- createUniqueId(8)
      private$min <- min
      private$max <- max
      private$value <- NULL
      private$style <- match.arg(style, choices = c("notification", "old"))
      private$closed <- FALSE

      session$sendProgress('open', list(id = private$id, style = private$style))
    },

    set = function(value = NULL, message = NULL, detail = NULL) {
      if (private$closed) {
        warning("Attempting to set progress, but progress already closed.")
        return()
      }

      if (is.null(value) || is.na(value))
        value <- NULL

      if (!is.null(value)) {
        private$value <- value
        # Normalize value to number between 0 and 1
        value <- min(1, max(0, (value - private$min) / (private$max - private$min)))
      }

      data <- dropNulls(list(
        id = private$id,
        message = message,
        detail = detail,
        value = value,
        style = private$style
      ))

      private$session$sendProgress('update', data)
    },

    inc = function(amount = 0.1, message = NULL, detail = NULL) {
      if (is.null(private$value))
        private$value <- private$min

      value <- min(private$value + amount, private$max)
      self$set(value, message, detail)
    },

    getMin = function() private$min,

    getMax = function() private$max,

    getValue = function() private$value,

    close = function() {
      if (private$closed) {
        warning("Attempting to close progress, but progress already closed.")
        return()
      }

      private$session$sendProgress('close',
        list(id = private$id, style = private$style)
      )
      private$closed <- TRUE
    }
  ),

  private = list(
    session = 'ShinySession',
    id = character(0),
    min = numeric(0),
    max = numeric(0),
    style = character(0),
    value = numeric(0),
    closed = logical(0)
  )
)

#' Reporting progress (functional API)
#'
#' Reports progress to the user during long-running operations.
#'
#' This package exposes two distinct programming APIs for working with progress.
#' Using `withProgress` with `incProgress` or `setProgress`
#' provide a simple function-based interface, while the [Progress()]
#' reference class provides an object-oriented API.
#'
#' Use `withProgress` to wrap the scope of your work; doing so will cause a
#' new progress panel to be created, and it will be displayed the first time
#' `incProgress` or `setProgress` are called. When `withProgress`
#' exits, the corresponding progress panel will be removed.
#'
#' The `incProgress` function increments the status bar by a specified
#' amount, whereas the `setProgress` function sets it to a specific value,
#' and can also set the text displayed.
#'
#' Generally, `withProgress`/`incProgress`/`setProgress` should
#' be sufficient; the exception is if the work to be done is asynchronous (this
#' is not common) or otherwise cannot be encapsulated by a single scope. In that
#' case, you can use the `Progress` reference class.
#'
#' As of version 0.14, the progress indicators use Shiny's new notification API.
#' If you want to use the old styling (for example, you may have used customized
#' CSS), you can use `style="old"` each time you call
#' `withProgress()`. If you don't want to set the style each time
#' `withProgress` is called, you can instead call
#' [`shinyOptions(progress.style="old")`][shinyOptions] just once, inside the server
#' function.
#'
#' @param session The Shiny session object, as provided by `shinyServer` to
#'   the server function. The default is to automatically find the session by
#'   using the current reactive domain.
#' @param expr The work to be done. This expression should contain calls to
#'   `setProgress`.
#' @param min The value that represents the starting point of the progress bar.
#'   Must be less tham `max`. Default is 0.
#' @param max The value that represents the end of the progress bar. Must be
#'   greater than `min`. Default is 1.
#' @param amount For `incProgress`, the amount to increment the status bar.
#'   Default is 0.1.
#' @param env The environment in which `expr` should be evaluated.
#' @param quoted Whether `expr` is a quoted expression (this is not
#'   common).
#' @param message A single-element character vector; the message to be displayed
#'   to the user, or `NULL` to hide the current message (if any).
#' @param detail A single-element character vector; the detail message to be
#'   displayed to the user, or `NULL` to hide the current detail message
#'   (if any). The detail message will be shown with a de-emphasized appearance
#'   relative to `message`.
#' @param style Progress display style. If `"notification"` (the default),
#'   the progress indicator will show using Shiny's notification API. If
#'   `"old"`, use the same HTML and CSS used in Shiny 0.13.2 and below
#'   (this is for backward-compatibility).
#' @param value Single-element numeric vector; the value at which to set the
#'   progress bar, relative to `min` and `max`.
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#' options(device.ask.default = FALSE)
#'
#' ui <- fluidPage(
#'   plotOutput("plot")
#' )
#'
#' server <- function(input, output) {
#'   output$plot <- renderPlot({
#'     withProgress(message = 'Calculation in progress',
#'                  detail = 'This may take a while...', value = 0, {
#'       for (i in 1:15) {
#'         incProgress(1/15)
#'         Sys.sleep(0.25)
#'       }
#'     })
#'     plot(cars)
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#' @seealso [Progress()]
#' @rdname withProgress
#' @export
withProgress <- function(expr, min = 0, max = 1,
  value = min + (max - min) * 0.1,
  message = NULL, detail = NULL,
  style = getShinyOption("progress.style", default = "notification"),
  session = getDefaultReactiveDomain(),
  env = parent.frame(), quoted = FALSE)
{

  if (!quoted)
    expr <- substitute(expr)

  if (is.null(session$progressStack))
    stop("'session' is not a ShinySession object.")

  style <- match.arg(style, c("notification", "old"))

  p <- Progress$new(session, min = min, max = max, style = style)

  session$progressStack$push(p)
  on.exit({
    session$progressStack$pop()
    p$close()
  })

  p$set(value, message, detail)

  eval(expr, env)
}

#' @rdname withProgress
#' @export
setProgress <- function(value = NULL, message = NULL, detail = NULL,
                        session = getDefaultReactiveDomain()) {

  if (is.null(session$progressStack))
    stop("'session' is not a ShinySession object.")

  if (session$progressStack$size() == 0) {
    warning('setProgress was called outside of withProgress; ignoring')
    return()
  }

  session$progressStack$peek()$set(value, message, detail)
  invisible()
}

#' @rdname withProgress
#' @export
incProgress <- function(amount = 0.1, message = NULL, detail = NULL,
                        session = getDefaultReactiveDomain()) {

  if (is.null(session$progressStack))
    stop("'session' is not a ShinySession object.")

  if (session$progressStack$size() == 0) {
    warning('incProgress was called outside of withProgress; ignoring')
    return()
  }

  p <- session$progressStack$peek()
  p$inc(amount, message, detail)
  invisible()
}
