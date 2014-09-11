#' Reporting progress (object-oriented API)
#'
#' Reports progress to the user during long-running operations.
#'
#' This package exposes two distinct programming APIs for working with
#' progress. \code{\link{withProgress}} and \code{\link{setProgress}}
#' together provide a simple function-based interface, while the
#' \code{Progress} reference class provides an object-oriented API.
#'
#' Instantiating a \code{Progress} object causes a progress panel to be
#' created, and it will be displayed the first time the \code{set}
#' method is called. Calling \code{close} will cause the progress panel
#' to be removed.
#'
#' \strong{Methods}
#'   \describe{
#'     \item{\code{initialize(session, min = 0, max = 1)}}{
#'       Creates a new progress panel (but does not display it).
#'     }
#'     \item{\code{set(message = NULL, detail = NULL, value = NULL)}}{
#'       Updates the progress panel. When called the first time, the
#'       progress panel is displayed.
#'     }
#'     \item{\code{close()}}{
#'       Removes the progress panel. Future calls to \code{set} and
#'       \code{close} will be ignored.
#'     }
#'   }
#'
#' @param session The Shiny session object, as provided by
#'   \code{shinyServer} to the server function.
#' @param min The value that represents the starting point of the
#'   progress bar. Must be less tham \code{max}.
#' @param max The value that represents the end of the progress bar.
#'   Must be greater than \code{min}.
#' @param message A single-element character vector; the message to be
#'   displayed to the user, or \code{NULL} to hide the current message
#'   (if any).
#' @param detail A single-element character vector; the detail message
#'   to be displayed to the user, or \code{NULL} to hide the current
#'   detail message (if any). The detail message will be shown with a
#'   de-emphasized appearance relative to \code{message}.
#' @param value Single-element numeric vector; the value at which to set
#'   the progress bar, relative to \code{min} and \code{max}.
#'   \code{NULL} hides the progress bar, if it is currently visible.
#'
#' @examples
#' \dontrun{
#' # server.R
#' shinyServer(function(input, output, session) {
#'   output$plot <- renderPlot({
#'     progress <- shiny::Progress$new(session, min=1, max=15)
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
#' })
#' }
#' @seealso \code{\link{withProgress}}
#' @format NULL
#' @usage NULL
#' @export
Progress <- R6Class(
  'Progress',
  portable = FALSE,
  public = list(
    .session = 'environment',
    .id = 'character',
    .min = 'numeric',
    .max = 'numeric',
    .value = 'ANY',
    .closed = 'logical',

    initialize = function(session = getDefaultReactiveDomain(), min = 0, max = 1) {
      # A hacky check to make sure the session object is indeed a session object.
      if (is.null(session$onFlush)) stop("'session' is not a session object.")

      .closed <<- FALSE
      .session <<- session
      .id <<- paste(as.character(as.raw(runif(8, min=0, max=255))), collapse='')
      .min <<- min
      .max <<- max
      .value <<- NULL

      .session$sendProgress('open', list(id = .id))
    },
    set = function(message = NULL, detail = NULL, value = NULL) {
      if (.closed) {
        warning("Attempting to set progress, but progress already closed.")
        return()
      }

      if (is.null(value) || is.na(value)) {
        value <- NULL
      } else {
        value <- min(1, max(0, (value - .min) / (.max - .min)))
      }

      .value <<- value

      data <- dropNulls(list(
        id = .id,
        message = message,
        detail = detail,
        value = value
      ))

      .session$sendProgress('update', data)
    },
    getMin = function() .min,
    getMax = function() .max,
    getValue = function() .value,
    close = function() {
      if (.closed) {
        warning("Attempting to close progress, but progress already closed.")
        return()
      }

      .session$sendProgress('close', list(id = .id))
      .closed <<- TRUE
    }
  )
)

#' Reporting progress (functional API)
#'
#' Reports progress to the user during long-running operations.
#'
#' This package exposes two distinct programming APIs for working with progress.
#' Using \code{withProgress} with \code{incProgress} or \code{setProgress}
#' provide a simple function-based interface, while the \code{\link{Progress}}
#' reference class provides an object-oriented API.
#'
#' Use \code{withProgress} to wrap the scope of your work; doing so will cause a
#' new progress panel to be created, and it will be displayed the first time
#' \code{incProgress} or \code{setProgress} are called. When \code{withProgress}
#' exits, the corresponding progress panel will be removed.
#'
#' The \code{incProgress} function increments the status bar by a specified
#' amount, whereas the \code{setProgress} function sets it to a specific value,
#' and can also set the text displayed.
#'
#' Generally, \code{withProgress}/\code{incProgress}/\code{setProgress} should
#' be sufficient; the exception is if the work to be done is asynchronous (this
#' is not common) or otherwise cannot be encapsulated by a single scope. In that
#' case, you can use the \code{Progress} reference class.
#'
#' @param session The Shiny session object, as provided by \code{shinyServer} to
#'   the server function. The default is to automatically find the session by
#'   using the current reactive domain.
#' @param expr The work to be done. This expression should contain calls to
#'   \code{setProgress}.
#' @param min The value that represents the starting point of the progress bar.
#'   Must be less tham \code{max}. Default is 0.
#' @param max The value that represents the end of the progress bar. Must be
#'   greater than \code{min}. Default is 1.
#' @param amount For \code{incProgress}, the amount to increment the status bar.
#'   Default is 0.1.
#' @param env The environment in which \code{expr} should be evaluated.
#' @param quoted Whether \code{expr} is a quoted expression (this is not
#'   common).
#' @param message A single-element character vector; the message to be displayed
#'   to the user, or \code{NULL} to hide the current message (if any).
#' @param detail A single-element character vector; the detail message to be
#'   displayed to the user, or \code{NULL} to hide the current detail message
#'   (if any). The detail message will be shown with a de-emphasized appearance
#'   relative to \code{message}.
#' @param value Single-element numeric vector; the value at which to set the
#'   progress bar, relative to \code{min} and \code{max}. \code{NULL} hides the
#'   progress bar, if it is currently visible.
#'
#' @examples
#' \dontrun{
#' # server.R
#' shinyServer(function(input, output) {
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
#' })
#' }
#' @seealso \code{\link{Progress}}
#' @rdname withProgress
#' @export
withProgress <- function(expr, min = 0, max = 1,
                         value = min + (max - min) * 0.1,
                         message = NULL, detail = NULL,
                         session = getDefaultReactiveDomain(),
                         env = parent.frame(), quoted = FALSE) {

  if (!quoted)
    expr <- substitute(expr)

  p <- Progress$new(session, min = min, max = max)

  session$progressStack$push(p)
  on.exit({
    session$progressStack$pop()
    p$close()
  })

  p$set(message, detail, value)

  eval(expr, env)
}

#' @rdname withProgress
#' @export
setProgress <- function(value = NULL, message = NULL, detail = NULL,
                        session = getDefaultReactiveDomain()) {

  # A hacky check to make sure the session object is indeed a session object.
  if (is.null(session$onFlush)) stop("'session' is not a session object.")

  if (session$progressStack$size() == 0) {
    warning('setProgress was called outside of withProgress; ignoring')
    return()
  }

  session$progressStack$peek()$set(message, detail, value)
  invisible()
}

#' @rdname withProgress
#' @export
incProgress <- function(amount = 0.1, message = NULL, detail = NULL,
                        session = getDefaultReactiveDomain()) {

  # A hacky check to make sure the session object is indeed a session object.
  if (is.null(session$onFlush)) stop("'session' is not a session object.")

  if (session$progressStack$size() == 0) {
    warning('incProgress was called outside of withProgress; ignoring')
    return()
  }

  p <- session$progressStack$peek()
  value <- min(p$getValue() + amount, p$getMax())
  p$set(message, detail, value)
  invisible()
}
