# TODO:
#  - implement testServer
#  - defineOutput - create a defineOutput method
#      https://github.com/rstudio/shiny/blob/aa3c1c80f2eccf13e70503cce9413d75c71003a8/R/shiny.R#L2014
#      Reading from the output: https://github.com/rstudio/shiny/blob/aa3c1c80f2eccf13e70503cce9413d75c71003a8/R/shiny.R#L2022
#      allowOutputReads = TRUE - withr::with_option
#  - We do need to make outputs automatically reactive; for free we could make the accessor for outputs not require
#      evaluation. So expect_equal(output$x, 2) should work.
#      Wrap the outputs that are defined in observers to make them reactive
#  - Do we want the output to be accessible natively, or some $get() on the output? If we do a get() we could
#    do more helpful spy-type things around exec count.
#  - plots and such?

#' Test a shiny module
#' @param module The module under test
#' @param expr Test code containing expectations. The test expression will run
#'   in the module's environment, meaning that the module's parameters (e.g.
#'   `input`, `output`, and `session`) will be available along with any other
#'   values created inside of the module.
#' @param args A list of arguments to pass into the module beyond `input`,
#'   `output`, and `session`.
#' @param initialState A list describing the initial values for `input`. If no
#'   initial state is given, `input` will initialize as an empty list.
#' @export
testModule <- function(module, expr, args, initialState=NULL) {
  # Capture the environment from the module
  # Inserts `session$env <- environment()` at the top of the function
  fn_body <- body(module)
  fn_body[seq(3, length(fn_body)+1)] <- fn_body[seq(2, length(fn_body))]
  fn_body[[2]] <- quote(session$env <- environment())
  body(module) <- fn_body

  # Substitute expr for later evaluation
  expr <- substitute(expr)

  # Cast the initial state to reactive values
  if (!is.null(initialState)){
    inp <- do.call(reactiveValues, initialState)
  } else {
    inp <- reactiveValues()
  }

  out <- list()

  # Create the mock session
  session <- new.env(parent=emptyenv())

  # The onFlush* methods return a deregistration function
  flushCBs <- Callbacks$new()
  session$onFlush <- function(fun, once){
    if (!isTRUE(once)) {
      return(flushCBs$register(fun))
    } else {
      dereg <- flushCBs$register(function() {
        dereg()
        fun()
      })
      return(dereg)
    }
  }
  flushedCBs <- Callbacks$new()
  session$onFlushed <- function(fun, once){
    if (!isTRUE(once)) {
      return(flushedCBs$register(fun))
    } else {
      dereg <- flushedCBs$register(function() {
        dereg()
        fun()
      })
      return(dereg)
    }
  }

  session$input <- inp
  session$output <- out
  isClosed <- FALSE
  session$isEnded <- function(){ isClosed }
  session$isClosed <- function(){ isClosed }
  session$close <- function(){ isClosed <<- TRUE }
  session$cycleStartAction <- function(callback){ callback() } #FIXME: this is wrong. Will need to be more complex.
  endedCBs <- Callbacks$new()
  session$onEnded <- function(sessionEndedCallback){
    endedCBs$register(sessionEndedCallback)
  }

  session$reactlog <- function(logEntry){} # TODO: Needed for mock?
  session$incrementBusyCount <- function(){} # TODO: Needed for mock?

  # Initialize the module
  isolate(
    withReactiveDomain(session,
      module(session$input, session$output, session)
    )
  )

  # Run the test expression in a reactive context and in the module's environment.
  # We don't need to flush before entering the loop because the first expr that we execute is `{`.
  # So we'll already flush before we get to the good stuff.
  for (i in 1:length(expr)){
    e <- expr[[i]]
    isolate({
      # withReactiveDomain(...)
      eval(e, session$env)
    })

    # timerCallbacks must run before flushReact.
    timerCallbacks$executeElapsed()
    isolate(flushCBs$invoke(..stacktraceon = TRUE))
    flushReact()
    isolate(flushedCBs$invoke(..stacktraceon = TRUE))
    later::run_now()
  }

  endedCBs$invoke(onError = printError, ..stacktraceon = TRUE)
}

#' Test an app's server-side logic
#' @param expr Test code containing expectations
#' @param dir The directory root of the Shiny application. If `NULL`, this function
#'   will work up the directory hierarchy --- starting with the current directory ---
#'   looking for a directory that contains an `app.R` or `server.R` file.
#' @export
testServer <- function(expr, dir=NULL) {
  stop("NYI")
}
