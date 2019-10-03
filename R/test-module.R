#' Test a shiny module
#' @param module The module under test
#' @param expr Test code containing expectations
#' @param args A list of arguments to pass into the module beyond `input`,
#'   `output`, and `session`.
#' @param initialState A list describing the initial values for `input`. If no
#'   initial state is given, `input` will initialize as an empty list.
testModule <- function(module, expr, args, initialState=NULL) {
  # Insert `session$env <- environment()` at the top of the function
  fn_body <- body(module)
  fn_body[seq(3, length(fn_body)+1)] <- fn_body[seq(2, length(fn_body))]
  fn_body[[2]] <- quote(session$env <- environment())
  body(module) <- fn_body

  # Substitute expr for later evaluation
  expr <- substitute(expr)

  if (!is.null(initialState)){
    inp <- do.call(reactiveValues, initialState)
  } else {
    inp <- reactiveValues()
  }
  out <- list()
  session <- new.env(parent=emptyenv())
  session$input <- inp
  session$output <- out
  module(session$input, session$output, session)

  # for (i in 1:length(expr)){
  #   e <- expr[[i]]
  #   print(e)
  #   isolate({
  #     eval(e, session$env)
  #   })
  #
  #   flushReact()
  #   later::run_now()
  #   timerCallbacks$executeElapsed()
  #   #session$onFlushed()
  # }

  isolate({
    eval(expr, session$env)
  })
}

#' Test an app's server-side logic
#' @param expr Test code containing expectations
#' @param dir The directory root of the Shiny application. If `NULL`, this function
#'   will work up the directory hierarchy --- starting with the current directory ---
#'   looking for a directory that contains an `app.R` or `server.R` file.
testServer <- function(expr, dir=NULL) {
  stop("NYI")
}
