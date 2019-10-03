#' Test a shiny module
#' @param module The module under test
#' @param expr Test code containing expectations
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
  session$input <- inp
  session$output <- out

  # Initialize the module
  module(session$input, session$output, session)

  # Run the test expression in a reactive context and in the module's environment.
  isolate({
    eval(expr, session$env)
  })
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
