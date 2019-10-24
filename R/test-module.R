

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
#' @param ... Additional named arguments to be passed on to the module function.
#' @include mock-session.R
#' @export
testModule <- function(module, expr, args, ...) {
  expr <- substitute(expr)
  .testModule(module, expr, args, ...)
}

.testModule <- function(module, expr, args, ...) {
  # Capture the environment from the module
  # Inserts `session$env <- environment()` at the top of the function
  fn_body <- body(module)
  fn_body[seq(3, length(fn_body)+1)] <- fn_body[seq(2, length(fn_body))]
  fn_body[[2]] <- quote(session$env <- environment())
  body(module) <- fn_body

  # Substitute expr for later evaluation
  if (!is.call(expr)){
    expr <- substitute(expr)
  }

  # Create a mock session
  session <- MockShinySession$new()

  # Parse the additional arguments
  args <- list(...)
  args[["input"]] <- session$input
  args[["output"]] <- session$output
  args[["session"]] <- session

  # Initialize the module
  isolate(
    withReactiveDomain(
      session,
      withr::with_options(list(`shiny.allowoutputreads`=TRUE), {
        # Remember that invoking this module implicitly assigns to `session$env`
        # Also, assigning to `$returned` will cause a flush to happen automatically.
        session$returned <- do.call(module, args)
      })
    )
  )

  # Run the test expression in a reactive context and in the module's environment.
  # We don't need to flush before entering the loop because the first expr that we execute is `{`.
  # So we'll already flush before we get to the good stuff.
  isolate({
    withReactiveDomain(
      session,
      withr::with_options(list(`shiny.allowoutputreads`=TRUE), {
        eval(expr, new.env(parent=session$env))
      })
    )
  })

  if (!session$isClosed()){
    session$close()
  }
}

#' Test an app's server-side logic
#' @param expr Test code containing expectations
#' @param appdir The directory root of the Shiny application. If `NULL`, this function
#'   will work up the directory hierarchy --- starting with the current directory ---
#'   looking for a directory that contains an `app.R` or `server.R` file.
#' @export
testServer <- function(expr, appDir=NULL) {
  if (is.null(appDir)){
    appDir <- findApp()
  }

  app <- shinyAppDir(appDir)
  server <- app$serverFuncSource()

  # Add `session` argument if not present
  fn_formals <- formals(server)
  if (! "session" %in% names(fn_formals)) {
    fn_formals$session <- bquote()
    formals(server) <- fn_formals
  }

  s3 <<- server
  # Now test the server as we would a module
  .testModule(server, expr=substitute(expr))
}

findApp <- function(startDir="."){
  dir <- normalizePath(startDir)

  # The loop will either return or stop() itself.
  while (TRUE){
    if(file.exists.ci(file.path(dir, "app.R")) || file.exists.ci(file.path(dir, "server.R"))){
      return(dir)
    }

    # Move up a directory
    origDir <- dir
    dir <- dirname(dir)

    # Testing for "root" path can be tricky. OSs differ and on Windows, network shares
    # might have a \\ prefix. Easier to just see if we got stuck and abort.
    if (dir == origDir){
      # We can go no further.
      stop("No shiny app was found in ", startDir, " or any of its parent directories")
    }
  }
}
