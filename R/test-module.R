

#' Integration testing for Shiny modules or server functions
#'
#' Offer a way to test the reactive interactions in Shiny --- either in Shiny
#' modules or in the server portion of a Shiny application. For more
#' information, visit [the Shiny Dev Center article on integration
#' testing](https://shiny.rstudio.com/articles/integration-testing.html).
#' @param module The module to test
#' @param expr Test code containing expectations. The test expression will run
#'   in the module's environment, meaning that the module's parameters (e.g.
#'   `input`, `output`, and `session`) will be available along with any other
#'   values created inside of the module.
#' @param ... Additional arguments to pass to the module function. These
#'   arguments are processed with [rlang::list2()] and so are
#'   _[dynamic][rlang::dyn-dots]_.
#' @include mock-session.R
#' @rdname testModule
#' @examples
#' module <- function(input, output, session, multiplier = 2, prefix = "I am ") {
#'   myreactive <- reactive({
#'     input$x * multiplier
#'   })
#'   output$txt <- renderText({
#'     paste0(prefix, myreactive())
#'   })
#' }
#'
#' # Basic Usage
#' # -----------
#' testModule(module, {
#'   session$setInputs(x = 1)
#'   # You're also free to use third-party
#'   # testing packages like testthat:
#'   #   expect_equal(myreactive(), 2)
#'   stopifnot(myreactive() == 2)
#'   stopifnot(output$txt == "I am 2")
#'
#'   session$setInputs(x = 2)
#'   stopifnot(myreactive() == 4)
#'   stopifnot(output$txt == "I am 4")
#'   # Any additional arguments, below, are passed along to the module.
#' }, multiplier = 2)
#'
#' # Advanced Usage
#' # --------------
#' multiplier_arg_name = "multiplier"
#' more_args <- list(prefix = "I am ")
#' testModule(module, {
#'   session$setInputs(x = 1)
#'   stopifnot(myreactive() == 2)
#'   stopifnot(output$txt == "I am 2")
#'   # !!/:= and !!! from rlang are used below to splice computed arguments
#'   # into the testModule() argument list.
#' }, !!multiplier_arg_name := 2, !!!more_args)
#' @export
testModule <- function(module, expr, ...) {
  expr <- substitute(expr)
  .testModule(module, expr, ...)
}

#' @noRd
#' @importFrom withr with_options
.testModule <- function(module, expr, ...) {
  # Capture the environment from the module
  # Inserts `session$env <- environment()` at the top of the function
  body(module) <- rlang::expr({
    session$env <- environment()
    !!!body(module)
  })

  # Create a mock session
  session <- MockShinySession$new()

  # Parse the additional arguments
  args <- rlang::list2(..., input = session$input, output = session$output, session = session)

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
#' @param appDir The directory root of the Shiny application. If `NULL`, this function
#'   will work up the directory hierarchy --- starting with the current directory ---
#'   looking for a directory that contains an `app.R` or `server.R` file.
#' @rdname testModule
#' @export
testServer <- function(expr, appDir=NULL) {
  if (is.null(appDir)){
    appDir <- findApp()
  }

  app <- shinyAppDir(appDir)
  message("Testing application found in: ", appDir)
  server <- app$serverFuncSource()

  origwd <- getwd()
  setwd(appDir)
  on.exit({ setwd(origwd) }, add=TRUE)

  # Add `session` argument if not present
  fn_formals <- formals(server)
  if (! "session" %in% names(fn_formals)) {
    fn_formals$session <- bquote()
    formals(server) <- fn_formals
  }

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
