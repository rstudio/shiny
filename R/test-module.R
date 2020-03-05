

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
#' @return The result of evaluating `expr`.
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
  .testModule(
    module,
    quosure = rlang::enquo(expr),
    dots = rlang::list2(...),
    env = rlang::caller_env()
  )
}

isOldModule <- function(func) {
  stopifnot(is.function(func))
  required <- c("input", "output", "session")
  declared <- names(formals(func))
  setequal(required, intersect(required, declared))
}

#' @noRd
.testModule <- function(module, quosure, dots, env) {
  session <- MockShinySession$new()
  on.exit(if (!session$isClosed()) session$close())

  if (isOldModule(module)) {
    # If the module is an "old-style" module that accepts input, output, and
    # session parameters, modify the function locally by inserting `session$env
    # <- environment()` at the beginning of its body. The dynamic environment of
    # the module function is saved so that it may be referenced after the module
    # function has returned. The saved dynamic environment is the basis for the
    # `data` argument of tidy_eval() when used below to evaluate `quosure`.
    body(module) <- rlang::expr({
      session$env <- base::environment()
      !!!body(module)
    })
    args <- append(dots, list(input = session$input, output = session$output, session = session))
  } else {
    # If the module is a "new-style" module, we rely on logic in callModule()
    # that instruments the function if the session is a MockShinySession.
    # Appending additional arguments is not necessary, as input/output/session
    # will be provided in moduleServer().
    args <- dots
  }

  isolate(
    withReactiveDomain(
      session,
      withr::with_options(list(`shiny.allowoutputreads`=TRUE), {
        # Assigning to `$returned` causes a flush to happen automatically.
        session$returned <- do.call(module, args)
      })
    )
  )

  # Evaluate `quosure` in a reactive context, and in the provided `env`, but
  # with `env` masked by a shallow view of `session$env`, the environment that
  # was saved when the module function was invoked. flush is not needed before
  # entering the loop because the first expr executed is `{`.
  isolate({
    withReactiveDomain(
      session,
      withr::with_options(list(`shiny.allowoutputreads`=TRUE), {
        rlang::eval_tidy(
          quosure,
          data = rlang::as_data_mask(as.list(session$env)),
          env = env
        )
      })
    )
  })
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

  # Test the server function almost as if it were a module. `dots` is empty
  # because server functions never take additional arguments.
  .testModule(
    server,
    quosure = rlang::enquo(expr),
    dots = list(),
    env = rlang::caller_env()
  )
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
