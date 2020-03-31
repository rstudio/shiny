

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

#' @noRd
#' @importFrom withr with_options
.testModule <- function(module, quosure, dots, env) {
  # Modify the module function locally by inserting `session$env <-
  # environment()` at the beginning of its body. The dynamic environment of the
  # module function is saved so that it may be referenced after the module
  # function has returned. The saved dynamic environment is the basis for the
  # `data` argument of tidy_eval() when used below to evaluate `quosure`, the
  # test code expression.
  body(module) <- rlang::expr({
    session$env <- base::environment()
    !!!body(module)
  })

  session <- MockShinySession$new()
  on.exit(if (!session$isClosed()) session$close())
  args <- append(dots, list(input = session$input, output = session$output, session = session))

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

#' @noRd
testCallModule <- function(module, id, session) {
  # TODO alan Figure out what to do with id here, necessary for nested usage
  body(module) <- rlang::expr({
    session$env <- base::environment()
    !!!body(module)
  })

  session$setReturned(do.call(module, list(
    input = session$input,
    output = session$output,
    session = session
  )))
}

# Create a "data mask" suitable for passing to rlang::eval_tidy. Bindings in
# `env` and bindings in the parent of `env` are merged into a single named list.
# Bindings in `env` take precedence over bindings in the parent of `env`.
#' @noRd
makeMask <- function(env) {
  stopifnot(length(rlang::env_parents(env)) > 1)
  stopifnot(all(c("input", "output", "session") %in% ls(env)))
  child <- as.list(env)
  parent <- as.list(rlang::env_parent(env))
  parent_only <- setdiff(names(parent), names(child))
  append(child, parent[parent_only])
}

#' Test an app's server-side logic
#' @param appDir The directory root of the Shiny application. If `NULL`, this function
#'   will work up the directory hierarchy --- starting with the current directory ---
#'   looking for a directory that contains an `app.R` or `server.R` file.
#' @rdname testModule
#' @export
testServer <- function(app, expr, ...) {
  session <- MockShinySession$new()
  on.exit(if (!session$isClosed()) session$close())
  quosure <- rlang::enquo(expr)
  isolate(
    withReactiveDomain(
      session,
      withr::with_options(list(`shiny.allowoutputreads` = TRUE), {
        rlang::exec(app, ...)
      })
    )
  )
  isolate(
    withReactiveDomain(
      session,
      withr::with_options(list(`shiny.allowoutputreads`=TRUE), {
        rlang::eval_tidy(quosure, makeMask(session$env), rlang::caller_env())
      })
    )
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
