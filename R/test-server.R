#' @noRd
testCallModule <- function(module, id, session) {
  # TODO alan Figure out what to do with id here, necessary for nested usage?
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

#' @noRd
isModuleServer <- function(x) {
  is.function(x) && names(formals(x))[1] == "id"
}

#' @noRd
coercableToAppObj <- function(x) {
  !is.null(getS3method("as.shiny.appobj", class(x), optional = TRUE))
}

#' Reactive testing for Shiny server functions and modules
#'
#' A way to test the reactive interactions in Shiny applications. Reactive
#' interactions are defined in the server function of applications and in
#' modules.
#' @param app The path to an application or module to test. In addition to
#'   paths, applications may be represented by any object suitable for coercion
#'   to an `appObj` by `as.shiny.appobj`.
#' @param expr Test code containing expectations. The test expression will run
#'   in the server function environment, meaning that the parameters of the
#'   server function (e.g. `input`, `output`, and `session`) will be available
#'   along with any other values created inside of the server function.
#' @param ... Additional arguments to pass to the module function. These
#'   arguments are processed with [rlang::list2()] and so are
#'   _[dynamic][rlang::dyn-dots]_.
#' @return The result of evaluating `expr`.
#' @include mock-session.R
#' @rdname testServer
#' @examples
#' server <- function(id, multiplier = 2, prefix = "I am ") {
#'   moduleServer(id, function(input, output, session) {
#'     myreactive <- reactive({
#'       input$x * multiplier
#'     })
#'     output$txt <- renderText({
#'       paste0(prefix, myreactive())
#'     })
#'   })
#' }
#'
#' # Basic Usage
#' # -----------
#' testServer(server, {
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
#' testServer(server, {
#'   session$setInputs(x = 1)
#'   stopifnot(myreactive() == 2)
#'   stopifnot(output$txt == "I am 2")
#'   # !!/:= and !!! from rlang are used below to splice computed arguments
#'   # into the testModule() argument list.
#' }, !!multiplier_arg_name := 2, !!!more_args)
#' @export
testServer <- function(app, expr, ...) {

  session <- MockShinySession$new()
  on.exit(if (!session$isClosed()) session$close())

  if (coercableToAppObj(app)) {
    appobj <- as.shiny.appobj(app)
    server <- appobj$serverFuncSource()
    if (! "session" %in% names(formals(server)))
      stop("Tested application server functions must declare input, output, and session arguments.")
    body(server) <- rlang::expr({
      session$env <- base::environment()
      !!!body(server)
    })
    app <- function() {
      session$setReturned(server(input = session$input, output = session$output, session = session))
    }
  } else if (!isModuleServer(app)) {
    stop("app argument must be a module function or coercable by as.shiny.appobj")
  }


  isolate(
    withReactiveDomain(
      session,
      withr::with_options(list(`shiny.allowoutputreads` = TRUE), {
        rlang::exec(app, ...)
      })
    )
  )

  quosure <- rlang::enquo(expr)
  isolate(
    withReactiveDomain(
      session,
      withr::with_options(list(`shiny.allowoutputreads`=TRUE), {
        rlang::eval_tidy(quosure, makeMask(session$env), rlang::caller_env())
      })
    )
  )
}
