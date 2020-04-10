# Create a "data mask" suitable for passing to rlang::eval_tidy. Bindings in
# `env` and bindings in the parent of `env` are merged into a single named list.
# Bindings in `env` take precedence over bindings in the parent of `env`.
#' @noRd
makeMask <- function(env) {
  stopifnot(length(rlang::env_parents(env)) > 1)
  child <- as.list(env)
  parent <- as.list(rlang::env_parent(env))
  parent_only <- setdiff(names(parent), names(child))
  append(child, parent[parent_only])
}

#' @noRd
isModuleServer <- function(x) {
  is.function(x) && names(formals(x))[1] == "id"
}

#' Reactive testing for Shiny server functions and modules
#'
#' A way to test the reactive interactions in Shiny applications. Reactive
#' interactions are defined in the server function of applications and in
#' modules.
#' @param app The path to an application or module to test. In addition to
#'   paths, applications may be represented by any object suitable for coercion
#'   to an `appObj` by `as.shiny.appobj`. Application server functions must
#'   include a `session` argument in order to be tested.
#' @param expr Test code containing expectations. The test expression will run
#'   in the server function environment, meaning that the parameters of the
#'   server function (e.g. `input`, `output`, and `session`) will be available
#'   along with any other values created inside of the server function.
#' @param ... Additional arguments to pass to the module function. These
#'   arguments are processed with [rlang::list2()] and so are
#'   _[dynamic][rlang::dyn-dots]_. If `app` is a module, and no `id` argument is
#'   provided, one will be generated and supplied automatically.
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
#' @export
testServer <- function(app, expr, ...) {

  args <- rlang::list2(...)

  session <- getDefaultReactiveDomain()

  if (inherits(session, "MockShinySession"))
    stop("Test expressions may not call testServer()")
  if (inherits(session, "session_proxy")
      && inherits(get("parent", envir = session), "MockShinySession"))
    stop("Modules may not call testServer()")

  session <- MockShinySession$new()
  on.exit(if (!session$isClosed()) session$close())

  if (isModuleServer(app)) {
    if (!("id" %in% names(args)))
      args[["id"]] <- session$genId()
  } else {
    appobj <- as.shiny.appobj(app)
    server <- appobj$serverFuncSource()
    if (! "session" %in% names(formals(server)))
      stop("Tested application server functions must declare input, output, and session arguments.")
    appEnv <- new.env(parent = rlang::caller_env())
    if (is.character(app)) {
      loadSupport(app, appEnv, appEnv)
      environment(server) <- appEnv
    }
    body(server) <- rlang::expr({
      session$setEnv(base::environment())
      !!!body(server)
    })
    app <- function() {
      session$setReturned(server(input = session$input, output = session$output, session = session))
    }
    if (length(args))
      message("Discarding unused arguments to server function")
  }

  isolate(
    withReactiveDomain(
      session,
      withr::with_options(list(`shiny.allowoutputreads` = TRUE), {
        rlang::exec(app, !!!args)
      })
    )
  )

  stopifnot(all(c("input", "output", "session") %in% ls(session$env)))

  quosure <- rlang::enquo(expr)

  isolate(
    withReactiveDomain(
      session,
      withr::with_options(list(`shiny.allowoutputreads` = TRUE), {
        rlang::eval_tidy(quosure, makeMask(session$env), rlang::caller_env())
      })
    )
  )
}
