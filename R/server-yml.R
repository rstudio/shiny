#' Launch a Shiny app using the _server.yml standard
#'
#' Implements the `_server.yml` standard for R web frameworks for \pkg{shiny}.
#'
#' @param settings path to the `_server.yml` file.
#' @inheritParams runApp host port
#'
#' @details
#' The provided shiny app must be located next to `_server.yml` or in a subdirectory of its containing path.
#' Configuration options from the `_server.yml` file are passed directly to `runApp()`.
#'
#' A valid `_server.yml` file has the following structure:
#'
#' ```yaml
#' engine: shiny
#'   options:host:"127.0.0.1"
#'   port: null
#'   app_dir: null
#'   quiet: false
#' ```
#' @keywords internal
#' @noRd
#' @examples
#' shiny:::launch_server(system.file(
#'   "examples/12_server_yml/_server.yml",
#'   package = "shiny"
#' ))
#' @seealso [runApp()]
launch_server <- function(
  settings = "_server.yml",
  host = getOption("shiny.host", "127.0.0.1"),
  port = getOption('shiny.port')
) {
  # ensure yaml is installed to read the file
  rlang::check_installed("yaml", "to read '_server.yml' settings.")

  # early abort based on _settings path
  if (!file.exists(settings)) {
    cli::cli_abort(c(
      "File {.file {settings}} does not exist.",
      "i" = " {.arg settings} must point to a {.file _server.yml} file."
    ))
  }

  # type check the host and port
  if (!rlang::is_string(host) && !is.null(host)) {
    cli::cli_abort("{.arg host} must be a string.")
  }

  if (!rlang::is_integerish(port, 1L) && !is.null(port)) {
    cli::cli_abort("{.arg port} must be an integer.")
  }

  # extract config
  config <- yaml::read_yaml(settings)

  # host precedence env_var > config > default
  host <- if (!nzchar(Sys.getenv("HOST"))) {
    config$options$host %||% host
  }

  # if PORT env var can't coerced into integer we fallback to config then port
  port_env <- Sys.getenv("PORT")
  port <- if (is.na(as.integer(port_env))) {
    config$options$port %||% port
  }

  # extract directory name of the _settings.yml file
  # according to standard, app must be relative to the _server.yml file
  containing_dir <- dirname(settings)

  # extract app_dir from config fall back to parent of _server.yml
  app_dir <- config$options$app_dir %||% containing_dir

  is_contained <- startsWith(
    normalizePath(app_dir, mustWork = TRUE),
    normalizePath(containing_dir, mustWork = TRUE)
  )

  if (!is_contained) {
    cli::cli_abort(
      "The {.file _server.yml}'s {.arg app_dir} option must be relative to it."
    )
  }

  runApp(
    app_dir,
    port = port,
    host = host,
    quiet = config$options$quiet %||% FALSE
  )
}
