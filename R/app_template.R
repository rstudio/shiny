#' Generate a Shiny application from a template
#'
#' This function populates a directory with files for a Shiny application.
#'
#' In an interactive R session, this function will, by default, prompt the user
#' to select which components to add to the application. Choices are
#'
#' ```
#' 1: All
#' 2: app.R              : Main application file
#' 3: R/example.R        : Helper file with R code
#' 4: R/example-module.R : Example module
#' 5: tests/testthat/    : Tests using the testthat and shinytest2 package
#' ```
#'
#' If option 1 is selected, the full example application including the
#' following files and directories is created:
#'
#' ```
#' appdir/
#' |- app.R
#' |- R
#' |   |- example-module.R
#' |   `- example.R
#' `- tests
#'     |- testthat.R
#'     `- testthat
#'         |- setup-shinytest2.R
#'         |- test-examplemodule.R
#'         |- test-server.R
#'         |- test-shinytest2.R
#'         `- test-sort.R
#' ```
#'
#' Some notes about these files:
#' * `app.R` is the main application file.
#' * All files in the `R/` subdirectory are automatically sourced when the
#'   application is run.
#' * `R/example.R` and `R/example-module.R` are automatically sourced when
#'   the application is run. The first contains a function `lexical_sort()`,
#'   and the second contains code for module created by the
#'   [moduleServer()] function, which is used in the application.
#' * `tests/` contains various tests for the application. You may
#'   choose to use or remove any of them. They can be executed by the
#'   [runTests()] function.
#' * `tests/testthat.R` is a test runner for test files in the
#'   `tests/testthat/` directory using the
#'   [shinytest2](https://rstudio.github.io/shinytest2/reference/test_app.html)
#'   package.
#' * `tests/testthat/setup-shinytest2.R` is setup file to source your `./R` folder into the testing environment.
#' * `tests/testthat/test-examplemodule.R` is a test for an application's module server function.
#' * `tests/testthat/test-server.R` is a test for the application's server code
#' * `tests/testthat/test-shinytest2.R` is a test that uses the
#'   [shinytest2](https://rstudio.github.io/shinytest2/) package to do
#'   snapshot-based testing.
#' * `tests/testthat/test-sort.R` is a test for a supporting function in the `R/` directory.
#'
#' @param path Path to create new shiny application template.
#' @param examples Either one of "default", "ask", "all", or any combination of
#'   "app", "rdir", "module", and "tests". In an
#'   interactive session, "default" falls back to "ask"; in a non-interactive
#'   session, "default" falls back to "all". With "ask", this function will
#'   prompt the user to select which template items will be added to the new app
#'   directory. With "all", all template items will be added to the app
#'   directory.
#' @param dryrun If `TRUE`, don't actually write any files; just print out which
#'   files would be written.
#'
#' @export
shinyAppTemplate <- function(path = NULL, examples = "default", dryrun = FALSE)
{
  if (is.null(path)) {
    stop("Please provide a `path`.")
  }

  # =======================================================
  # Option handling
  # =======================================================

  choices <- c(
    app    = "app.R              : Main application file",
    rdir   = "R/example.R        : Helper file with R code",
    module = "R/example-module.R : Example module",
    tests  = "tests/testthat/    : Tests using {testthat} and {shinytest2}"
  )

  # Support legacy value
  examples[examples == "shinytest"] <- "tests"
  examples[examples == "testthat"] <- "tests"
  examples <- unique(examples)

  if (identical(examples, "default")) {
    if (rlang::is_interactive()) {
      examples <- "ask"
    } else {
      examples <- "all"
    }
  }

  if (!identical(examples, "ask") &&
      !identical(examples, "all") &&
      any(! examples %in% names(choices)))
  {
    stop('`examples` must be one of "default", "ask", "all", or any combination of "',
      paste(names(choices), collapse = '", "'), '".')
  }

  if (identical(examples, "ask")) {
    response <- select_menu(
      c(all = "All", choices),
      title = paste0(
        "Select which of the following to add at ", path, "/ :"
      ),
      msg = "Enter one or more numbers (with spaces), or an empty line to exit: \n"
    )

    examples <- names(response)
  }

  examples <- unique(examples)

  if ("all" %in% examples) {
    examples <- names(choices)
  }

  if (length(examples) == 0) {
    return(invisible())
  }

  if ("tests" %in% examples) {
    rlang::check_installed("shinytest2", "for {testthat} tests to work as expected", version = "0.2.0")
  }

  # =======================================================
  # Utility functions
  # =======================================================

  # Check if a directory is empty, ignoring certain files
  dir_is_empty <- function(path) {
    files <- list.files(path, all.files = TRUE, no.. = TRUE)
    # Ignore .DS_Store files, which are sometimes automatically created on macOS
    files <- setdiff(files, ".DS_Store")
    return(length(files) != 0)
  }

  # Helper to resolve paths relative to our template
  template_path <- function(...) {
    system_file("app_template", ..., package = "shiny")
  }

  # Resolve path relative to destination
  dest_path <- function(...) {
    file.path(path, ...)
  }

  mkdir <- function(path) {
    if (!dirExists(path)) {
      message("Creating ", ensure_trailing_slash(path))
      if (!dryrun) {
        dir.create(path, recursive = TRUE)
      }
    }
  }

  # Copy a file from the template directory to the destination directory. If the
  # file has templating code (it contains `{{` in the text), then run it through
  # the htmlTemplate().
  copy_file_one <- function(name) {
    from <- template_path(name)
    to <- dest_path(name)

    message("Creating ", to)
    if (file.exists(to)) {
      stop(to, " already exists. Please remove it and try again.", call. = FALSE)
    }

    if (!dryrun) {
      is_template <- any(grepl("{{", readLines(from), fixed = TRUE))

      if (is_template) {
        writeChar(
          as.character(htmlTemplate(
            from,
            rdir = "rdir" %in% examples,
            module = "module" %in% examples
          )),
          con = to,
          eos = NULL
        )
      } else {
        file.copy(from, to)
      }
    }
  }

  # Copy multiple files from template to destination.
  copy_file <- function(names) {
    for (name in names) {
      copy_file_one(name)
    }
  }

  # Copy the files for a tests/ subdirectory
  copy_test_dir <- function() {
    files <- dir(template_path("tests"), recursive = TRUE)

    # Filter out files that are not module files in the R directory.
    if (! "rdir" %in% examples) {
      # find all files in the testthat folder that are not module or server files
      is_r_folder_file <- !grepl("module|server|shinytest2|testthat", basename(files))
      files <- files[!is_r_folder_file]
    }

    # Filter out module files, if applicable.
    if (! "module" %in% examples) {
      files <- files[!grepl("module", files)]
    }

    mkdir(dest_path("tests"))

    # Create any subdirectories if needed
    dirs <- setdiff(unique(dirname(files)), ".")
    for (dir in dirs) {
      mkdir(dest_path("tests", dir))
    }

    copy_file(file.path("tests", files))
  }

  # =======================================================
  # Main function
  # =======================================================

  if (is.null(path)) {
    stop("`path` is missing.")
  }
  if (file.exists(path) && !dirExists(path)) {
    stop(path, " exists but is not a directory.")
  }

  if (dirExists(path) && dir_is_empty(path)) {
    if (interactive()) {
      response <- readline(paste0(
        ensure_trailing_slash(path),
        " is not empty. Do you want to use this directory anyway? [y/n] "
      ))
      if (tolower(response) != "y") {
        return(invisible())
      }
    }
  } else {
    mkdir(path)
  }

  if ("app" %in% examples) {
    copy_file("app.R")
  }

  # R/ dir with non-module files
  if ("rdir" %in% examples) {
    files <- dir(template_path("R"))
    non_module_files <- files[!grepl("module.R$", files)]
    mkdir(dest_path("R"))
    copy_file(file.path("R", non_module_files))
  }

  # R/ dir with module files
  if ("module" %in% examples) {
    files <- dir(template_path("R"))
    module_files <- files[grepl("module.R$", files)]
    mkdir(dest_path("R"))
    copy_file(file.path("R", module_files))
  }

  # tests/testthat dir
  if ("tests" %in% examples) {
    copy_test_dir()
  }

  invisible()
}
