#' Generate a Shiny application from a template
#'
#' This function populates a directory with files for a Shiny application.
#'
#' In an interactive R session, this function will, by default, prompt the user
#' which components to add to the application.
#'
#' The full example application includes the following files and directories:
#'
#' ```
#' appdir/
#' |- app.R
#' |- R
#' |   |- my-module.R
#' |   |- sort.R
#' `-- tests
#'     |- shinytest.R
#'     |- shinytest
#'     |   `- mytest.R
#'     |- testthat.R
#'     `-- testthat
#'         |- helper-load.R
#'     |   |- test-mymodule.R
#'     |   `- test-server.R
#'         `- test-sort.R
#' ```
#'
#' Some notes about these files:
#' * `app.R` is the main application file.
#' * All files in the `R/` subdirectory are automatically sourced when the
#'   application is run.
#' * `R/sort.R` and `R/my-module.R` are automatically sourced when
#'   the application is run. The first contains a function `lexical_sort()`,
#'   and the second contains code for a [Shiny module](moduleServer()) which
#'   is used in the application.
#' * `tests/` contains various tests for the application. You may
#'   choose to use or remove any of them. They can be executed by the
#'   [runTests()] function.
#' * `tests/shinytest.R` is a test runner for test files in the
#'   `tests/shinytest/` directory.
#' * `tests/shinytest/mytest.R` is a test that uses the
#'   [shinytest](https://rstudio.github.io/shinytest/) package to do
#'   snapshot-based testing.
#' * `tests/testthat.R` is a test runner for test files in the
#'   `tests/testthat/` directory using the [testthat](https://testthat.r-lib.org/) package.
#' * `tests/testthat/test-mymodule.R` is a test for an application's module server function.
#' * `tests/testthat/test-server.R` is a test for the application's server code
#' * `tests/testthat/test-sort.R` is a test for a supporting function in the `R/` directory.
#'
#' @param path Path to create new shiny application template.
#' @param examples Either one of "default", "ask", "all", or any combination of
#'   "app", "rdir", "module", "shinytest", and "testthat". In an
#'   interactive session, "default" falls back to "ask"; in a non-interactive
#'   session, "default" falls back to "all". With "ask", this function will
#'   prompt the user to select which template items will be added to the new app
#'   directory. With "all", all template items will be added to the app
#'   directory.
#' @param dryrun If `TRUE`, don't actually write any files; just print out what
#'   would be written.
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
    app       = "app.R            : Main application file",
    rdir      = "R/sort.R         : Helper file with R code",
    module    = "R/my-module.R    : Example module",
    shinytest = "tests/shinytest/ : Tests using shinytest package",
    testthat  = "tests/testthat/  : Tests using testthat"
  )

  if (identical(examples, "default")) {
    if (interactive()) {
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

  if ("shinytest" %in% examples) {
    if (system.file(package = "shinytest") != "" &&
        packageVersion("shinytest") <= "1.3.1.9000")
    {
      message(
        "The tests/shinytest directory needs shinytest 1.4.0 or later to work properly.\n",
        "You currently have shinytest ", packageVersion("shinytest"), " installed."
      )
    }
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

  # Helper to resolve paths relative to our example
  example_path <- function(path) {
    system.file("app_template", path, package = "shiny")
  }

  mkdir <- function(path) {
    if (!dirExists(path)) {
      message("Creating ", ensure_trailing_slash(path))
      if (!dryrun) {
        dir.create(path, recursive = TRUE)
      }
    }
  }

  copy_file <- function(from, to) {
    message("Creating ", to)
    if (file.exists(to)) {
      stop(to, " already exists. Please remove it and try again.", call. = FALSE)
    }

    if (!dryrun) {
      is_template <- any(grepl("{{", readLines(from_file), fixed = TRUE))

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

  # Copy the files for a tests/ subdirectory
  copy_test_dir <- function(name) {
    tests_dir <- file.path(path, "tests")

    files <- dir(example_path("tests"), recursive = TRUE)
    # Note: This is not the same as using dir(pattern = "^shinytest"), since
    # that will not match files inside of shinytest/.
    files <- files[grepl(paste0("^", name), files)]

    # Filter out files that are not module files in the R directory.
    if (! "rdir" %in% examples) {
      # find all files in the testthat folder that are not module or server files
      is_r_folder_file <- (!grepl("module|server", basename(files))) & (dirname(files) == "testthat")
      files <- files[!is_r_folder_file]
    }

    # Filter out module files, if applicable.
    if (! "module" %in% examples) {
      files <- files[!grepl("module", files)]
    }

    mkdir(tests_dir)

    # Create any subdirectories if needed
    dirs <- setdiff(unique(dirname(files)), ".")
    for (dir in dirs) {
      mkdir(file.path(tests_dir, dir))
    }

    for (file in files) {
      copy_file(
        file.path(example_path("tests"), file),
        file.path(path, "tests", file)
      )
    }
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
        " is not empty. Do you want to create a Shiny app in this directory anyway? [y/n] "
      ))
      if (tolower(response) != "y") {
        return(invisible())
      }
    }
  } else {
    mkdir(path)
  }

  # app.R - If "app", populate with example; otherwise use empty file.
  if ("app" %in% examples) {
    copy_file(
      example_path("app.R"),
      file.path(path, "app.R")
    )
  }

  # R/ dir with non-module files
  r_dir <- file.path(path, "R")
  if ("rdir" %in% examples) {
    non_module_files <- dir(example_path("R"), pattern = "[^(module)].R$")
    mkdir(r_dir)
    for (file in non_module_files) {
      copy_file(
        example_path(file.path("R", file)),
        file.path(r_dir, file)
      )
    }
  }

  # R/ dir with module files
  if ("module" %in% examples) {
    module_files <- dir(example_path("R"), pattern = "module.R$")
    mkdir(r_dir)
    for (file in module_files) {
      copy_file(
        example_path(file.path("R", file)),
        file.path(r_dir, file)
      )
    }
  }

  # tests/ dir
  if ("shinytest" %in% examples) {
    copy_test_dir("shinytest")
  }
  if ("testthat" %in% examples) {
    copy_test_dir("testthat")
  }
  invisible()
}
