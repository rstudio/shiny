#' Function to get the path of a Shiny example in a package
#'
#' @param package A character string specifying the name of the package
#' @param name A character string specifying the name of the example
#'
#' @return A character string specifying the path of the example
#' OR a data frame containing information about all available Shiny examples
#'
#' @noRd
run_example_app_dir <- function(package = NULL, name = NULL) {
    # arg checking
    if (is.null(package)) {
        if (is.null(name)) {
            # neither package nor name is specified
            return(available_examples(package = NULL))
        } else {
            stop("Please provide a package name
            when specifying an example name.")
        }
    } else {
        stopifnot(length(package) == 1 && is.character(package))
        # Search for a Shiny example with a given name in a package
        if (!is.null(name)) {
            examples <- available_examples(package = package)
            example_path <- examples[examples$name == name, "source_directory"]
            if (length(example_path) > 0) {
                return(example_path)
            } else {
                stop("No matching example found within the package")
            }
        }
        # only package is specified
        return(available_examples(package = package))
    }
}

#' Function to get a data frame of all available Shiny examples
#'
#' @param package A character string specifying the name of the package
#'
#' @return A data frame containing information
#' about all available Shiny examples
#'
#' @export
available_examples <- function(package = NULL) {
    info <-
        if (is.null(package)) {
            all_available_examples()
        } else {
            available_examples_for_package(package)
        }
    if (!is.null(info$error)) {
        stop(info$error, call. = FALSE)
    }
    examples <- info$examples
    return(examples)
}

#' Function to get a data frame of all available Shiny examples for a package
#'
#' @param package A character string specifying the name of the package
#'
#' @return A data frame containing information about all
#'  available Shiny examples for the package
#' @noRd
available_examples_for_package <- function(package) {
    an_error <- function(...) {
        list(
            examples = NULL,
            error = paste0(...)
        )
    }

    if (!nzchar(system.file(package = package))) {
        return(an_error(
            "No package found with name: \"", package, "\""
        ))
    }

    examples_dir <- system.file("shiny-examples", package = package)
    if (!nzchar(examples_dir)) {
        return(an_error(
            "No Shiny examples found for package: \"", package, "\""
        ))
    }

    example_folders <- list.dirs(
        examples_dir,
        full.names = TRUE,
        recursive = FALSE
    )
    names(example_folders) <- basename(example_folders)

    example_info <- lapply(example_folders, function(example_dir) {
        data.frame(
            package = package,
            name = basename(example_dir),
            source_directory = example_dir,
            stringsAsFactors = FALSE,
            row.names = FALSE
        )
    })

    examples <- do.call(rbind, example_info)
    class(examples) <- c("shiny_available_examples", class(examples))

    list(
        examples = examples,
        error = NULL
    )
}

#' Function to get a data frame of all available Shiny examples
#'  for all installed packages
#'
#' @return A data frame containing information about
#'  all available Shiny examples for all installed packages
#' @noRd
all_available_examples <- function() {
    ret <- list()
    all_pkgs <- installed.packages()[, "Package"]

    for (pkg in all_pkgs) {
        info <- available_examples_for_package(pkg)
        if (!is.null(info$examples)) {
            ret[[length(ret) + 1]] <- info$examples
        }
    }

    # combines the data frames into a single data frame
    examples <- do.call(rbind, ret)

    list(
        examples = examples, # will maintain class
        error = NULL
    )
}

#' @export
format.shiny_available_examples <- function(x, ...) {
    examples <- x
    split_examples <- split(examples, examples$package)

    pkg_examples <- vapply(
        split_examples,
        function(examples_sub) {
            paste0(
                "* ", examples_sub$package[1], "\n",
                paste0("  - ", examples_sub$name, collapse = "\n")
            )
        },
        character(1)
    )

    paste0(
        "Available Shiny examples:\n",
        paste0(pkg_examples, collapse = "\n")
    )
}

#' @export
print.shiny_available_examples <- function(x, ...) {
    cat(format(x, ...), "\n", sep = "")
}

# run_example_app_dir()
# run_example_app_dir(package = "shiny")
# run_example_app_dir(package = "tidyverse")

# run_example_app_dir(package = c("shiny", "tidyverse"))
# run_example_app_dir(package = "shuny")
# run_example_app_dir(name = "01_hello")
# run_example_app_dir(package = "shiny", name = "01_hello")
# run_example_app_dir(package = "shiny", name = "05_hello")
