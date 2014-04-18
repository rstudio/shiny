

# Define an HTML dependency
#
# Define an HTML dependency (e.g. CSS or Javascript and related library). HTML
# dependency definitions are required for \code{\link{html_output}} that
# require CSS or JavaScript within the document head to render correctly.
#
# @param name Library name
# @param version Library version
# @param path Full path to library
# @param meta Named list of meta tags to insert into document head
# @param script Script(s) to include within the document head (should be
#   specified relative to the \code{path} parameter).
# @param stylesheet Stylesheet(s) to include within the document (should be
#   specified relative to the \code{path} parameter).
# @param head Arbitrary lines of HTML to insert into the document head
#
# @return An object that can be included in the list of dependencies passed to
#   \code{\link{html_print}} or \code{\link{html_knit_print}}.
#
# @details See the documentation on
#   \href{http://rmarkdown.rstudio.com/developer_html_widgets.html}{R
#   Markdown HTML Widgets} for examples and additional details.
#
html_dependency <- function(name,
  version,
  path,
  meta = NULL,
  script = NULL,
  stylesheet = NULL,
  head = NULL) {
  structure(class = "html_dependency", list(
    name = name,
    version = version,
    path = path,
    meta = meta,
    script = script,
    stylesheet = stylesheet,
    head = head
  ))
}


# Given a list of HTML dependencies produce a character representation
# suitable for inclusion within the head of an HTML document
html_dependencies_as_character <- function(dependencies, lib_dir = NULL) {

  html <- c()

  for (dep in dependencies) {

    # copy library files if necessary
    if (!is.null(lib_dir)) {

      if (!file.exists(lib_dir))
        dir.create(lib_dir)

      target_dir <- file.path(lib_dir, basename(dep$path))
      if (!file.exists(target_dir))
        file.copy(from = dep$path, to = lib_dir, recursive = TRUE)

      dep$path <- file.path(basename(lib_dir), basename(target_dir))
    }

    # add meta content
    for (name in names(dep$meta)) {
      html <- c(html, paste("<meta name=\"", name,
        "\" content=\"", dep$meta[[name]], "\" />",
        sep = ""))
    }

    # add stylesheets
    for (stylesheet in dep$stylesheet) {
      stylesheet <- file.path(dep$path, stylesheet)
      html <- c(html, paste("<link href=\"", stylesheet, "\" ",
        "rel=\"stylesheet\" />",
        sep = ""))
    }

    # add scripts
    for (script in dep$script) {
      script <- file.path(dep$path, script)
      html <- c(html,
        paste("<script src=\"", script, "\"></script>", sep = ""))
    }

    # add raw head content
    html <- c(html, dep$head)
  }

  html
}

attach_dependency <- function(x, dependency) {
  structure(x, html_dependency = dependency)
}
