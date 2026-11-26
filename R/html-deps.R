#' Create a web dependency
#'
#' Ensure that a file-based HTML dependency (from the htmltools package) can be
#' served over Shiny's HTTP server. This function works by using
#' [addResourcePath()] to map the HTML dependency's directory to a
#' URL.
#'
#' @param dependency A single HTML dependency object, created using
#'   [htmltools::htmlDependency()]. If the `src` value is named,
#'   then `href` and/or `file` names must be present.
#' @param scrubFile If TRUE (the default), remove `src$file` for the
#'   dependency. This prevents the local file path from being sent to the client
#'   when dynamic web dependencies are used. If FALSE, don't remove
#'   `src$file`. Setting it to FALSE should be needed only in very unusual
#'   cases.
#'
#' @return A single HTML dependency object that has an `href`-named element
#'   in its `src`.
#' @export
createWebDependency <- function(dependency, scrubFile = TRUE) {
  if (is.null(dependency))
    return(NULL)

  if (!inherits(dependency, "html_dependency"))
    stop("Unexpected non-html_dependency type")

  if (is.null(dependency$src$href)) {
    if (!is.null(dependency$package) && !is.null(dependency$src$file)) {
      dependency$src$file <- system.file(dependency$src$file, package = dependency$package)
    }
    prefix <- paste(dependency$name, "-", dependency$version, sep = "")
    addResourcePath(prefix, dependency$src$file)
    dependency$src$href <- prefix
  }

  # Don't leak local file path to client
  if (scrubFile)
    dependency$src$file <- NULL

  return(dependency)
}


# Given a Shiny tag object, process singletons and dependencies. Returns a list
# with rendered HTML and dependency objects.
# This implementation is very similar to renderTags(), but ignores
# <head> handling (it should only be used after the user session has started)
processDeps <- function(tags, session) {
  tags <- utils::getFromNamespace("tagify", "htmltools")(tags)
  ui <- takeSingletons(tags, session$singletons, desingleton = FALSE)$ui
  ui <- surroundSingletons(ui)
  dependencies <- lapply(
    resolveDependencies(findDependencies(ui, tagify = FALSE)),
    createWebDependency
  )
  names(dependencies) <- NULL

  list(
    html = doRenderTags(ui),
    deps = dependencies
  )
}
