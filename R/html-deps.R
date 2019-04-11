#' Create a web dependency
#'
#' Ensure that a file-based HTML dependency (from the htmltools package) can be
#' served over Shiny's HTTP server. This function works by using
#' \code{\link{addResourcePath}} to map the HTML dependency's directory to a
#' URL.
#'
#' @param dependency A single HTML dependency object, created using
#'   \code{\link[htmltools]{htmlDependency}}. If the \code{src} value is named,
#'   then \code{href} and/or \code{file} names must be present.
#' @param scrubFile If TRUE (the default), remove \code{src$file} for the
#'   dependency. This prevents the local file path from being sent to the client
#'   when dynamic web dependencies are used. If FALSE, don't remove
#'   \code{src$file}. Setting it to FALSE should be needed only in very unusual
#'   cases.
#'
#' @return A single HTML dependency object that has an \code{href}-named element
#'   in its \code{src}.
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
processDeps <- function(tags, session) {
  ui <- takeSingletons(tags, session$singletons, desingleton=FALSE)$ui
  ui <- surroundSingletons(ui)
  dependencies <- lapply(
    resolveDependencies(findDependencies(ui)),
    createWebDependency
  )
  names(dependencies) <- NULL

  list(
    html = doRenderTags(ui),
    deps = dependencies
  )
}
