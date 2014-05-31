createWebDependency <- function(dependency) {
  if (is.null(dependency))
    return(NULL)

  if (!inherits(dependency, "html_dependency"))
    stop("Unexpected non-html_dependency type")

  if (is.null(dependency$src$href)) {
    prefix <- paste(dependency$name, "-", dependency$version, sep = "")
    addResourcePath(prefix, dependency$src$file)
    dependency$src$href <- prefix
  }

  return(dependency)
}
