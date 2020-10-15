.globals$resourcePaths <- list()
.globals$resources <- list()

#' Resource Publishing
#'
#' Add, remove, or list directory of static resources to Shiny's web server,
#' with the given path prefix. Primarily intended for package authors to make
#' supporting JavaScript/CSS files available to their components.
#'
#' Shiny provides two ways of serving static files (i.e., resources):
#'
#' 1. Static files under the `www/` directory are automatically made available
#' under a request path that begins with `/`.
#' 2. `addResourcePath()` makes static files in a `directoryPath` available
#' under a request path that begins with `prefix`.
#'
#' The second approach is primarily intended for package authors to make
#' supporting JavaScript/CSS files available to their components.
#'
#' Tools for managing static resources published by Shiny's web server:
#'  * `addResourcePath()` adds a directory of static resources.
#'  * `resourcePaths()` lists the currently active resource mappings.
#'  * `removeResourcePath()` removes a directory of static resources.
#'
#' @param prefix The URL prefix (without slashes). Valid characters are a-z,
#'   A-Z, 0-9, hyphen, period, and underscore. For example, a value of 'foo'
#'   means that any request paths that begin with '/foo' will be mapped to the
#'   given directory.
#' @param directoryPath The directory that contains the static resources to be
#'   served.
#'
#' @rdname resourcePaths
#' @seealso [singleton()]
#'
#' @examples
#' addResourcePath('datasets', system.file('data', package='datasets'))
#' resourcePaths()
#' removeResourcePath('datasets')
#' resourcePaths()
#'
#' # make sure all resources are removed
#' lapply(names(resourcePaths()), removeResourcePath)
#' @export
addResourcePath <- function(prefix, directoryPath) {
  if (length(prefix) != 1) stop("prefix must be of length 1")
  if (grepl("^\\.+$", prefix)) stop("prefix can't be composed of dots only")
  if (!grepl('[a-z0-9\\-_.]+$', prefix, ignore.case = TRUE, perl = TRUE)) {
    stop("addResourcePath called with invalid prefix; please see documentation")
  }
  if (prefix %in% c('shared')) {
    stop("addResourcePath called with the reserved prefix '", prefix, "'; ",
         "please use a different prefix")
  }
  normalizedPath <- tryCatch(normalizePath(directoryPath, mustWork = TRUE),
    error = function(e) {
      stop("Couldn't normalize path in `addResourcePath`, with arguments: ",
        "`prefix` = '", prefix, "'; `directoryPath` = '" , directoryPath, "'")
    }
  )

  # # Often times overwriting a resource path is "what you want",
  # # but sometimes it can lead to difficult to diagnose issues
  # # (e.g. an implict dependency might set a resource path that
  # # conflicts with what you, the app author, are trying to register)
  # # Note that previous versions of shiny used to warn about this case,
  # # but it was eventually removed since it caused confusion (#567).
  # # It seems a good compromise is to throw a more information message.
  # if (getOption("shiny.resourcePathChanges", FALSE) &&
  #     prefix %in% names(.globals$resourcePaths)) {
  #   existingPath <- .globals$resourcePaths[[prefix]]$path
  #   if (normalizedPath != existingPath) {
  #     message(
  #       "The resource path '", prefix, "' used to point to ",
  #       existingPath, ", but it now points to ", normalizedPath, ". ",
  #       "If your app doesn't work as expected, you may want to ",
  #       "choose a different prefix name."
  #     )
  #   }
  # }

  # If a shiny app is currently running, dynamically register this path with
  # the corresponding httpuv server object.
  if (!is.null(getShinyOption("server", default = NULL)))
  {
    getShinyOption("server")$setStaticPath(.list = stats::setNames(normalizedPath, prefix))
  }

  # .globals$resourcePaths and .globals$resources persist across runs of applications.
  .globals$resourcePaths[[prefix]] <- staticPath(normalizedPath)
  # This is necessary because resourcePaths is only for serving assets out of C++;
  # to support subapps, we also need assets to be served out of R, because those
  # URLs are rewritten by R code (i.e. routeHandler) before they can be matched to
  # a resource path.
  .globals$resources[[prefix]] <- list(
    directoryPath = normalizedPath,
    func = staticHandler(normalizedPath)
  )
}

#' @rdname resourcePaths
#' @export
resourcePaths <- function() {
  urls <- names(.globals$resourcePaths)
  paths <- vapply(.globals$resourcePaths, function(x) x$path, character(1))
  stats::setNames(paths, urls)
}

hasResourcePath <- function(prefix) {
  prefix %in% names(resourcePaths())
}

#' @rdname resourcePaths
#' @export
removeResourcePath <- function(prefix) {
  if (length(prefix) > 1) stop("`prefix` must be of length 1.")
  if (!hasResourcePath(prefix)) {
    warning("Resource ", prefix, " not found.")
    return(invisible(FALSE))
  }
  .globals$resourcePaths[[prefix]] <- NULL
  .globals$resources[[prefix]] <- NULL
  invisible(TRUE)
}

# This function handles any GET request with two or more path elements where the
# first path element matches a prefix that was previously added using
# addResourcePath().
#
# For example, if `addResourcePath("foo", "~/bar")` was called, then a GET
# request for /foo/one/two.html would rewrite the PATH_INFO as /one/two.html and
# send it to the resource path function for "foo". As of this writing, that
# function will always be a staticHandler, which serves up a file if it exists
# and NULL if it does not.
#
# Since Shiny 1.3.x, assets registered via addResourcePath should mostly be
# served out of httpuv's native static file serving features. However, in the
# specific case of subapps, the R code path must be used, because subapps insert
# a giant random ID into the beginning of the URL that must be stripped off by
# an R route handler (see addSubApp()).
resourcePathHandler <- function(req) {
  if (!identical(req$REQUEST_METHOD, 'GET'))
    return(NULL)

  # e.g. "/foo/one/two.html"
  path <- req$PATH_INFO

  match <- regexpr('^/([^/]+)/', path, perl=TRUE)
  if (match == -1)
    return(NULL)
  len <- attr(match, 'capture.length')
  # e.g. "foo"
  prefix <- substr(path, 2, 2 + len - 1)

  resInfo <- .globals$resources[[prefix]]
  if (is.null(resInfo))
    return(NULL)

  # e.g. "/one/two.html"
  suffix <- substr(path, 2 + len, nchar(path))

  # Create a new request that's a clone of the current request, but adjust
  # PATH_INFO and SCRIPT_NAME to reflect that we have already matched the first
  # path element (e.g. "/foo"). See routeHandler() for more info.
  subreq <- as.environment(as.list(req, all.names=TRUE))
  subreq$PATH_INFO <- suffix
  subreq$SCRIPT_NAME <- paste(subreq$SCRIPT_NAME, substr(path, 1, 2 + len), sep='')

  return(resInfo$func(subreq))
}
