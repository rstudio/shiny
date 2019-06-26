#' Run a Shiny application from a URL
#'
#' `runUrl()` downloads and launches a Shiny application that is hosted at
#' a downloadable URL. The Shiny application must be saved in a .zip, .tar, or
#' .tar.gz file. The Shiny application files must be contained in the root
#' directory or a subdirectory in the archive. For example, the files might be
#' `myapp/server.r` and `myapp/ui.r`. The functions `runGitHub()`
#' and `runGist()` are based on `runUrl()`, using URL's from GitHub
#' (<https://github.com>) and GitHub gists (<https://gist.github.com>),
#' respectively.
#' @param url URL of the application.
#' @param filetype The file type (`".zip"`, `".tar"`, or
#'   `".tar.gz"`. Defaults to the file extension taken from the url.
#' @param subdir A subdirectory in the repository that contains the app. By
#'   default, this function will run an app from the top level of the repo, but
#'   you can use a path such as `"inst/shinyapp"`.
#' @param destdir Directory to store the downloaded application files. If `NULL`
#'   (the default), the application files will be stored in a temporary directory
#'   and removed when the app exits
#' @param ... Other arguments to be passed to [runApp()], such as
#'   `port` and `launch.browser`.
#' @export
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'   runUrl('https://github.com/rstudio/shiny_example/archive/master.tar.gz')
#'
#'   # Can run an app from a subdirectory in the archive
#'   runUrl("https://github.com/rstudio/shiny_example/archive/master.zip",
#'     subdir = "inst/shinyapp/")
#' }
runUrl <- function(url, filetype = NULL, subdir = NULL, destdir = NULL, ...) {

  if (!is.null(subdir) && ".." %in% strsplit(subdir, '/')[[1]])
    stop("'..' not allowed in subdir")

  if (is.null(filetype))
    filetype <- basename(url)

  if (grepl("\\.tar\\.gz$", filetype))
    fileext <- ".tar.gz"
  else if (grepl("\\.tar$", filetype))
    fileext <- ".tar"
  else if (grepl("\\.zip$", filetype))
    fileext <- ".zip"
  else
    stop("Unknown file extension.")

  message("Downloading ", url)
  if (is.null(destdir)) {
    filePath <- tempfile('shinyapp', fileext = fileext)
    fileDir  <- tempfile('shinyapp')
  } else {
    fileDir <- destdir
    filePath <- paste(destdir, fileext)
  }

  dir.create(fileDir, showWarnings = FALSE)
  if (download(url, filePath, mode = "wb", quiet = TRUE) != 0)
    stop("Failed to download URL ", url)
  on.exit(unlink(filePath))

  if (fileext %in% c(".tar", ".tar.gz")) {
    # Regular untar commonly causes two problems on Windows with github tarballs:
    #   1) If RTools' tar.exe is in the path, you get cygwin path warnings which
    #      throw list=TRUE off;
    #   2) If the internal untar implementation is used, it chokes on the 'g'
    #      type flag that github uses (to stash their commit hash info).
    # By using our own forked/modified untar2 we sidestep both issues.
    first <- untar2(filePath, list=TRUE)[1]
    untar2(filePath, exdir = fileDir)

  } else if (fileext == ".zip") {
    first <- as.character(utils::unzip(filePath, list=TRUE)$Name)[1]
    utils::unzip(filePath, exdir = fileDir)
  }

  if(is.null(destdir)){
    on.exit(unlink(fileDir, recursive = TRUE), add = TRUE)
  }

  appdir <- file.path(fileDir, first)
  if (!utils::file_test('-d', appdir)) appdir <- dirname(appdir)

  if (!is.null(subdir)) appdir <- file.path(appdir, subdir)
  runApp(appdir, ...)
}

#' @rdname runUrl
#' @param gist The identifier of the gist. For example, if the gist is
#'   https://gist.github.com/jcheng5/3239667, then `3239667`,
#'   `'3239667'`, and `'https://gist.github.com/jcheng5/3239667'` are
#'   all valid values.
#' @export
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'   runGist(3239667)
#'   runGist("https://gist.github.com/jcheng5/3239667")
#'
#'   # Old URL format without username
#'   runGist("https://gist.github.com/3239667")
#' }
#'
runGist <- function(gist, destdir = NULL, ...) {

  gistUrl <- if (is.numeric(gist) || grepl('^[0-9a-f]+$', gist)) {
    sprintf('https://gist.github.com/%s/download', gist)
  } else if(grepl('^https://gist.github.com/([^/]+/)?([0-9a-f]+)$', gist)) {
    paste(gist, '/download', sep='')
  } else {
    stop('Unrecognized gist identifier format')
  }

  runUrl(gistUrl, filetype = ".zip", destdir = destdir, ...)
}


#' @rdname runUrl
#' @param repo Name of the repository.
#' @param username GitHub username. If `repo` is of the form
#'   `"username/repo"`, `username` will be taken from `repo`.
#' @param ref Desired git reference. Could be a commit, tag, or branch name.
#'   Defaults to `"master"`.
#' @export
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'   runGitHub("shiny_example", "rstudio")
#'   # or runGitHub("rstudio/shiny_example")
#'
#'   # Can run an app from a subdirectory in the repo
#'   runGitHub("shiny_example", "rstudio", subdir = "inst/shinyapp/")
#' }
runGitHub <- function(repo, username = getOption("github.user"),
                      ref = "master", subdir = NULL, destdir = NULL, ...) {

  if (grepl('/', repo)) {
    res <- strsplit(repo, '/')[[1]]
    if (length(res) != 2) stop("'repo' must be of the form 'username/repo'")
    username <- res[1]
    repo     <- res[2]
  }

  url <- paste("https://github.com/", username, "/", repo, "/archive/",
               ref, ".tar.gz", sep = "")

  runUrl(url, subdir = subdir, destdir = destdir, ...)
}
