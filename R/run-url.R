#' Run a Shiny application from https://gist.github.com
#'
#' Download and launch a Shiny application that is hosted on GitHub as a gist.
#'
#' @param gist The identifier of the gist. For example, if the gist is
#'   https://gist.github.com/jcheng5/3239667, then \code{3239667},
#'   \code{'3239667'}, and \code{'https://gist.github.com/jcheng5/3239667'}
#'   are all valid values.
#' @param port The TCP port that the application should listen on. Defaults to
#'   port 8100.
#' @param launch.browser If true, the system's default web browser will be
#'   launched automatically after the app is started. Defaults to true in
#'   interactive sessions only.
#'
#' @examples
#' \dontrun{
#' runGist(3239667)
#' runGist("https://gist.github.com/jcheng5/3239667")
#'
#' # Old URL format without username
#' runGist("https://gist.github.com/3239667")
#' }
#'
#' @export
runGist <- function(gist,
                    port=8100L,
                    launch.browser=getOption('shiny.launch.browser',
                                             interactive())) {

  gistUrl <- if (is.numeric(gist) || grepl('^[0-9a-f]+$', gist)) {
    sprintf('https://gist.github.com/%s/download', gist)
  } else if(grepl('^https://gist.github.com/([^/]+/)?([0-9a-f]+)$', gist)) {
    paste(gist, '/download', sep='')
  } else {
    stop('Unrecognized gist identifier format')
  }

  runUrl(gistUrl, filetype=".tar.gz", subdir=NULL, port=port,
    launch.browser=launch.browser)
}


#' Run a Shiny application from a GitHub repository
#'
#' Download and launch a Shiny application that is hosted in a GitHub repository.
#'
#' @param repo Name of the repository
#' @param username GitHub username
#' @param ref Desired git reference. Could be a commit, tag, or branch
#'   name. Defaults to \code{"master"}.
#' @param subdir A subdirectory in the repository that contains the app. By
#'   default, this function will run an app from the top level of the repo, but
#'   you can use a path such as `\code{"inst/shinyapp"}.
#' @param port The TCP port that the application should listen on. Defaults to
#'   port 8100.
#' @param launch.browser If true, the system's default web browser will be
#'   launched automatically after the app is started. Defaults to true in
#'   interactive sessions only.
#'
#' @examples
#' \dontrun{
#' runGitHub("shiny_example", "rstudio")
#'
#' # Can run an app from a subdirectory in the repo
#' runGitHub("shiny_example", "rstudio", subdir = "inst/shinyapp/")
#' }
#'
#' @export
runGitHub <- function(repo, username = getOption("github.user"),
  ref = "master", subdir = NULL, port = 8100,
  launch.browser = getOption('shiny.launch.browser', interactive())) {

  if (is.null(ref)) {
    stop("Must specify either a ref. ")
  }

  message("Downloading github repo(s) ",
    paste(repo, ref, sep = "/", collapse = ", "),
    " from ",
    paste(username, collapse = ", "))
  name <- paste(username, "-", repo, sep = "")

  url <- paste("https://github.com/", username, "/", repo, "/archive/",
    ref, ".tar.gz", sep = "")

  runUrl(url, subdir=subdir, port=port, launch.browser=launch.browser)
}


#' Run a Shiny application from a URL
#'
#' Download and launch a Shiny application that is hosted at a downloadable
#' URL. The Shiny application must be saved in a .zip, .tar, or .tar.gz file.
#' The Shiny application files must be contained in a subdirectory in the
#' archive. For example, the files might be \code{myapp/server.r} and
#' \code{myapp/ui.r}.
#'
#' @param url URL of the application.
#' @param filetype The file type (\code{".zip"}, \code{".tar"}, or
#'   \code{".tar.gz"}. Defaults to the file extension taken from the url.
#' @param subdir A subdirectory in the repository that contains the app. By
#'   default, this function will run an app from the top level of the repo, but
#'   you can use a path such as `\code{"inst/shinyapp"}.
#' @param port The TCP port that the application should listen on. Defaults to
#'   port 8100.
#' @param launch.browser If true, the system's default web browser will be
#'   launched automatically after the app is started. Defaults to true in
#'   interactive sessions only.
#'
#' @examples
#' \dontrun{
#' runUrl('https://github.com/rstudio/shiny_example/archive/master.tar.gz')
#'
#' # Can run an app from a subdirectory in the archive
#' runUrl("https://github.com/rstudio/shiny_example/archive/master.zip",
#'  subdir = "inst/shinyapp/")
#' }
#'
#' @export
runUrl <- function(url, filetype = NULL, subdir = NULL, port = 8100,
  launch.browser = getOption('shiny.launch.browser', interactive())) {

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
  filePath <- tempfile('shinyapp', fileext=fileext)
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
    dirname <- untar2(filePath, list=TRUE)[1]
    untar2(filePath, exdir = dirname(filePath))

  } else if (fileext == ".zip") {
    dirname <- as.character(unzip(filePath, list=TRUE)$Name[1])
    unzip(filePath, exdir = dirname(filePath))
  }

  appdir <- file.path(dirname(filePath), dirname)
  on.exit(unlink(appdir, recursive = TRUE), add = TRUE)

  appsubdir <- ifelse(is.null(subdir), appdir, file.path(appdir, subdir))
  runApp(appsubdir, port=port, launch.browser=launch.browser)
}
