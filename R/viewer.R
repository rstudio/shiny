
#' Viewer options
#'
#' Use these functions to control where the gadget is displayed in RStudio (or
#' other R environments that emulate RStudio's viewer pane/dialog APIs). If
#' viewer APIs are not available in the current R environment, then the gadget
#' will be displayed in the system's default web browser (see
#' [utils::browseURL()]).
#'
#' @return A function that takes a single `url` parameter, suitable for
#'   passing as the `viewer` argument of [runGadget()].
#'
#' @rdname viewer
#' @name viewer
NULL

#' @param minHeight The minimum height (in pixels) desired to show the gadget in
#'   the viewer pane. If a positive number, resize the pane if necessary to show
#'   at least that many pixels. If `NULL`, use the existing viewer pane
#'   size. If `"maximize"`, use the maximum available vertical space.
#' @rdname viewer
#' @export
paneViewer <- function(minHeight = NULL) {
  viewer <- getOption("viewer")
  if (is.null(viewer)) {
    utils::browseURL
  } else {
    function(url) {
      viewer(url, minHeight)
    }
  }
}

#' @param dialogName The window title to display for the dialog.
#' @param width,height The desired dialog width/height, in pixels.
#' @rdname viewer
#' @export
dialogViewer <- function(dialogName, width = 600, height = 600) {
  viewer <- getOption("shinygadgets.showdialog")
  if (is.null(viewer)) {
    utils::browseURL
  } else {
    function(url) {
      viewer(dialogName, url, width = width, height = height)
    }
  }
}

#' @param browser See [utils::browseURL()].
#' @rdname viewer
#' @export
browserViewer <- function(browser = getOption("browser")) {
  function(url) {
    utils::browseURL(url, browser = browser)
  }
}
