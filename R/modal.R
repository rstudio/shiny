#' Show a modal dialog
#'
#' @param ui UI content to show in the modal.
#' @param session The \code{session} object passed to function given to
#'   \code{shinyServer}.
#'
#' @seealso \code{\link{modalDialog}} for examples.
#' @export
showModal <- function(ui, session = getDefaultReactiveDomain()) {
  res <- processDeps(ui, session)

  session$sendModal("show",
    list(
      html = res$html,
      deps = res$deps
    )
  )

  invisible()
}

#' @rdname showModal
#' @export
removeModal <- function(session = getDefaultReactiveDomain()) {
  session$sendModal("remove", NULL)
}


#' Create a modal dialog UI
#'
#' @param ... UI elements for the body of the modal dialog box.
#' @param title An optional title for the dialog.
#' @param footer UI for footer. Use \coode{NULL} for no footer.
#' @param dismissable If \code{TRUE}, the modal dialog can be dismissed by
#'   clicking outside the dialog box, or be pressing the Escape key. If
#'   \code{FALSE} (the default), the modal dialog can't be dismissed in those
#'   ways; instead it must be dismissed by clicking on the dismiss button, or
#'   from a call to \code{\link{removeModal}} on the server.
#' @export
modalDialog <- function(..., title = NULL, footer = modalButton("Dismiss"),
                        dismissable = FALSE) {

  div(id = "shiny-modal", class = "modal fade", tabindex = "-1",
    `data-backdrop` = if (!dismissable) "static",
    `data-keyboard` = if (!dismissable) "false",

    div(class = "modal-dialog",
      div(class = "modal-content",
        if (!is.null(title)) div(class = "modal-header",
          tags$h4(class = "modal-title", title)
        ),
        div(class = "modal-body", ...),
        if (!is.null(footer)) div(class = "modal-footer", footer)
      )
    ),
    tags$script("$('#shiny-modal').modal().focus(); ")
  )
}

#' Create a button for a modal dialog
#'
#' When clicked, a \code{modalButton} will dismiss the modal dialog.
#'
#' @inheritParams actionButton
#' @seealso \code{\link{modalDialog}} for examples.
#' @export
modalButton <- function(label, icon = NULL) {
  tags$button(type = "button", class = "btn btn-default",
    `data-dismiss` = "modal", validateIcon(icon), label
  )
}
