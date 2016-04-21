#' Show or remove a modal dialog
#'
#' This causes a modal dialog to be displayed in the client browser, and is
#' typically used with \code{\link{modalDialog}}.
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
}

#' @rdname showModal
#' @export
removeModal <- function(session = getDefaultReactiveDomain()) {
  session$sendModal("remove", NULL)
}


#' Create a modal dialog UI
#'
#' This creates the UI for a modal dialog, using Bootstrap's modal class. Modals
#' are typically used for showing important messages, or for presenting UI that
#' requires input from the user, such as a username and password input.
#'
#' @param ... UI elements for the body of the modal dialog box.
#' @param title An optional title for the dialog.
#' @param footer UI for footer. Use \code{NULL} for no footer.
#' @param easyClose If \code{TRUE}, the modal dialog can be dismissed by
#'   clicking outside the dialog box, or be pressing the Escape key. If
#'   \code{FALSE} (the default), the modal dialog can't be dismissed in those
#'   ways; instead it must be dismissed by clicking on the dismiss button, or
#'   from a call to \code{\link{removeModal}} on the server.
#'
#' @examples
#' if (interactive()) {
#' # Display an important message that can be dismissed only by clicking the
#' # dismiss button.
#' shinyApp(
#'   ui = basicPage(
#'     actionButton("show", "Show modal dialog")
#'   ),
#'   server = function(input, output) {
#'     observeEvent(input$show, {
#'       showModal(modalDialog(
#'         title = "Important message",
#'         "This is an important message!"
#'       ))
#'     })
#'   }
#' )
#'
#'
#' # Display a message that can be dismissed by clicking outside the modal dialog,
#' # or by pressing Esc.
#' shinyApp(
#'   ui = basicPage(
#'     actionButton("show", "Show modal dialog")
#'   ),
#'   server = function(input, output) {
#'     observeEvent(input$show, {
#'       showModal(modalDialog(
#'         title = "Somewhat important message",
#'         "This is a somewhat important message.",
#'         easyClose = TRUE,
#'         footer = NULL
#'       ))
#'     })
#'   }
#' )
#'
#'
#' # Display a modal that requires valid username and password input.
#' shinyApp(
#'   ui = basicPage(
#'     actionButton("show", "Show modal dialog"),
#'     verbatimTextOutput("loginInfo")
#'   ),
#'   server = function(input, output) {
#'     # A string with the current login status. This is in a reactiveValues
#'     # object so that it can trigger reactivity.
#'     vals <- reactiveValues(loginStatus = "Not logged in.")
#'
#'     # Attempt logging in with a username and password, returning TRUE if
#'     # successful and FALSE if not.
#'     login <- function(username, password) {
#'       # In a real-world use case, this would check against some sort of user
#'       # database instead of just checking that the values are identical to
#'       # hard-coded values.
#'       if (identical(username, "user1") && identical(password, "pass1")) {
#'         vals$loginStatus <- paste0('Logged in as "', username, '"')
#'         return(TRUE)
#'
#'       } else {
#'         vals$loginStatus <- "Not logged in."
#'         return(FALSE)
#'       }
#'     }
#'
#'     # Return the UI for a modal dialog with username/password inputs.
#'     # If 'failed' is TRUE, then display a message that the previous username
#'     # and password were invalid.
#'     loginModal <- function(failed = FALSE) {
#'       modalDialog(
#'         textInput("username", "Username"),
#'         passwordInput("password", "Password"),
#'         span('(Try logging in with "user1" and "pass1")'),
#'         if (failed)
#'           div(tags$b("Invalid username/password", style = "color: red;")),
#'
#'         footer = tagList(
#'           modalButton("Cancel"),
#'           actionButton("login", "Log in")
#'         )
#'       )
#'     }
#'
#'     observeEvent(input$show, {
#'       showModal(loginModal())
#'     })
#'
#'     # When login button is pressed, attempt to log in. If successful, remove the
#'     # modal. If not show another modal, but this time with a failure message.
#'     observeEvent(input$login, {
#'       if (login(input$username, input$password)) {
#'         removeModal()
#'       } else {
#'         showModal(loginModal(failed = TRUE))
#'       }
#'     })
#'
#'     # Display current login status
#'     output$loginInfo <- renderText({
#'       vals$loginStatus
#'     })
#'   }
#' )
#'
#' }
#' @export
modalDialog <- function(..., title = NULL, footer = modalButton("Dismiss"),
                        easyClose = FALSE) {

  div(id = "shiny-modal", class = "modal fade", tabindex = "-1",
    `data-backdrop` = if (!easyClose) "static",
    `data-keyboard` = if (!easyClose) "false",

    div(class = "modal-dialog",
      div(class = "modal-content",
        if (!is.null(title)) div(class = "modal-header",
          tags$h4(class = "modal-title", title)
        ),
        div(class = "modal-body", ...),
        if (!is.null(footer)) div(class = "modal-footer", footer)
      )
    ),
    tags$script("$('#shiny-modal').modal().focus();")
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
