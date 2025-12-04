#' Show or remove a modal dialog
#'
#' This causes a modal dialog to be displayed in the client browser, and is
#' typically used with [modalDialog()].
#'
#' @param ui UI content to show in the modal.
#' @param session The `session` object passed to function given to
#'   `shinyServer`.
#'
#' @seealso [modalDialog()] for examples.
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
#' @description
#' `modalDialog()` creates the UI for a modal dialog, using Bootstrap's modal
#' class. Modals are typically used for showing important messages, or for
#' presenting UI that requires input from the user, such as a user name and
#' password input.
#'
#' `modalButton()` creates a button that will dismiss the dialog when clicked,
#' typically used when customising the `footer`.
#'
#' @inheritParams actionButton
#' @param ... UI elements for the body of the modal dialog box.
#' @param title An optional title for the dialog.
#` @param title_level An optional header level (between 1 and 6) for the title. 
#'   Defaults to 4 (h4 element).
#' @param footer UI for footer. Use `NULL` for no footer.
#' @param size One of `"s"` for small, `"m"` (the default) for medium,
#'   `"l"` for large, or `"xl"` for extra large. Note that `"xl"` only 
#'    works with Bootstrap 4 and above (to opt-in to Bootstrap 4+, 
#'    pass [bslib::bs_theme()] to the `theme` argument of a page container 
#'    like [fluidPage()]).
#' @param easyClose If `TRUE`, the modal dialog can be dismissed by
#'   clicking outside the dialog box, or be pressing the Escape key. If
#'   `FALSE` (the default), the modal dialog can't be dismissed in those
#'   ways; instead it must be dismissed by clicking on a `modalButton()`, or
#'   from a call to [removeModal()] on the server.
#' @param fade If `FALSE`, the modal dialog will have no fade-in animation
#'   (it will simply appear rather than fade in to view).
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
#' # Display a modal that requires valid input before continuing.
#' shinyApp(
#'   ui = basicPage(
#'     actionButton("show", "Show modal dialog"),
#'     verbatimTextOutput("dataInfo")
#'   ),
#'
#'   server = function(input, output) {
#'     # reactiveValues object for storing current data set.
#'     vals <- reactiveValues(data = NULL)
#'
#'     # Return the UI for a modal dialog with data selection input. If 'failed' is
#'     # TRUE, then display a message that the previous value was invalid.
#'     dataModal <- function(failed = FALSE) {
#'       modalDialog(
#'         textInput("dataset", "Choose data set",
#'           placeholder = 'Try "mtcars" or "abc"'
#'         ),
#'         span('(Try the name of a valid data object like "mtcars", ',
#'              'then a name of a non-existent object like "abc")'),
#'         if (failed)
#'           div(tags$b("Invalid name of data object", style = "color: red;")),
#'
#'         footer = tagList(
#'           modalButton("Cancel"),
#'           actionButton("ok", "OK")
#'         )
#'       )
#'     }
#'
#'     # Show modal when button is clicked.
#'     observeEvent(input$show, {
#'       showModal(dataModal())
#'     })
#'
#'     # When OK button is pressed, attempt to load the data set. If successful,
#'     # remove the modal. If not show another modal, but this time with a failure
#'     # message.
#'     observeEvent(input$ok, {
#'       # Check that data object exists and is data frame.
#'       if (!is.null(input$dataset) && nzchar(input$dataset) &&
#'           exists(input$dataset) && is.data.frame(get(input$dataset))) {
#'         vals$data <- get(input$dataset)
#'         removeModal()
#'       } else {
#'         showModal(dataModal(failed = TRUE))
#'       }
#'     })
#'
#'     # Display information about selected data
#'     output$dataInfo <- renderPrint({
#'       if (is.null(vals$data))
#'         "No data selected"
#'       else
#'         summary(vals$data)
#'     })
#'   }
#' )
#' }
#' @export
modalDialog <- function(..., title = NULL, title_level = 4, footer = modalButton("Dismiss"),
  size = c("m", "s", "l", "xl"), easyClose = FALSE, fade = TRUE) {

if (!is.numeric(title_level) || length(title_level) != 1 || is.na(title_level)) {
  stop("`title_level` must be a single numeric value between 1 and 6.")
}

if (title_level %% 1 != 0) {
  stop("`title_level` must be an integer between 1 and 6.")
}

if (title_level < 1 || title_level > 6) {
  stop("`title_level` must be a value 1 through 6.")
}

  size <- match.arg(size)

  backdrop <- if (!easyClose) "static"
  keyboard <- if (!easyClose) "false"
  div(
    id = "shiny-modal",
    class = "modal",
    class = if (fade) "fade",
    tabindex = "-1",
    `data-backdrop` = backdrop,
    `data-bs-backdrop` = backdrop,
    `data-keyboard` = keyboard,
    `data-bs-keyboard` = keyboard,

    div(
      class = "modal-dialog",
      class = switch(size, s = "modal-sm", m = NULL, l = "modal-lg", xl = "modal-xl"),
      div(class = "modal-content",
        if (!is.null(title)) div(class = "modal-header",
          tag_fun = htmltools::tags[[paste0("h", title_level)]] #NOW, GRAB HEADER LEVEL ACCORDING TO USER SPEC
          tag_fun(class = "modal-title", title)
        ),
        div(class = "modal-body", ...),
        if (!is.null(footer)) div(class = "modal-footer", footer)
      )
    ),
    # jQuery plugin doesn't work in Bootstrap 5, but vanilla JS doesn't work in Bootstrap 4 :sob:
    tags$script(HTML(
      "if (window.bootstrap && !window.bootstrap.Modal.VERSION.match(/^4\\./)) {
         var modal = new bootstrap.Modal(document.getElementById('shiny-modal'));
         modal.show();
      } else {
         $('#shiny-modal').modal().focus();
      }"
    ))
  )
}

#' @export
#' @rdname modalDialog
modalButton <- function(label, icon = NULL) {
  tags$button(
    type = "button",
    class = "btn btn-default",
    `data-dismiss` = "modal",
    `data-bs-dismiss` = "modal",
    validateIcon(icon), label
  )
}
