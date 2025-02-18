#' Create a textarea input control with a submit button
#'
#' Create a textarea input control for entry of unstructured text values.
#' The input includes a submit button that allows users to explicitly submit their input.
#' This is useful for scenarios where you want to capture the final input rather than reacting to every keystroke.
#'
#' @inheritParams textInput
#' @param placeholder A character string giving the user a hint as to what can be
#'  entered into the control.
#' @param value The initial input text. Note that, unlike [textAreaInput()], this
#'   won't set a server-side value until the value is submitted.
#' @param button A [tags] element to use for the submit button. It's recommended
#'   that this be a [bslib::input_task_button()] since it will automatically provide
#'   a busy indicator (and disable) until the next flush occurs. Note also that if
#'   the submit button launches a [ExtendedTask], this button can also be bound to
#'   the task ([bslib::bind_task_button()]) and/or manually updated for more
#'   accurate progress reporting ([bslib::update_task_button()]).
#' @param submitKey A character string indicating what keyboard event should trigger
#'   the submit button. The default is `enter`, which will submit the input when
#'   the user presses the Enter/Return key. The `enter+modifier` option will submit the
#'   input when the user presses the Enter key while holding down Ctrl/Cmd.
#'
#' @return A textarea input control that can be added to a UI definition.
#'
#' @family input elements
#' @seealso [updateTextAreaInput()]
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#'   ui <- fluidPage(
#'     textSubmitInput("text", "Enter some input..."),
#'     verbatimTextOutput("value")
#'   )
#'   server <- function(input, output) {
#'     output$value <- renderText({
#'         req(input$text)
#'         Sys.sleep(2)
#'         paste("You entered:", input$text)
#'     })
#'   }
#'   shinyApp(ui, server)
#'
#' }
#'
#' @section Server value:
#' A character string of the text input. The default value is `""`
#' unless `value` is provided.
#'
#' @export
textSubmitInput <- function(
  inputId,
  placeholder,
  value = "",
  ...,
  button = NULL,
  # TODO: should this default to something like min(500px, 100%)? Is that weird that it'd
  # be a different width from textAreaInput()?
  width = NULL,
  submitKey = c("enter", "enter+modifier")
) {
  rlang::check_dots_empty()
  submitKey <- rlang::arg_match(submitKey)
  needsModifier <- isTRUE(submitKey == "enter+modifier")


  if (is.null(button)) {
    if (needsModifier) {
      label <- "Submit ⌘ ⏎"
      title <- "Press ⌘ + Enter to Submit"
    } else {
      label <- "Submit ⏎"
      title <- "Press Enter to Submit"
    }

    button <- bslib::input_task_button(
      id = paste0(inputId, "-text-submit-input-button"),
      class = "btn-sm",
      label = label,
      title = title,
      "aria-label" = title,
    )
  }

  if (!is_button_tag(button)) {
    stop("`button` must be a `tags$button()`", call. = FALSE)
  }

  value <- restoreInput(id = inputId, default = value)

  div(
    class = "form-group shiny-input-container shiny-input-textsubmit",
    # validateCssUnit() really needs to handle more complex CSS
    style = css(
      width = if (is.numeric(width)) paste0(width, "px") else width,
    ),
    tags$textarea(
      id = inputId,
      class = "textarea-autoresize form-control",
      "data-needs-modifier" = if (needsModifier) "",
      style = css(width = if (!is.null(width)) "100%"),
      placeholder = placeholder,
      rows = 1,
      value
    ),
    button,
    autoresizeDeps()
  )
}

is_button_tag <- function(x) {
  if (!inherits(x, "shiny.tag")) {
    return(FALSE)
  }

  isTRUE(x$name == "button") ||
    isTRUE(x$attribs$type == "button")
}
