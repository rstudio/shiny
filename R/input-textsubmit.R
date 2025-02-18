#' Create a textarea input control with explicit submission
#'
#' Creates a textarea input where users can enter multi-line text and submit
#' their input using a dedicated button or keyboard shortcut. This control is
#' ideal when you want to capture finalized input, rather than reacting to every
#' keystroke, making it useful for chat boxes, comments, or other scenarios
#' where users may compose and review their text before submitting.
#'
#' @inheritParams textInput
#' @param placeholder A character string giving the user a hint as to what can
#'   be entered into the control.
#' @param value The initial input text. Note that, unlike [textAreaInput()],
#'   this won't set a server-side value until the value is submitted.
#' @param button A [tags] element to use for the submit button. It's recommended
#'   that this be a [bslib::input_task_button()] since it will automatically
#'   provide a busy indicator (and disable) until the next flush occurs. Note
#'   also that if the submit button launches a [ExtendedTask], this button can
#'   also be bound to the task ([bslib::bind_task_button()]) and/or manually
#'   updated for more accurate progress reporting
#'   ([bslib::update_task_button()]).
#' @param submitKey A character string indicating what keyboard event should
#'   trigger the submit button. The default is `enter`, which will submit the
#'   input when the user presses the Enter/Return key. The `enter+modifier`
#'   option will submit the input when the user presses the Enter key while
#'   holding down Ctrl/Cmd.
#'
#' @return A textarea input control that can be added to a UI definition.
#'
#' @family input elements
#' @seealso [updateTextSubmitInput()], [textAreaInput()], [bslib::input_task_button()]
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
#'       req(input$text)
#'       Sys.sleep(2)
#'       paste("You entered:", input$text)
#'     })
#'   }
#'   shinyApp(ui, server)
#'
#' }
#'
#' @section Server value:
#' A character string of the text input. The default value is `""` even if
#' `value` is provided. The value will only be set/updated when the user submits
#' the input by pressing the Enter key or clicking the submit button.
#'
#' @export
textSubmitInput <- function(
  inputId,
  placeholder,
  value = "",
  ...,
  button = NULL,
  label = NULL,
  # TODO: should this default to something like min(500px, 100%)?
  # Or is that weird that it'd be a different width from other inputs?
  width = NULL,
  submitKey = c("enter", "enter+modifier")
) {
  rlang::check_dots_empty()

  value <- restoreInput(id = inputId, default = value)
  if (length(value) != 1 || !is.character(value)) {
    stop("`value` must be a character string", call. = FALSE)
  }

  submitKey <- rlang::arg_match(submitKey)
  needsModifier <- isTRUE(submitKey == "enter+modifier")

  if (is.null(button)) {
    if (needsModifier) {
      btn_label <- "Submit ⌘ ⏎"
      btn_title <- "Press ⌘ + Enter to Submit"
    } else {
      btn_label <- "Submit ⏎"
      btn_title <- "Press Enter to Submit"
    }

    button <- bslib::input_task_button(
      id = paste0(inputId, "-text-submit-input-button"),
      class = "btn-sm",
      label = btn_label,
      title = btn_title,
      `aria-label` = btn_title
    )
  }

  if (!is_button_tag(button)) {
    stop("`button` must be a `tags$button()`", call. = FALSE)
  }

  # A CSS class manages the overall disabled state. Part of why this approach
  # makes sense is because task buttons maintain their own "busy" state using
  # the disabled attribute, and we don't want to conflate the two.
  if (!nzchar(value)) {
    button <- tagAppendAttributes(button, class = "disabled")
  }

  div(
    class = "form-group shiny-input-container",
    shinyInputLabel(inputId, label),
    style = css(
      # TODO: validateCssUnit() needs to handle more complex CSS
      width = if (is.numeric(width)) paste0(width, "px") else width,
    ),
    div(
      class = "shiny-input-textsubmit",
      tags$textarea(
        id = inputId,
        class = "textarea-autoresize form-control",
        style = css(width = if (!is.null(width)) "100%"),
        placeholder = placeholder,
        `data-needs-modifier` = if (needsModifier) "",
        rows = 1,
        value
      ),
      button,
      autoresizeDeps()
    )
  )
}

is_button_tag <- function(x) {
  if (!inherits(x, "shiny.tag")) {
    return(FALSE)
  }

  isTRUE(x$name == "button") ||
    isTRUE(x$attribs$type == "button")
}

#' Change the value of a text submit input on the client
#'
#' @template update-input
#' @param value The value to set the user input to.
#' @param placeholder The placeholder text for the user input.
#' @param submit Whether to automatically submit the text for the user. Requires `value`.
#' @param focus Whether to move focus to the input element. Requires `value`.
#'
#' @seealso [textSubmitInput()]
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#'   ui <- fluidPage(
#'    sliderInput("controller", "Controller", 0, 20, 10),
#'    textSubmitInput("inText", "Input text"),
#'    textSubmitInput("inText2", "Input text 2")
#'   )
#'
#'   server <- function(input, output, session) {
#'    observe({
#'      # We'll use the input$controller variable multiple times, so save it as x
#'      # for convenience.
#'      x <- input$controller
#'
#'      # This will change the value of input$inText, based on x
#'      updateTextSubmitInput(session, "inText", value = paste("New text", x))
#'
#'      # Can also set the label, this time for input$inText2
#'      updateTextSubmitInput(
#'        session, "inText2",
#'        label = paste("New label", x),
#'        value = paste("New text", x)
#'       )
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
#'
#' @export
updateTextSubmitInput <- function(
  session,
  inputId,
  value = NULL,
  placeholder = NULL,
  label = NULL,
  submit = FALSE,
  focus = FALSE
) {

  validate_session_object(session)

  if (is.null(value) && (submit || focus)) {
    stop(
      "An input `value` must be provided when `submit` or `focus` are `TRUE`.",
      call. = FALSE
    )
  }

  message <- dropNulls(list(
    value = value,
    placeholder = placeholder,
    label = label,
    submit = submit,
    focus = focus
  ))

  session$sendInputMessage(inputId, message)
}
