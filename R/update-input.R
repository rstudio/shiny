#' @export
updateTextInput <- function(session, inputId, value = NULL) {
  message <- list(value=value)
  # Drop nulls
  message <- message[!vapply(message, is.null, FUN.VALUE=logical(1))]
  
  sendInputMessage(session, inputId, message)
}

#' @export
updateCheckboxInput <- updateTextInput

#' @export
updateSliderInput <- updateTextInput

#' @export
updateTabsetInput <- updateTextInput

#' @export
updateNumberInput <- function(session, inputId, value = NULL, min = NULL,
    max = NULL, step = NULL) {

  message <- list(value=value, min=min, max=max, step=step)
  # Drop nulls
  message <- message[!vapply(message, is.null, FUN.VALUE=logical(1))]
  
  sendInputMessage(session, inputId, message)
}

#' @export
updateSelectInput <- function(session, inputId, value = NULL, options = NULL) {
  message <- list(value=value, options=options)
  # Drop nulls
  message <- message[!vapply(message, is.null, FUN.VALUE=logical(1))]

  sendInputMessage(session, inputId, message)
}

#' @export
updateRadioInput <- updateSelectInput

#' @export
updateCheckboxGroupInput <- updateRadioInput


sendInputMessage <- function(session, inputId, message) {
  session$send(
    type = "inputMessage",
    data = list(
      id = inputId,
      message = message
    )
  )
}
