#' @export
updateTextInput <- function(session, inputId, label = NULL, value = NULL) {
  message <- list(label=label, value=value)
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
updateNumberInput <- function(session, inputId, label = NULL, value = NULL,
    min = NULL, max = NULL, step = NULL) {

  message <- list(label=label, value=value, min=min, max=max, step=step)
  # Drop nulls
  message <- message[!vapply(message, is.null, FUN.VALUE=logical(1))]
  
  sendInputMessage(session, inputId, message)
}

#' @export
updateSelectInput <- function(session, inputId, label = NULL, value = NULL,
    options = NULL) {
  message <- list(label=label, value=value, options=options)
  # Drop nulls
  message <- message[!vapply(message, is.null, FUN.VALUE=logical(1))]

  sendInputMessage(session, inputId, message)
}

#' @export
updateRadioInput <- updateSelectInput

#' @export
updateCheckboxGroupInput <- function(session, inputId, label, choices = NULL,
    selected = NULL) {

  choices <- choicesWithNames(choices)
  options <- list()

  for (i in seq_along(choices)) {
    choiceName <- names(choices)[i]

    opt <- list(value = choices[[i]],
                label = names(choices)[i],
                checked = choiceName %in% selected)

    options[[i]] <- opt
  }

  sendInputMessage(session, inputId,
    message = list(label = label, options = options))
}


sendInputMessage <- function(session, inputId, message) {
  session$send(
    type = "inputMessage",
    data = list(
      id = inputId,
      message = message
    )
  )
}
