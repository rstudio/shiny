#' @export
updateTextInput <- function(session, inputId, label = NULL, value = NULL) {
  message <- dropNulls(list(label=label, value=value))
  sendInputMessage(session, inputId, message = message)
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

  message <- dropNulls(list(label=label, value=value, min=min, max=max, step=step))
  sendInputMessage(session, inputId, message = message)
}

#' @export
updateSelectInput <- function(session, inputId, label = NULL, value = NULL,
    options = NULL) {
  message <- dropNulls(list(label=label, value=value, options=options))
  sendInputMessage(session, inputId, message = message)
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

  message <- dropNulls(list(label = label, options = options))

  sendInputMessage(session, inputId, message = message)
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
