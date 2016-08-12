#' @details
#'
#' The input updater functions send a message to the client, telling it to
#' change the settings of an input object. The messages are collected and sent
#' after all the observers (including outputs) have finished running.
#'
#' The syntax of these functions is similar to the functions that created the
#' inputs in the first place. For example, \code{\link{numericInput}()} and
#' \code{updateNumericInput()} take a similar set of arguments.
#'
#' Any arguments with NULL values will be ignored; they will not result in any
#' changes to the input object on the client.
#'
#' For \code{\link{radioButtons}()}, \code{\link{checkboxGroupInput}()} and
#' \code{\link{selectInput}()}, the set of choices can be cleared by using
#' \code{choices=character(0)}. Similarly, for these inputs, the selected item
#' can be cleared by using \code{selected=character(0)}.
#'
#' @param session The \code{session} object passed to function given to
#'   \code{shinyServer}.
#' @param inputId The id of the input object.
#' @param label The label to set for the input object.
