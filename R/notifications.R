#' Show or remove a notification
#'
#' These functions show and remove notifications in a Shiny application.
#'
#' @param html HTML content of message.
#' @param duration Number of seconds to display the message before it
#'   disappears. Use \code{NULL} to make the message not automatically
#'   disappear.
#' @param closeButton If \code{TRUE}, display a button which will make the
#'   notification disappear when clicked. If \code{FALSE} do not display.
#' @param id An ID string. This can be used to change the contents of an
#'   existing message with \code{showNotification}, or to remove it with
#'   \code{removeNotification}. If not provided, one will be generated
#'   automatically.
#' @param session Session object to send notification to.
#'
#' @return An ID for the notification.
#'
#' @examples
#' if (interactive()) {
#' # Show a message when button is clicked
#' shinyApp(
#'   ui = fluidPage(
#'     actionButton("show", "Show")
#'   ),
#'   server = function(input, output) {
#'     observeEvent(input$show, {
#'       showNotification("Message text")
#'     })
#'   }
#' )
#'
#' # App with show and remove buttons
#' shinyApp(
#'   ui = fluidPage(
#'     actionButton("show", "Show"),
#'     actionButton("remove", "Remove")
#'   ),
#'   server = function(input, output) {
#'     # A queue of notification IDs
#'     ids <- character(0)
#'     # A counter
#'     n <- 0
#'
#'     observeEvent(input$show, {
#'       # Save the ID for removal later
#'       id <- showNotification(paste("Message", n), duration = NULL)
#'       ids <<- c(ids, id)
#'       n <<- n + 1
#'     })
#'
#'     observeEvent(input$remove, {
#'       if (length(ids) > 0)
#'         removeNotification(ids[1])
#'       ids <<- ids[-1]
#'     })
#'   }
#' )
#' }
#' @export
showNotification <- function(html, duration = 5, closeButton = TRUE,
  id = NULL, session = getDefaultReactiveDomain()) {

  if (is.null(id))
    id <- randomID()

  session$sendNotification("show",
    list(
      html = as.character(htmltools::as.tags(html)),
      duration = if (!is.null(duration)) duration * 1000,
      closeButton = closeButton,
      id = id
    )
  )

  id
}

#' @rdname showNotification
#' @export
removeNotification <- function(id = NULL, session = getDefaultReactiveDomain()) {
  if (is.null(id)) {
    stop("id is required.")
  }
  session$sendNotification("remove", id)
  id
}
