#' Show or remove a notification
#'
#' These functions show and remove notifications in a Shiny application.
#'
#' @param ui Content of message.
#' @param action Message content that represents an action. For example, this
#'   could be a link that the user can click on. This is separate from `ui`
#'   so customized layouts can handle the main notification content separately
#'   from action content.
#' @param duration Number of seconds to display the message before it
#'   disappears. Use `NULL` to make the message not automatically
#'   disappear.
#' @param closeButton If `TRUE`, display a button which will make the
#'   notification disappear when clicked. If `FALSE` do not display.
#' @param id A unique identifier for the notification.
#'
#'   `id` is optional for `showNotification()`: Shiny will automatically create
#'   one if needed. If you do supply it, Shiny will update an existing
#'   notification if it exists, otherwise it will create a new one.
#'
#'   `id` is required for `removeNotification()`.
#' @param type A string which controls the color of the notification. One of
#'   "default" (gray), "message" (blue), "warning" (yellow), or "error" (red).
#' @param session Session object to send notification to.
#'
#' @return An ID for the notification.
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#' # Show a message when button is clicked
#' shinyApp(
#'   ui = fluidPage(
#'     actionButton("show", "Show")
#'   ),
#'   server = function(input, output) {
#'     observeEvent(input$show, {
#'       showNotification("Message text",
#'         action = a(href = "javascript:location.reload();", "Reload page")
#'       )
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
showNotification <- function(ui, action = NULL, duration = 5,
  closeButton = TRUE, id = NULL,
  type = c("default", "message", "warning", "error"),
  session = getDefaultReactiveDomain())
{

  if (is.null(id))
    id <- createUniqueId(8)

  res <- processDeps(ui, session)
  actionRes <- processDeps(action, session)

  session$sendNotification("show",
    list(
      html = res$html,
      action = actionRes$html,
      deps = c(res$deps, actionRes$deps),
      duration = if (!is.null(duration)) duration * 1000,
      closeButton = closeButton,
      id = id,
      type = match.arg(type)
    )
  )

  id
}

#' @rdname showNotification
#' @export
removeNotification <- function(id, session = getDefaultReactiveDomain()) {
  force(id)
  session$sendNotification("remove", id)
  id
}
