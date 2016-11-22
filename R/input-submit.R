#' Create a submit button
#'
#' Create a submit button for an input form. Forms that include a submit
#' button do not automatically update their outputs when inputs change,
#' rather they wait until the user explicitly clicks the submit button.
#'
#' @section Warning:
#' Submit buttons are very particular shiny objects. One the weirdest
#' things about them is they have no IDs, and there is no way to regulate
#' which inputs they should (and which ones they should not) be applied
#' to. This means they are very unwieldy for anything but the simplest
#' apps (see the example below). For example, having \emph{two} submit
#' buttons in the same app will probably not do what you'd expect because
#' submit buttons are not isolated in any way, so activating any of the
#' two buttons will cause all inputs in the app to fire. Another problem
#' with submit buttons is that dynamically created submit buttons (for
#' example, with \code{\link{renderUI}} or \code{\link{insertUI}}) will
#' not work.
#'
#' For all of these reasons, \strong{our general reccomendation is to
#' avoid submit buttons} and
#' \href{http://shiny.rstudio.com/articles/action-buttons.html#pattern-2---delay-reactions}{
#' use action buttons instead when you want to delay a reaction}. Any
#' code that uses a submit button can be converted to code that uses an
#' action button instead (while the reverse is almost never true). So,
#' if you find your app getting increasingly complicated, opt for using
#' an action button (or more since there is no problem with multiple
#' action buttons in the same app). The link above shows an example of
#' a delayed reaction using an action button. For another example, the
#' code below converts the example in this page (at the bottom) to an
#' equivalent app using an action button (using the same pattern
#' described in the link above):
#'
#' \preformatted{
#' shinyApp(
#'   ui = basicPage(
#'     numericInput("num", label = "Make changes", value = 1),
#'     actionButton("update" ,"Update View", icon("refresh"),
#'                  class = "btn btn-primary"),
#'     helpText("When you click the button above, you should see",
#'              "the output below update to reflect the value you",
#'              "entered at the top:"),
#'     verbatimTextOutput("value")
#'   ),
#'   server = function(input, output) {
#'     currentValue <- eventReactive(input$update, {
#'       input$num
#'     }, ignoreNULL = FALSE)
#'
#'     output$value <- renderPrint({ currentValue() })
#'   }
#' )
#' }
#'
#' @param text Button caption
#' @param icon Optional \code{\link{icon}} to appear on the button
#' @param width The width of the button, e.g. \code{'400px'}, or \code{'100\%'};
#'   see \code{\link{validateCssUnit}}.
#' @return A submit button that can be added to a UI definition.
#'
#' @family input elements
#'
#' @examples
#' if (interactive()) {
#'
#' shinyApp(
#'   ui = basicPage(
#'     numericInput("num", label = "Make changes", value = 1),
#'     submitButton("Update View", icon("refresh")),
#'     helpText("When you click the button above, you should see",
#'              "the output below update to reflect the value you",
#'              "entered at the top:"),
#'     verbatimTextOutput("value")
#'   ),
#'   server = function(input, output) {
#'
#'     # submit buttons do not have a value of their own,
#'     # they control when the app accesses values of other widgets.
#'     # input$num is the value of the number widget.
#'     output$value <- renderPrint({ input$num })
#'   }
#' )
#' }
#' @export
submitButton <- function(text = "Apply Changes", icon = NULL, width = NULL) {
  div(
    tags$button(
      type="submit",
      class="btn btn-primary",
      style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
      list(icon, text)
    )
  )
}
