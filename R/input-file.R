#' File Upload Control
#'
#' Create a file upload control that can be used to upload one or more files.
#'
#' Whenever a file upload completes, the corresponding input variable is set to
#' a dataframe. See the `Server value` section.
#'
#' Each time files are uploaded, they are written to a new random subdirectory
#' inside of R's process-level temporary directory. The Shiny user session keeps
#' track of all uploads in the session, and when the session ends, Shiny deletes
#' all of the subdirectories where files where uploaded to.
#'
#' @family input elements
#'
#' @inheritParams textInput
#' @param multiple Whether the user should be allowed to select and upload
#'   multiple files at once. **Does not work on older browsers, including
#'   Internet Explorer 9 and earlier.**
#' @param accept A character vector of "unique file type specifiers" which gives
#'   the browser a hint as to the type of file the server expects. Many browsers
#'   use this prevent the user from selecting an invalid file.
#'
#'   A unique file type specifier can be:
#'   * A case insensitive extension like `.csv` or `.rds`.
#'   * A valid MIME type, like `text/plain` or `application/pdf`
#'   * One of `audio/*`, `video/*`, or `image/*` meaning any audio, video,
#'   or image type, respectively.
#' @param buttonLabel The label used on the button. Can be text or an HTML tag
#'   object.
#' @param placeholder The text to show before a file has been uploaded.
#' @param capture What source to use for capturing image, audio or video data.
#'   This attribute facilitates user access to a device's media capture
#'   mechanism, such as a camera, or microphone, from within a file upload
#'   control.
#'
#'   A value of `user` indicates that the user-facing camera and/or microphone
#'   should be used. A value of `environment` specifies that the outward-facing
#'   camera and/or microphone should be used.
#'
#'   By default on most phones, this will accept still photos or video. For
#'   still photos only, also use `accept="image/*"`. For video only, use
#'   `accept="video/*"`.
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(
#'       fileInput("file1", "Choose CSV File", accept = ".csv"),
#'       checkboxInput("header", "Header", TRUE)
#'     ),
#'     mainPanel(
#'       tableOutput("contents")
#'     )
#'   )
#' )
#'
#' server <- function(input, output) {
#'   output$contents <- renderTable({
#'     file <- input$file1
#'     ext <- tools::file_ext(file$datapath)
#'
#'     req(file)
#'     validate(need(ext == "csv", "Please upload a csv file"))
#'
#'     read.csv(file$datapath, header = input$header)
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#'
#' @section Server value:
#'
#'   A `data.frame` that contains one row for each selected file, and following
#'   columns:
#' \describe{
#'   \item{`name`}{The filename provided by the web browser. This is
#'   **not** the path to read to get at the actual data that was uploaded
#'   (see
#'   `datapath` column).}
#'   \item{`size`}{The size of the uploaded data, in
#'   bytes.}
#'   \item{`type`}{The MIME type reported by the browser (for example,
#'   `text/plain`), or empty string if the browser didn't know.}
#'   \item{`datapath`}{The path to a temp file that contains the data that was
#'   uploaded. This file may be deleted if the user performs another upload
#'   operation.}
#' }
#'
#' @export
fileInput <- function(inputId, label, multiple = FALSE, accept = NULL,
  width = NULL, buttonLabel = "Browse...", placeholder = "No file selected",
  capture = NULL) {

  restoredValue <- restoreInput(id = inputId, default = NULL)

  # Catch potential edge case - ensure that it's either NULL or a data frame.
  if (!is.null(restoredValue) && !is.data.frame(restoredValue)) {
    warning("Restored value for ", inputId, " has incorrect format.")
    restoredValue <- NULL
  }

  if (!is.null(restoredValue)) {
    restoredValue <- toJSON(restoredValue, strict_atomic = FALSE)
  }

  inputTag <- tags$input(
    id = inputId,
    class = "shiny-input-file",
    name = inputId,
    type = "file",
    # Don't use "display: none;" style, which causes keyboard accessibility issue; instead use the following workaround: https://css-tricks.com/places-its-tempting-to-use-display-none-but-dont/
    style = "position: absolute !important; top: -99999px !important; left: -99999px !important;",
    `data-restore` = restoredValue
  )

  if (multiple)
    inputTag$attribs$multiple <- "multiple"
  if (length(accept) > 0)
    inputTag$attribs$accept <- paste(accept, collapse=',')

  if (!is.null(capture)) {
    inputTag$attribs$capture <- capture
  }

  div(class = "form-group shiny-input-container",
    style = css(width = validateCssUnit(width)),
    shinyInputLabel(inputId, label),

    div(class = "input-group",
      # input-group-prepend is for bootstrap 4 compat
      tags$label(class = "input-group-btn input-group-prepend",
        span(class = "btn btn-default btn-file",
          buttonLabel,
          inputTag
        )
      ),
      tags$input(type = "text", class = "form-control",
        placeholder = placeholder, readonly = "readonly"
      )
    ),

    tags$div(
      id=paste(inputId, "_progress", sep=""),
      class="progress active shiny-file-input-progress",
      tags$div(class="progress-bar")
    )
  )
}
