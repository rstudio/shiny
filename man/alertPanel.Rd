\name{alertPanel}
\alias{alertPanel}
\title{Create an alert panel}
\usage{
  alertPanel(...,
    type = c("warning", "error", "success", "info"),
    closeable = TRUE, block = FALSE)
}
\arguments{
  \item{...}{Contents of the alert panel. Often a simple
  text value however arbitrary formatting and UI elements
  can be included.}

  \item{type}{Type of alert (determines the background
  color of the panel). Can be "warning", "error",
  "success", or "info" (defaults to "warning").}

  \item{closeable}{Allow the user to dismiss the alert
  panel (useful for dynamic alerts).}

  \item{block}{Provide extra padding around the alert's
  content (useful when there is a lot of alert text).}
}
\description{
  Create an alert panel with warning, error, or
  informational content. Alert's use a distinct background
  color designed to gain the user's attention immediately.
}
\details{
  You can use \code{\link{renderUI}} to show an alert
  dynamically in response to specific conditions (see
  example below).
}
\examples{
\dontrun{
# Specifying a uiOutput placeholder for a dynamic alert in ui.R:
mainPanel(
  uiOutput("alert")
)

# Generating the dynamic alert in server.R:
output$alert <- renderUI(alertPanel("This might take a while..."))
}

# Change the type of alert to error (red background)
alertPanel("Invalid range specified", type = "error")

# Use a heading
alertPanel(h4("Warning"),
           "The uploaded dataset is very large and ",
           "will take a long time to process")
}

