\name{runApp}
\alias{runApp}
\title{Run Shiny Application}
\usage{
  runApp(appDir = getwd(), port = NULL,
    launch.browser = getOption("shiny.launch.browser", interactive()),
    host = getOption("shiny.host", "127.0.0.1"),
    workerId = "", quiet = FALSE)
}
\arguments{
  \item{appDir}{The directory of the application. Should
  contain \code{server.R}, plus, either \code{ui.R} or a
  \code{www} directory that contains the file
  \code{index.html}. Defaults to the working directory.}

  \item{port}{The TCP port that the application should
  listen on. Defaults to choosing a random port.}

  \item{launch.browser}{If true, the system's default web
  browser will be launched automatically after the app is
  started. Defaults to true in interactive sessions only.
  This value of this parameter can also be a function to
  call with the application's URL.}

  \item{host}{The IPv4 address that the application should
  listen on. Defaults to the \code{shiny.host} option, if
  set, or \code{"127.0.0.1"} if not. See Details.}

  \item{workerId}{Can generally be ignored. Exists to help
  some editions of Shiny Server Pro route requests to the
  correct process.}

  \item{quiet}{Should Shiny status messages be shown?
  Defaults to FALSE.}
}
\description{
  Runs a Shiny application. This function normally does not
  return; interrupt R to stop the application (usually by
  pressing Ctrl+C or Esc).
}
\details{
  The host parameter was introduced in Shiny 0.9.0. Its
  default value of \code{"127.0.0.1"} means that, contrary
  to previous versions of Shiny, only the current machine
  can access locally hosted Shiny apps. To allow other
  clients to connect, use the value \code{"0.0.0.0"}
  instead (which was the value that was hard-coded into
  Shiny in 0.8.0 and earlier).
}
\examples{
\dontrun{
# Start app in the current working directory
runApp()

# Start app in a subdirectory called myapp
runApp("myapp")


# Apps can be run without a server.r and ui.r file
runApp(list(
  ui = bootstrapPage(
    numericInput('n', 'Number of obs', 100),
    plotOutput('plot')
  ),
  server = function(input, output) {
    output$plot <- renderPlot({ hist(runif(input$n)) })
  }
))
}
}

