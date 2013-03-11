
#' @export
p <- function(...) tags$p(...)

#' @export
h1 <- function(...) tags$h1(...)

#' @export
h2 <- function(...) tags$h2(...)

#' @export
h3 <- function(...) tags$h3(...)

#' @export
h4 <- function(...) tags$h4(...)

#' @export
h5 <- function(...) tags$h5(...)

#' @export
h6 <- function(...) tags$h6(...)

#' @export
a <- function(...) tags$a(...)

#' @export
br <- function(...) tags$br(...)

#' @export
div <- function(...) tags$div(...)

#' @export
span <- function(...) tags$span(...)

#' @export
pre <- function(...) tags$pre(...)

#' @export
code <- function(...) tags$code(...)

#' @export
img <- function(...) tags$img(...)

#' @export
strong <- function(...) tags$strong(...)

#' @export
em <- function(...) tags$em(...)

#' @export
includeHTML <- function(path) {
  dependsOnFile(path)
  lines <- readLines(path, warn=FALSE, encoding='UTF-8')
  return(HTML(paste(lines, collapse='\r\n')))
}

#' @export
includeText <- function(path) {
  dependsOnFile(path)
  lines <- readLines(path, warn=FALSE, encoding='UTF-8')
  return(HTML(paste(lines, collapse='\r\n')))
}

#' @export
includeMarkdown <- function(path) {
  if (!require(markdown))
    stop("Markdown package is not installed")
  
  dependsOnFile(path)
  html <- markdown::markdownToHTML(path, fragment.only=TRUE)
  Encoding(html) <- 'UTF-8'
  return(HTML(html))
}


#' Include Content Only Once
#' 
#' Use \code{singleton} to wrap contents (tag, text, HTML, or lists) that should
#' be included in the generated document only once, yet may appear in the 
#' document-generating code more than once. Only the first appearance of the 
#' content (in document order) will be used. Useful for custom components that 
#' have JavaScript files or stylesheets.
#' 
#' @param x A \code{\link{tag}}, text, \code{\link{HTML}}, or list.
#'
#' @export
singleton <- function(x) {
  class(x) <- c(class(x), 'shiny.singleton')
  return(x)
}

renderPage <- function(ui, connection) {
  
  # provide a filter so we can intercept head tag requests
  context <- new.env()
  context$head <- character()
  context$singletons <- character()
  context$filter <- function(content) {
    if (inherits(content, 'shiny.singleton')) {
      sig <- digest(content, algo='sha1')
      if (sig %in% context$singletons)
        return(FALSE)
      context$singletons <- c(sig, context$singletons)
    }
    
    if (isTag(content) && identical(content$name, "head")) {
      textConn <- textConnection(NULL, "w") 
      textConnWriter <- function(text) cat(text, file = textConn)
      tagWrite(content$children, textConnWriter, 1, context)
      context$head <- append(context$head, textConnectionValue(textConn))
      close(textConn)
      return (FALSE)
    }
    else {
      return (TRUE)
    }
  }
  
  # write ui HTML to a character vector
  textConn <- textConnection(NULL, "w") 
  tagWrite(ui, function(text) cat(text, file = textConn), 0, context)
  uiHTML <- textConnectionValue(textConn)
  close(textConn)
 
  # write preamble
  writeLines(c('<!DOCTYPE html>',
               '<html>',
               '<head>',
               '  <meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>',
               '  <script src="shared/jquery.js" type="text/javascript"></script>',
               '  <script src="shared/shiny.js" type="text/javascript"></script>',
               '  <link rel="stylesheet" type="text/css" href="shared/shiny.css"/>',
               context$head,
               '</head>',
               '<body>', 
               recursive=TRUE),
             con = connection)
  
  # write UI html to connection
  writeLines(uiHTML, con = connection)
  
  # write end document
  writeLines(c('</body>',
               '</html>'),
             con = connection)
}

#' Create a Shiny UI handler
#' 
#' Register a UI handler by providing a UI definition (created with e.g. 
#' \link{pageWithSidebar}) and web server path (typically "/", the default
#' value).
#' 
#' @param ui A user-interace definition
#' @param path The web server path to server the UI from
#' @return Called for its side-effect of registering a UI handler
#' 
#' @examples
#' el <- div(HTML("I like <u>turtles</u>"))
#' cat(as.character(el))
#'   
#' @examples
#' # Define UI
#' shinyUI(pageWithSidebar(
#'   
#'   # Application title
#'   headerPanel("Hello Shiny!"),
#'   
#'   # Sidebar with a slider input
#'   sidebarPanel(
#'     sliderInput("obs", 
#'                 "Number of observations:", 
#'                 min = 0, 
#'                 max = 1000, 
#'                 value = 500)
#'   ),
#'   
#'   # Show a plot of the generated distribution
#'   mainPanel(
#'     plotOutput("distPlot")
#'   )
#' ))
#'
#' @export
shinyUI <- function(ui, path='/') {
  
  force(ui)
  
  registerClient({
    
    function(req) {
      if (!identical(req$REQUEST_METHOD, 'GET'))
        return(NULL)

      if (req$PATH_INFO != path)
        return(NULL)
      
      textConn <- textConnection(NULL, "w") 
      on.exit(close(textConn))
      
      renderPage(ui, textConn)
      html <- paste(textConnectionValue(textConn), collapse='\n')
      return(httpResponse(200, content=html))
    }
  })
}

