
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
hr <- function(...) tags$hr(...)

#' Include Content From a File
#' 
#' Include HTML, text, or rendered Markdown into a \link[=shinyUI]{Shiny UI}.
#' 
#' These functions provide a convenient way to include an extensive amount of 
#' HTML, textual, Markdown, CSS, or JavaScript content, rather than using a
#' large literal R string.
#' 
#' @note \code{includeText} escapes its contents, but does no other processing. 
#'   This means that hard breaks and multiple spaces will be rendered as they 
#'   usually are in HTML: as a single space character. If you are looking for 
#'   preformatted text, wrap the call with \code{\link{pre}}, or consider using 
#'   \code{includeMarkdown} instead.
#'   
#' @note The \code{includeMarkdown} function requires the \code{markdown} 
#'   package.
#'   
#' @param path The path of the file to be included. It is highly recommended to 
#'   use a relative path (the base path being the Shiny application directory), 
#'   not an absolute path.
#'   
#' @rdname include
#' @export
includeHTML <- function(path) {
  dependsOnFile(path)
  lines <- readLines(path, warn=FALSE, encoding='UTF-8')
  return(HTML(paste(lines, collapse='\r\n')))
}

#' @rdname include
#' @export
includeText <- function(path) {
  dependsOnFile(path)
  lines <- readLines(path, warn=FALSE, encoding='UTF-8')
  return(paste(lines, collapse='\r\n'))
}

#' @rdname include
#' @export
includeMarkdown <- function(path) {
  if (!require(markdown))
    stop("Markdown package is not installed")
  
  dependsOnFile(path)
  html <- markdown::markdownToHTML(path, fragment.only=TRUE)
  Encoding(html) <- 'UTF-8'
  return(HTML(html))
}

#' @param ... Any additional attributes to be applied to the generated tag.
#' @rdname include
#' @export
includeCSS <- function(path, ...) {
  dependsOnFile(path)
  lines <- readLines(path, warn=FALSE, encoding='UTF-8')
  args <- list(...)
  if (is.null(args$type))
    args$type <- 'text/css'
  return(do.call(tags$style,
                 c(list(HTML(paste(lines, collapse='\r\n'))), args)))
}

#' @rdname include
#' @export
includeScript <- function(path, ...) {
  dependsOnFile(path)
  lines <- readLines(path, warn=FALSE, encoding='UTF-8')
  return(tags$script(HTML(paste(lines, collapse='\r\n')), ...))
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
  
  result <- renderTags(ui)

  # write preamble
  writeLines(c('<!DOCTYPE html>',
               '<html>',
               '<head>',
               '  <meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>',
               '  <script src="shared/jquery.js" type="text/javascript"></script>',
               '  <script src="shared/shiny.js" type="text/javascript"></script>',
               '  <link rel="stylesheet" type="text/css" href="shared/shiny.css"/>',
               sprintf('  <script type="application/shiny-singletons">%s</script>',
                       paste(result$singletons, collapse = ',')
               ),
               result$head,
               '</head>',
               '<body>', 
               recursive=TRUE),
             con = connection)
  
  # write UI html to connection
  writeLines(result$html, con = connection)
  
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

