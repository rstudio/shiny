
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

# Writes the portion of the showcase header that lives inside the <HEAD> tag to
# the connection.
writeShowcaseHead <- function(connection) {
  writeLines(c('  <script src="shared/highlight/highlight.pack.js"></script>',
               '  <script src="shared/jquery-ui.js"></script>',
               '  <script src="shared/showdown/src/showdown.js"></script>',
               '  <link rel="stylesheet" type="text/css" href="shared/highlight/styles/tomorrow.css" />',
               '  <script type="text/javascript">', 
               '    $(document).ready(function() { ', 
               '      $("pre code").each(function(i, e) { hljs.highlightBlock(e) });'),
             con = connection)
  mdfile <- file.path.ci(getwd(), 'Readme.md')
  if (file.exists(mdfile)) {
    writeLines(c('      document.getElementById("readme-md").innerHTML = ',
                 '         (new Showdown.converter()).makeHtml('), 
               con = connection)
    # Read lines from the Markdown file, join them to a single string separated
    # by literal \n (for JavaScript), escape quotes, and emit to a JavaScript
    # string literal. 
    writeLines(paste('"', do.call(paste, as.list(c(gsub('"', '\"', readLines(mdfile)), sep = "\\n"))),
                     '");', sep = ""), con = connection)
  }
  writeLines(c('});', 
               '</script>'), con = connection)
}

# Writes the showcase preamble (the UI drawn above the application to be 
# showcased) to the connection.
writeShowcasePreamble <- function(connection) {
  writeLines(c('<div class="container-fluid showcase-container">', 
               '<div class="row-fluid"><div class="span8">'), 
             con = connection)
  descfile <- file.path.ci(getwd(), 'DESCRIPTION')
  if (file.exists(descfile)) {
    desc <- read.dcf(descfile)
    cols <- colnames(desc)
    if ("Title" %in% cols) {
      writeLines(paste('<h2><small>', desc[1,"Title"], 
                       sep = ""), con = connection)
      if ("Author" %in% cols) {
        writeLines('by', con = connection)
        if ("AuthorUrl" %in% cols) {
          writeLines(paste('<a href="', desc[1,"AuthorUrl"], '">', 
                           desc[1,"Author"], '</a>', sep = ''), 
                     con = connection)
        } else {
          writeLines(desc[1,"Author"], con = connection)
        }
        if ("AuthorEmail" %in% cols) {
          writeLines(paste('(<a href="mailto:', desc[1,"AuthorEmail"], '">', 
                           desc[1,"AuthorEmail"], '</a>', sep = ''), 
                     con = connection)
        }
      }
      writeLines('</small></h2>', con = connection)
    }
  } else {
    writeLines('<h2><small>Shiny Application</small></h2>', con = connection)
  }
  writeLines(c('</div><div class="span4 showcase-code-link">',
               '<button type="button" class="btn btn-default btn-lg"',
               'onclick="javascript:Shiny.popOutCode()">show code',
               '<span class="glyphicon glyphicon-new-window"></span></button>',
               '</div></div></div>'),
             con = connection)
}

# Writes the showcase application information (readme and code) to the given
# connection.
writeShowcaseAppInfo <- function(connection) {
  writeLines(c('<div class="container-fluid shiny-code-container showcase-container">',
               '<div class="row-fluid" id="readme-md"></div>',
               '<div class="row-fluid"><h3>Code</h3></div>',
               '<div class="row-fluid">', 
               '<div class="span6"><h4>ui.R</h4>',
               '<pre class="shiny-code"><code id="ui-r-code">', 
               readLines(file.path.ci(getwd(), 'ui.R')), 
               '</code></pre></div>',
               '<div class="span6"><h4>server.R</h4>',
               '<pre class="shiny-code"><code id="server-r-code">', 
               readLines(file.path.ci(getwd(), 'server.R')), 
               '</code></pre></div></div></div>'), 
             con = connection)
}
  
renderPage <- function(ui, connection, showcase=FALSE) {
  
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
               )),
              con = connection)
  if (showcase) {
    writeShowcaseHead(connection)
  }
  writeLines(c(result$head,
               '</head>',
               '<body>',
               recursive=TRUE),
             con = connection)
  if (showcase) {
    writeShowcasePreamble(connection)
  }
  
  # write UI html to connection
  writeLines(result$html, con = connection)
  
  if (showcase) {
    writeShowcaseAppInfo(connection)
  }
  
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
      
      # check to see if showcase mode was requested 
      showcase <- FALSE
      if (nchar(req$QUERY_STRING) > 0) {
        qsenv <- parseQueryString(req$QUERY_STRING)
        if (exists("showcase", where = qsenv)) {
          showcase <- as.logical(as.numeric(qsenv$showcase))
        }
      }
      
      textConn <- textConnection(NULL, "w") 
      on.exit(close(textConn))
      
      renderPage(ui, textConn, showcase)
      html <- paste(textConnectionValue(textConn), collapse='\n')
      return(httpResponse(200, content=html))
    }
  })
}

