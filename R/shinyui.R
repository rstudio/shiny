#' @rdname builder
#' @export
p <- function(...) tags$p(...)

#' @rdname builder
#' @export
h1 <- function(...) tags$h1(...)

#' @rdname builder
#' @export
h2 <- function(...) tags$h2(...)

#' @rdname builder
#' @export
h3 <- function(...) tags$h3(...)

#' @rdname builder
#' @export
h4 <- function(...) tags$h4(...)

#' @rdname builder
#' @export
h5 <- function(...) tags$h5(...)

#' @rdname builder
#' @export
h6 <- function(...) tags$h6(...)

#' @rdname builder
#' @export
a <- function(...) tags$a(...)

#' @rdname builder
#' @export
br <- function(...) tags$br(...)

#' @rdname builder
#' @export
div <- function(...) tags$div(...)

#' @rdname builder
#' @export
span <- function(...) tags$span(...)

#' @rdname builder
#' @export
pre <- function(...) tags$pre(...)

#' @rdname builder
#' @export
code <- function(...) tags$code(...)

#' @rdname builder
#' @export
img <- function(...) tags$img(...)

#' @rdname builder
#' @export
strong <- function(...) tags$strong(...)

#' @rdname builder
#' @export
em <- function(...) tags$em(...)

#' @rdname builder
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
#' @name include
#' @aliases includeHTML
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

#' Load the MathJax library and typeset math expressions
#'
#' This function adds MathJax to the page and typeset the math expressions (if
#' found) in the content \code{...}. It only needs to be called once in an app
#' unless the content is rendered \emph{after} the page is loaded, e.g. via
#' \code{\link{renderUI}}, in which case we have to call it explicitly every
#' time we write math expressions to the output.
#' @param ... any HTML elements to apply MathJax to
#' @export
#' @examples withMathJax(helpText("Some math here $$\\alpha+\\beta$$"))
#' # now we can just write "static" content without withMathJax()
#' div("more math here $$\\sqrt{2}$$")
withMathJax <- function(...) {
  path <- 'https://c328740.ssl.cf1.rackcdn.com/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML'
  tagList(
    tags$head(
      singleton(tags$script(HTML('window.MathJax = {skipStartupTypeset: true};'))),
      singleton(tags$script(src = path, type = 'text/javascript'))
    ),
    ...,
    tags$script(HTML('$(function() {
                        setTimeout(function() {MathJax.Hub.Typeset();}, 200);
                      });'))
  )
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

renderPage <- function(ui, connection, showcase=0) {

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
  if (showcase > 0) {
    writeLines(as.character(showcaseHead()), con = connection)
  }
  writeLines(c(result$head,
               '</head>',
               '<body>',
               recursive=TRUE),
             con = connection)

  if (showcase > 0) {
    # in showcase mode, emit containing elements and app HTML
    writeLines(as.character(showcaseBody(result$html)),
               con = connection)
  } else {
    # in normal mode, write UI html directly to connection
    writeLines(result$html, con = connection)
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

      textConn <- textConnection(NULL, "w")
      on.exit(close(textConn))

      showcaseMode <- .globals$showcaseDefault
      if (.globals$showcaseOverride) {
        mode <- showcaseModeOfReq(req)
        if (!is.null(mode))
          showcaseMode <- mode
      }
      renderPage(ui, textConn, showcaseMode)
      html <- paste(textConnectionValue(textConn), collapse='\n')
      return(httpResponse(200, content=html))
    }
  })
}

