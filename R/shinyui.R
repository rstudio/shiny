#' @include globals.R
NULL

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
  path <- 'https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML'
  tagList(
    tags$head(
      singleton(tags$script(src = path, type = 'text/javascript'))
    ),
    ...,
    tags$script(HTML('MathJax.Hub.Queue(["Typeset", MathJax.Hub]);'))
  )
}

renderPage <- function(ui, connection, showcase=0) {

  if (showcase > 0)
    ui <- showcaseUI(ui)

  # Wrap ui in body tag if it doesn't already have a single top-level body tag.
  if (!(inherits(ui, "shiny.tag") && ui$name == "body"))
    ui <- tags$body(ui)

  result <- renderTags(ui)

  deps <- c(
    list(
      htmlDependency("json2", "2014.02.04", c(href="shared"), script = "json2-min.js"),
      htmlDependency("jquery", "1.11.3", c(href="shared"), script = "jquery.min.js"),
      htmlDependency("shiny", utils::packageVersion("shiny"), c(href="shared"),
        script = if (getOption("shiny.minified", TRUE)) "shiny.min.js" else "shiny.js",
        stylesheet = "shiny.css")
    ),
    result$dependencies
  )
  deps <- resolveDependencies(deps)
  deps <- lapply(deps, createWebDependency)
  depStr <- paste(sapply(deps, function(dep) {
    sprintf("%s[%s]", dep$name, dep$version)
  }), collapse = ";")
  depHtml <- renderDependencies(deps, "href")

  # write preamble
  writeUTF8(c('<!DOCTYPE html>',
               '<html>',
               '<head>',
               '  <meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>',
               sprintf('  <script type="application/shiny-singletons">%s</script>',
                       paste(result$singletons, collapse = ',')
               ),
               sprintf('  <script type="application/html-dependencies">%s</script>',
                       depStr
               ),
               depHtml
              ),
              con = connection)
  writeUTF8(c(result$head,
               '</head>',
               recursive=TRUE),
             con = connection)

  writeUTF8(result$html, con = connection)

  # write end document
  writeUTF8('</html>',
             con = connection)
}

#' Create a Shiny UI handler
#'
#' Historically this function was used in ui.R files to register a user
#' interface with Shiny. It is no longer required as of Shiny 0.10; simply
#' ensure that the last expression to be returned from ui.R is a user interface.
#' This function is kept for backwards compatibility with older applications. It
#' returns the value that is passed to it.
#'
#' @param ui A user interace definition
#' @return The user interface definition, without modifications or side effects.
#'
#' @export
shinyUI <- function(ui) {
  .globals$ui <- list(ui)
  ui
}

uiHttpHandler <- function(ui, uiPattern = "^/$") {

  force(ui)

  function(req) {
    if (!identical(req$REQUEST_METHOD, 'GET'))
      return(NULL)

    if (!isTRUE(grepl(uiPattern, req$PATH_INFO)))
      return(NULL)

    textConn <- file(open = "w+")
    on.exit(close(textConn))

    showcaseMode <- .globals$showcaseDefault
    if (.globals$showcaseOverride) {
      mode <- showcaseModeOfReq(req)
      if (!is.null(mode))
        showcaseMode <- mode
    }
    uiValue <- if (is.function(ui)) {
      if (length(formals(ui)) > 0) {
        # No corresponding ..stacktraceoff.., this is pure user code
        ..stacktraceon..(ui(req))
      } else {
        # No corresponding ..stacktraceoff.., this is pure user code
        ..stacktraceon..(ui())
      }
    } else {
      ui
    }
    if (is.null(uiValue))
      return(NULL)

    renderPage(uiValue, textConn, showcaseMode)
    html <- paste(readLines(textConn, encoding = 'UTF-8'), collapse='\n')
    return(httpResponse(200, content=enc2utf8(html)))
  }
}
